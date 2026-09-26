use crate::tree::{
    Expression, Frac, Func, Group, Intermediate, Matrix, Script, ScriptFunc, Simple, SimpleBinary,
    SimpleFunc, SimpleScript, SimpleUnary,
};
use crate::{Token, Tokenizer};
use std::collections::HashSet;

/// The maximum recursion depth before deeper structure is treated as [missing][Simple::Missing].
///
/// Recursion depth is otherwise linear in the input length, so deeply-nested input like
/// `"sqrt ".repeat(100_000)` would overflow the stack and abort the process.
const MAX_DEPTH: usize = 256;

/// A token paired with the bracket-matching info precomputed for its position.
struct Entry<'a> {
    text: &'a str,
    token: Token,
    /// Matching close-bracket index for an open bracket, or `usize::MAX` if unmatched.
    close: usize,
    /// Whether this open bracket has a top-level separator (more than one column).
    has_sep: bool,
}

/// A recursive-descent parser over a materialized token slice.
///
/// The index cursor makes backtracking a `pos` assignment, and a one-pass precompute of matching
/// brackets makes matrix detection O(1), keeping the parse linear.
struct Parser<'a> {
    entries: Box<[Entry<'a>]>,
    pos: usize,
    /// Current recursion depth, bounded by [`MAX_DEPTH`].
    depth: usize,
    /// Indices of open brackets whose `|` group or matrix parse already failed.
    ///
    /// A failed attempt rewinds and is retried every time an enclosing attempt fails, which is
    /// exponential in nesting. The outcome from a position doesn't depend on the caller, except
    /// through the depth cap, so a failure is never retried.
    failed_opens: HashSet<usize>,
}

impl<'a> Parser<'a> {
    fn new(tokens: impl IntoIterator<Item = (&'a str, Token)>) -> Self {
        // collect straight into entries; unmatched open brackets keep close == usize::MAX
        let mut entries: Vec<Entry<'a>> = tokens
            .into_iter()
            .map(|(text, token)| Entry {
                text,
                token,
                close: usize::MAX,
                has_sep: false,
            })
            .collect();
        // one linear pass matches brackets and records top-level separators
        let mut open_stack: Vec<usize> = Vec::new();
        for index in 0..entries.len() {
            match entries[index].token {
                Token::OpenBracket => open_stack.push(index),
                Token::CloseBracket => {
                    if let Some(open) = open_stack.pop() {
                        entries[open].close = index;
                    }
                }
                Token::Sep => {
                    if let Some(&open) = open_stack.last() {
                        entries[open].has_sep = true;
                    }
                }
                _ => {}
            }
        }
        Parser {
            entries: entries.into(),
            pos: 0,
            depth: 0,
            failed_opens: HashSet::new(),
        }
    }

    /// Consume and return the next token, advancing the cursor.
    fn advance(&mut self) -> Option<(&'a str, Token)> {
        let item = self
            .entries
            .get(self.pos)
            .map(|entry| (entry.text, entry.token));
        if item.is_some() {
            self.pos += 1;
        }
        item
    }

    fn next_simple(&mut self, stop: Option<Token>) -> Option<Simple<'a>> {
        if self.depth >= MAX_DEPTH {
            return None;
        }
        self.depth += 1;
        let mark = self.pos;
        let result = match self.advance() {
            Some((_, token)) if Some(token) == stop => {
                self.pos = mark; // rewind
                None
            }
            Some((num, Token::Number)) => Some(Simple::Number(num)),
            Some((text, Token::Text)) => Some(Simple::Text(text)),
            Some((ident, Token::Ident)) => Some(Simple::Ident(ident)),
            Some((symb, Token::Symbol)) => Some(Simple::Symbol(symb)),
            Some((unary, Token::Unary)) => {
                Some(SimpleUnary::new(unary, self.next_simple(None).unwrap_or_default()).into())
            }
            Some((func, Token::Function)) => {
                Some(SimpleFunc::new(func, self.next_simple(None).unwrap_or_default()).into())
            }
            Some((binary, Token::Binary)) => Some(
                SimpleBinary::new(
                    binary,
                    self.next_simple(None).unwrap_or_default(),
                    self.next_simple(None).unwrap_or_default(),
                )
                .into(),
            ),
            Some((_, Token::CloseBracket)) => {
                self.pos = mark; // rewind; always stop on close bracket
                None
            }
            Some((open, Token::OpenBracket)) => Some({
                // gate the matrix parse on the precompute; done unconditionally it's exponential
                let open_index = mark;
                let matrix = (self.could_be_matrix() && !self.failed_opens.contains(&open_index))
                    .then(|| {
                        let mark = self.pos;
                        self.next_matrix(open).or_else(|| {
                            self.pos = mark; // rewind before the failed matrix attempt
                            self.failed_opens.insert(open_index);
                            None
                        })
                    });
                match matrix {
                    Some(Some(matrix)) => matrix.into(),
                    _ => self.next_open_group(open).into(),
                }
            }),
            Some((open, Token::OpenCloseBracket)) => Some(self.next_open_close_group(open)),
            Some((raw, Token::Frac | Token::Super | Token::Sub | Token::Sep)) => {
                Some(Simple::Symbol(raw))
            }
            None => None,
        };
        self.depth -= 1;
        result
    }

    /// Whether the just-consumed open bracket (at `self.pos - 1`) begins a matrix.
    ///
    /// O(1) via the precomputed tables; only returns `false` when [`next_matrix`][Self::next_matrix]
    /// would certainly fail, so results are unchanged.
    fn could_be_matrix(&self) -> bool {
        let outer_open = self.pos - 1;
        let row_open = self.pos;
        // the first row must itself open with a bracket
        if !self
            .entries
            .get(row_open)
            .is_some_and(|entry| entry.token == Token::OpenBracket)
        {
            return false;
        }
        let row_close = self.entries[row_open].close;
        if row_close >= self.entries.len() {
            return false; // the first row never closes
        }
        let after = row_close + 1;
        if self
            .entries
            .get(after)
            .is_some_and(|entry| entry.token == Token::Sep)
        {
            true // a separator implies a second row
        } else if after == self.entries[outer_open].close {
            self.entries[row_open].has_sep // single row: a matrix only with more than one column
        } else {
            false
        }
    }

    fn next_open_group(&mut self, open: &'a str) -> Group<'a> {
        let expr = self.next_expression(None);
        let mark = self.pos;
        let close = if let Some((bracket, Token::CloseBracket)) = self.advance() {
            bracket
        } else {
            // unterminated (EOF or depth-capped): rewind and close with an empty bracket
            self.pos = mark; // rewind
            ""
        };
        Group::new(open, expr, close)
    }

    fn next_open_close_group(&mut self, open: &'a str) -> Simple<'a> {
        let mark = self.pos;
        let open_index = mark - 1;
        if self.failed_opens.contains(&open_index) {
            Simple::Symbol(open)
        } else if let Some(first) = self.next_intermediate(None) {
            // take the first intermediate, even if it's another OpenCloseBracket
            let mut inters = vec![first];
            while let Some(inter) = self.next_intermediate(Some(Token::OpenCloseBracket)) {
                inters.push(inter);
            }
            if let Some((close, Token::OpenCloseBracket)) = self.advance() {
                Simple::Group(Group::new(open, inters, close))
            } else {
                // couldn't match the left-right bracket, so rewind and treat it as a symbol
                self.pos = mark; // rewind
                self.failed_opens.insert(open_index);
                Simple::Symbol(open)
            }
        } else {
            // empty so must return symbol
            Simple::Symbol(open)
        }
    }

    fn next_expression(&mut self, stop: Option<Token>) -> Expression<'a> {
        let mut inters = Vec::new();
        while let Some(inter) = self.next_intermediate(stop) {
            inters.push(inter);
        }
        inters.into()
    }

    fn next_matrix_row(
        &mut self,
        exprs: &mut impl Extend<Expression<'a>>,
    ) -> Option<(&'a str, usize, &'a str)> {
        let open = match self.advance() {
            Some((open, Token::OpenBracket)) => Some(open),
            _ => None,
        }?;
        let mut len = 1;
        exprs.extend([self.next_expression(Some(Token::Sep))]);
        loop {
            match self.advance() {
                Some((_, Token::Sep)) => {
                    exprs.extend([self.next_expression(Some(Token::Sep))]);
                    len += 1;
                }
                Some((close, Token::CloseBracket)) => return Some((open, len, close)),
                _ => return None,
            }
        }
    }

    fn next_matrix(&mut self, left: &'a str) -> Option<Matrix<'a>> {
        let mut data = Vec::new();
        let (open, num_cols, close) = self.next_matrix_row(&mut data)?;
        loop {
            match self.advance() {
                Some((_, Token::Sep)) => {
                    let (no, ncols, nc) = self.next_matrix_row(&mut data)?;
                    if no != open || ncols != num_cols || nc != close {
                        return None;
                    }
                }
                Some((right, Token::CloseBracket))
                    if data.len() > 1 && open == left && close == right =>
                {
                    return Some(Matrix::new(left, data, num_cols, right));
                }
                _ => return None,
            }
        }
    }

    fn next_script(&mut self) -> Script<'a> {
        let mark = self.pos;
        match self.advance() {
            Some((_, Token::Super)) => Script::Super(self.next_simple(None).unwrap_or_default()),
            Some((_, Token::Sub)) => {
                let sub = self.next_simple(None).unwrap_or_default();
                let mark = self.pos;
                if let Some((_, Token::Super)) = self.advance() {
                    Script::Subsuper(sub, self.next_simple(None).unwrap_or_default())
                } else {
                    self.pos = mark; // rewind
                    Script::Sub(sub)
                }
            }
            _ => {
                self.pos = mark; // rewind
                Script::None
            }
        }
    }

    fn next_script_func(&mut self, stop: Option<Token>) -> Option<ScriptFunc<'a>> {
        if self.depth >= MAX_DEPTH {
            return None;
        }
        self.depth += 1;
        let mark = self.pos;
        let result = if let Some((func, Token::Function)) = self.advance() {
            Some(
                Func::new(
                    func,
                    self.next_script(),
                    self.next_script_func(None).unwrap_or_default(),
                )
                .into(),
            )
        } else {
            self.pos = mark; // rewind
            self.next_simple(stop)
                .map(|simp| SimpleScript::new(simp, self.next_script()).into())
        };
        self.depth -= 1;
        result
    }

    fn next_intermediate(&mut self, stop: Option<Token>) -> Option<Intermediate<'a>> {
        let base = self.next_script_func(stop)?;
        let mark = self.pos;
        if let Some((_, Token::Frac)) = self.advance() {
            Some(Intermediate::Frac(Frac::new(
                base,
                self.next_script_func(None).unwrap_or_default(),
            )))
        } else {
            self.pos = mark; // rewind
            Some(Intermediate::ScriptFunc(base))
        }
    }

    fn parse(&mut self) -> Expression<'a> {
        let mut inters = Vec::new();
        let mut wraps = 0;
        loop {
            while let Some(inter) = self.next_intermediate(None) {
                inters.push(inter);
            }
            match self.advance() {
                Some((close, Token::CloseBracket)) => {
                    // cap the invisible-group nesting so the tree stays bounded; drop excess closes
                    if wraps < MAX_DEPTH {
                        let group = Simple::Group(Group::new("", inters, close));
                        inters = vec![group.into()];
                        wraps += 1;
                    }
                }
                other => {
                    // NOTE this can still hide errors if the last token is unexpected
                    debug_assert!(other.is_none(), "didn't exhaust tokens");
                    break;
                }
            }
        }
        Expression::from(inters)
    }
}

/// Parse a tokenized expression
pub fn parse_tokens<'a, T>(tokens: T) -> Expression<'a>
where
    T: IntoIterator<Item = (&'a str, Token)>,
{
    Parser::new(tokens).parse()
}

/// Parse a string returning an asciimath expression
///
/// This uses an extended set of asciimath tokens that are accessible in [`crate::ASCIIMATH_TOKENS`].
#[must_use]
pub fn parse(inp: &str) -> Expression<'_> {
    parse_tokens(Tokenizer::new(inp))
}

#[cfg(test)]
mod tests {
    use crate::tree::{
        Expression, Frac, Func, Group, Intermediate, Matrix, Simple, SimpleBinary, SimpleFunc,
        SimpleScript, SimpleUnary,
    };

    #[test]
    fn complex_precedence() {
        let expr = super::parse("sin_a^b c_d / (abs h)_i^j");
        let expected = [Frac::new(
            Func::with_subsuper(
                "sin",
                Simple::Ident("a"),
                Simple::Ident("b"),
                SimpleScript::with_sub(Simple::Ident("c"), Simple::Ident("d")),
            ),
            SimpleScript::with_subsuper(
                Group::from_iter("(", [SimpleUnary::new("abs", Simple::Ident("h"))], ")"),
                Simple::Ident("i"),
                Simple::Ident("j"),
            ),
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn missing_sub() {
        let expr = super::parse("a_");
        let expected =
            Expression::from_iter([SimpleScript::with_sub(Simple::Ident("a"), Simple::Missing)]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn missing_super() {
        let expr = super::parse("a^");
        let expected = [SimpleScript::with_super(
            Simple::Ident("a"),
            Simple::Missing,
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn missing_group_subsuper() {
        // NOTE crashes asciimath
        let expr = super::parse("(a_b^)");
        let expected = [Group::from_iter(
            "(",
            [SimpleScript::with_subsuper(
                Simple::Ident("a"),
                Simple::Ident("b"),
                Simple::Missing,
            )],
            ")",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn missing_group_unary() {
        // NOTE crashes asciimath
        let expr = super::parse("(sqrt)");
        let expected = [Group::from_iter(
            "(",
            [SimpleUnary::new("sqrt", Simple::Missing)],
            ")",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn unmatched_close() {
        let expr = super::parse(")");
        let expected = [Group::new("", Expression::default(), ")")]
            .into_iter()
            .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn simple_bracket_matching() {
        let expr = super::parse("|a|");
        let expected = [Group::from_iter("|", [Simple::Ident("a")], "|")]
            .into_iter()
            .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn eager_bracket_matching() {
        let expr = super::parse("|a|b|c|"); // "|:a:|b|:c:|"
        let expected = [
            Group::from_iter("|", [Simple::Ident("a")], "|").into(),
            Simple::Ident("b"),
            Group::from_iter("|", [Simple::Ident("c")], "|").into(),
        ]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn close_bracket_matching() {
        let expr = super::parse("(a|b)c|d"); // "(:a|b:)c|d" not "(a|:b)c:|d"
        let expected = [
            Group::from_iter(
                "(",
                [Simple::Ident("a"), Simple::Symbol("|"), Simple::Ident("b")],
                ")",
            )
            .into(),
            Simple::Ident("c"),
            Simple::Symbol("|"),
            Simple::Ident("d"),
        ]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn open_close_nonempty() {
        let expr = super::parse("| |");
        let expected = [Simple::Symbol("|"), Simple::Symbol("|")]
            .into_iter()
            .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn double_open_close() {
        let expr = super::parse("||x||");
        let expected = Expression::from_iter([Group::from_iter("||", [Simple::Ident("x")], "||")]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn simple_function() {
        let expr = super::parse("sin x");
        let expected = [Func::without_scripts("sin", Simple::Ident("x"))]
            .into_iter()
            .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn complex_function() {
        let expr = super::parse("sin_cos a cos^b c");
        let expected = [Func::with_sub(
            "sin",
            SimpleFunc::new("cos", Simple::Ident("a")),
            Func::with_super("cos", Simple::Ident("b"), Simple::Ident("c")),
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn unary_power_precidence() {
        let expr = super::parse("sin_a b^c / d");
        let expected = [Intermediate::Frac(Frac::new(
            Func::with_sub(
                "sin",
                Simple::Ident("a"),
                SimpleScript::with_super(Simple::Ident("b"), Simple::Ident("c")),
            ),
            Simple::Ident("d"),
        ))]
        .into();
        assert_eq!(expr, expected);
    }

    #[test]
    fn matrix_parsing() {
        let expr = super::parse("[[a, b], [c, d]]");
        let expected = [Matrix::new(
            "[",
            [
                [Simple::Ident("a")].into_iter().collect(),
                [Simple::Ident("b")].into_iter().collect(),
                [Simple::Ident("c")].into_iter().collect(),
                [Simple::Ident("d")].into_iter().collect(),
            ],
            2,
            "]",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn no_singleton_matrix() {
        let expr = super::parse("[[a]]");
        let expected = [Group::from_iter(
            "[",
            [Group::from_iter("[", [Simple::Ident("a")], "]")],
            "]",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn sets_as_groups() {
        // asciimath treats sets special, here we opt to make matrix parsing a little more strict
        // to avoid the possibility
        let expr = super::parse("{(x, y), (a, b)}");
        let expected = [Group::from_iter(
            "{",
            [
                Group::from_iter(
                    "(",
                    [Simple::Ident("x"), Simple::Symbol(","), Simple::Ident("y")],
                    ")",
                )
                .into(),
                Simple::Symbol(","),
                Group::from_iter(
                    "(",
                    [Simple::Ident("a"), Simple::Symbol(","), Simple::Ident("b")],
                    ")",
                )
                .into(),
            ],
            "}",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn simple_binary() {
        let expr = super::parse("root 3");
        let expected = [SimpleBinary::new(
            "root",
            Simple::Number("3"),
            Simple::Missing,
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn raw_text() {
        let expr = super::parse(r#""raw text""#);
        let expected = Expression::from_iter([Simple::Text("raw text")]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn bare_symbol() {
        let expr = super::parse("alpha");
        let expected = Expression::from_iter([Simple::Symbol("alpha")]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn open_close_multiple_intermediates() {
        // a left-right bracket group with more than one intermediate inside
        let expr = super::parse("|a b|");
        let expected = [Group::from_iter(
            "|",
            [Simple::Ident("a"), Simple::Ident("b")],
            "|",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn unclosed_groups() {
        // brackets that never close fall back to groups with an empty closing bracket
        let expr = super::parse("[[a");
        let expected = [Group::from_iter(
            "[",
            [Group::from_iter("[", [Simple::Ident("a")], "")],
            "",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn deep_nested_brackets_are_not_exponential() {
        // nested brackets used to be exponential (this would hang); it must be linear now
        let depth = 150;
        let input = format!("{}a{}", "(".repeat(depth), ")".repeat(depth));
        let expr = super::parse(&input);
        assert_eq!(expr.len(), 1);
    }

    #[test]
    fn failed_brackets_are_not_exponential() {
        // each unclosed "|" used to be retried whenever an enclosing attempt failed, doubling per
        // nesting level; nested failed matrices did the same
        let start = std::time::Instant::now();
        for unit in ["|(", "(|", "|sqrt(", "|x_(", "[[a,b],[c]],"] {
            let input = unit.repeat(200);
            let expr = super::parse(&input);
            assert!(!expr.is_empty());
        }
        let nested_matrix =
            (0..200).fold(String::from("a"), |inner, _| format!("[[{inner},a],[b]]"));
        assert!(!super::parse(&nested_matrix).is_empty());
        assert!(
            start.elapsed() < std::time::Duration::from_secs(1),
            "took {:?}",
            start.elapsed()
        );
    }

    #[test]
    fn deep_unary_chain_does_not_overflow() {
        // a deep unary chain must not overflow the stack (recurses via next_simple)
        let input = "sqrt ".repeat(100_000);
        let expr = super::parse(&input);
        assert!(!expr.is_empty());
    }

    #[test]
    fn deep_function_chain_does_not_overflow() {
        // a deep function chain must not overflow the stack (recurses via next_script_func)
        let input = "sin ".repeat(100_000);
        let expr = super::parse(&input);
        assert!(!expr.is_empty());
    }

    #[test]
    fn many_unmatched_closes_are_capped() {
        // capped to a bounded tree, so clone/compare/drop are all safe
        let input = ")".repeat(200_000);
        let expr = super::parse(&input);
        assert_eq!(expr.len(), 1);
        let cloned = expr.clone();
        assert_eq!(expr, cloned);
    }

    #[test]
    fn ragged_matrix_is_group() {
        // mismatched column counts mean the second row doesn't match, so it isn't a matrix
        let expr = super::parse("[[a, b], [c]]");
        let expected = [Group::from_iter(
            "[",
            [
                Group::from_iter(
                    "[",
                    [Simple::Ident("a"), Simple::Symbol(","), Simple::Ident("b")],
                    "]",
                )
                .into(),
                Simple::Symbol(","),
                Group::from_iter("[", [Simple::Ident("c")], "]").into(),
            ],
            "]",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn single_row_matrix() {
        // a single bracketed row with more than one column is still a matrix (total cells > 1)
        let expr = super::parse("[[a, b]]");
        let expected = [Matrix::new(
            "[",
            [
                [Simple::Ident("a")].into_iter().collect(),
                [Simple::Ident("b")].into_iter().collect(),
            ],
            2,
            "]",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn matrix_candidate_with_trailing_tokens_is_group() {
        // a row followed by a non-separator token can't be a matrix, so it stays a group
        let expr = super::parse("[[a] b]");
        let expected = [Group::from_iter(
            "[",
            [
                Group::from_iter("[", [Simple::Ident("a")], "]").into(),
                Simple::Ident("b"),
            ],
            "]",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn matrix_row_with_bar() {
        // a "|" inside a matrix row is just a symbol in that cell; the rows still parse as a matrix
        let expr = super::parse("[[a|b],[c|d]]");
        let expected = [Matrix::new(
            "[",
            [
                [Simple::Ident("a"), Simple::Symbol("|"), Simple::Ident("b")]
                    .into_iter()
                    .collect(),
                [Simple::Ident("c"), Simple::Symbol("|"), Simple::Ident("d")]
                    .into_iter()
                    .collect(),
            ],
            1,
            "]",
        )]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }
}
