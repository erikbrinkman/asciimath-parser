use crate::tree::{
    Expression, Frac, Func, Group, Intermediate, Matrix, Script, ScriptFunc, Signed, Simple,
    SimpleBinary, SimpleFunc, SimpleScript, SimpleSigned, SimpleUnary, Symbol,
};
use crate::{SymbolClass, Token, Tokenizer};
use std::collections::HashSet;

/// The maximum recursion depth before deeper structure is treated as [missing][Simple::Missing].
///
/// Recursion depth is otherwise linear in the input length, so deeply-nested input like
/// `"sqrt ".repeat(100_000)` would overflow the stack and abort the process.
const MAX_DEPTH: usize = 256;

/// The bracket pairs that can surround a matrix row.
const MATRIX_ROW_BRACKETS: [(&str, &str); 2] = [("(", ")"), ("[", "]")];

/// A token paired with the bracket-matching info precomputed for its position.
struct Entry<'a> {
    text: &'a str,
    token: Token,
    /// The whitespace before this token, or empty if there was none.
    space: &'a str,
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
    /// Indices of `|` brackets, with the stop token, whose group parse already failed.
    ///
    /// A failed attempt rewinds and is retried every time an enclosing attempt fails, which is
    /// exponential in nesting. The outcome from a position depends on the caller only through the
    /// stop token and the depth cap, so a failure is never retried.
    failed_opens: HashSet<(usize, Option<Token>)>,
    /// Indices of open brackets whose matrix parse already failed, for the same reason.
    failed_matrices: HashSet<usize>,
}

impl<'a> Parser<'a> {
    fn new(tokens: impl IntoIterator<Item = (&'a str, Token)>) -> Self {
        // collect straight into entries; unmatched open brackets keep close == usize::MAX
        let mut entries: Vec<Entry<'a>> = Vec::new();
        let mut space = "";
        for (text, token) in tokens {
            if token == Token::Space {
                assert!(space.is_empty(), "two space tokens in a row");
                space = text;
            } else {
                entries.push(Entry {
                    text,
                    token,
                    space,
                    close: usize::MAX,
                    has_sep: false,
                });
                space = "";
            }
        }
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
            failed_matrices: HashSet::new(),
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

    /// Parse the next simple expression.
    ///
    /// `prefix_sign` says whether a sign here prefixes the operand after it, which
    /// [`ends_operand`] settles for the caller.
    fn next_simple(&mut self, stop: Option<Token>, prefix_sign: bool) -> Option<Simple<'a>> {
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
            Some((operator, Token::Operator)) => Some(Simple::Operator(operator)),
            Some((symb, Token::Symbol(class))) => Some(Symbol::new(symb, class).into()),
            Some((sign, Token::Sign)) => Some(if prefix_sign {
                SimpleSigned::new(sign, self.next_simple(stop, true).unwrap_or_default()).into()
            } else {
                Simple::Sign(sign)
            }),
            Some((unary, Token::Unary)) => Some(
                SimpleUnary::new(unary, self.next_simple(None, true).unwrap_or_default()).into(),
            ),
            Some((func, Token::Function)) => {
                Some(SimpleFunc::new(func, self.next_simple(None, true).unwrap_or_default()).into())
            }
            Some((binary, Token::Binary)) => Some(
                SimpleBinary::new(
                    binary,
                    self.next_simple(None, true).unwrap_or_default(),
                    self.next_simple(None, true).unwrap_or_default(),
                )
                .into(),
            ),
            Some((_, Token::CloseBracket)) => {
                self.pos = mark; // rewind; always stop on close bracket
                None
            }
            Some((open, Token::OpenBracket)) => Some(match self.try_matrix(open) {
                Some(matrix) => matrix.into(),
                None => self.next_open_group(open).into(),
            }),
            Some((open, Token::OpenCloseBracket)) => Some(self.next_open_close_group(open, stop)),
            Some((sep, Token::Sep)) => Some(Symbol::new(sep, SymbolClass::Separator).into()),
            Some((raw, Token::Frac | Token::Super | Token::Sub)) => {
                Some(Symbol::new(raw, SymbolClass::Glyph).into())
            }
            // spaces are folded into the entry after them, so never reach here
            Some((_, Token::Space)) | None => None,
        };
        self.depth -= 1;
        result
    }

    /// Parse a matrix after the just-consumed open bracket, or rewind and return `None`.
    fn try_matrix(&mut self, left: &'a str) -> Option<Matrix<'a>> {
        // gate the matrix parse on the precompute; done unconditionally it's exponential
        let open_index = self.pos - 1;
        if self.failed_matrices.contains(&open_index) || !self.could_be_matrix() {
            return None;
        }
        let mark = self.pos;
        let matrix = self.next_matrix_rows().and_then(|(open, data, num_cols)| {
            // as in asciimath, rows of "(" in "{...}" are a set of tuples
            let is_set = open == "(" && self.entries[self.pos].text == "}";
            let (cells, num_cols, column_lines) = split_column_lines(data, num_cols);
            (!is_set && cells.len() > 1 && self.closes(open_index, self.pos)).then(|| {
                let (right, _) = self.advance().expect("outer close");
                Matrix::new(left, cells, num_cols, right).with_column_lines(column_lines)
            })
        });
        if matrix.is_none() {
            self.pos = mark; // rewind before the failed matrix attempt
            self.failed_matrices.insert(open_index);
        }
        matrix
    }

    /// Whether the just-consumed open bracket (at `self.pos - 1`) begins a matrix.
    ///
    /// O(1) via the precomputed tables; only returns `false` when
    /// [`next_matrix_rows`][Self::next_matrix_rows] would certainly fail, so results are unchanged.
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
        } else if self.closes(outer_open, after) {
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

    /// Parse a left-right group, which, like its caller, won't extend past `stop`.
    ///
    /// Matrix cells stop at separators, so a `|` cell can't pair with a `|` in a later cell.
    fn next_open_close_group(&mut self, open: &'a str, stop: Option<Token>) -> Simple<'a> {
        let mark = self.pos;
        let open_index = mark - 1;
        if let Some(matrix) = self.try_matrix(open) {
            matrix.into()
        } else if self.failed_opens.contains(&(open_index, stop)) {
            Symbol::new(open, SymbolClass::Glyph).into()
        } else if let Some(first) = self.next_intermediate(stop, true) {
            // take the first intermediate, even if it's another OpenCloseBracket
            let mut inters = vec![first];
            // any other left-right bracket, e.g. "|" inside "||", opens its own group
            while !self.at_open_close(open) && self.push_intermediate(&mut inters, stop) {}
            if self.at_open_close(open)
                && let Some((close, _)) = self.advance()
            {
                Simple::Group(Group::new(open, inters, close))
            } else {
                // couldn't match the left-right bracket, so rewind and treat it as a symbol
                self.pos = mark; // rewind
                self.failed_opens.insert((open_index, stop));
                Symbol::new(open, SymbolClass::Glyph).into()
            }
        } else {
            // empty so must return symbol
            Symbol::new(open, SymbolClass::Glyph).into()
        }
    }

    /// Whether the token at `close` closes the bracket at `open`.
    fn closes(&self, open: usize, close: usize) -> bool {
        let open = &self.entries[open];
        match open.token {
            Token::OpenBracket => open.close == close,
            _ => self.entries.get(close).is_some_and(|entry| {
                entry.token == Token::OpenCloseBracket && entry.text == open.text
            }),
        }
    }

    /// Whether the next token is the left-right bracket `bracket`.
    fn at_open_close(&self, bracket: &str) -> bool {
        self.entries
            .get(self.pos)
            .is_some_and(|entry| entry.token == Token::OpenCloseBracket && entry.text == bracket)
    }

    /// Parse one intermediate onto `inters`, keeping the whitespace before it unless it's first.
    fn push_intermediate(
        &mut self,
        inters: &mut Vec<Intermediate<'a>>,
        stop: Option<Token>,
    ) -> bool {
        let space = self.entries.get(self.pos).map_or("", |entry| entry.space);
        let prefix_sign = !inters.last().is_some_and(ends_operand);
        if let Some(inter) = self.next_intermediate(stop, prefix_sign) {
            if !space.is_empty() && !inters.is_empty() {
                inters.push(Intermediate::Space(space));
            }
            inters.push(inter);
            true
        } else {
            false
        }
    }

    /// Parse intermediates onto `inters` until none is left, keeping the whitespace between them.
    fn extend_intermediates(&mut self, inters: &mut Vec<Intermediate<'a>>, stop: Option<Token>) {
        while self.push_intermediate(inters, stop) {}
    }

    fn next_expression(&mut self, stop: Option<Token>) -> Expression<'a> {
        let mut inters = Vec::new();
        self.extend_intermediates(&mut inters, stop);
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

    /// Parse comma-separated matrix rows, stopping before the outer close bracket.
    ///
    /// As in asciimath, every row is bracketed by the same `(` `)` or `[` `]` pair and has the same
    /// number of columns. Returns the row open bracket, the cells, and the number of columns.
    fn next_matrix_rows(&mut self) -> Option<(&'a str, Vec<Expression<'a>>, usize)> {
        let mut data = Vec::new();
        let (open, num_cols, close) = self.next_matrix_row(&mut data)?;
        if !MATRIX_ROW_BRACKETS.contains(&(open, close)) {
            return None;
        }
        while self
            .entries
            .get(self.pos)
            .is_some_and(|entry| entry.token == Token::Sep)
        {
            self.pos += 1;
            let (row_open, row_cols, row_close) = self.next_matrix_row(&mut data)?;
            if row_open != open || row_cols != num_cols || row_close != close {
                return None;
            }
        }
        self.entries.get(self.pos)?;
        Some((open, data, num_cols))
    }

    /// The argument of a sub- or superscript.
    fn next_script_arg(&mut self) -> Simple<'a> {
        self.next_simple(None, true).unwrap_or_default()
    }

    fn next_script(&mut self) -> Script<'a> {
        let mark = self.pos;
        match self.advance() {
            Some((_, Token::Super)) => Script::Super(self.next_script_arg()),
            Some((_, Token::Sub)) => {
                let sub = self.next_script_arg();
                let mark = self.pos;
                if let Some((_, Token::Super)) = self.advance() {
                    Script::Subsuper(sub, self.next_script_arg())
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

    /// Parse the next scripted part, which a function or a prefixing sign can head.
    ///
    /// `prefix_sign` says whether a sign here prefixes the operand after it, which
    /// [`ends_operand`] settles for the caller.
    fn next_script_func(
        &mut self,
        stop: Option<Token>,
        prefix_sign: bool,
    ) -> Option<ScriptFunc<'a>> {
        if self.depth >= MAX_DEPTH {
            return None;
        }
        self.depth += 1;
        let mark = self.pos;
        let result = match self.advance() {
            Some((func, Token::Function)) => Some(
                Func::new(
                    func,
                    self.next_script(),
                    self.next_script_func(None, true).unwrap_or_default(),
                )
                .into(),
            ),
            Some((sign, Token::Sign)) if prefix_sign => Some(
                Signed::new(sign, self.next_script_func(stop, true).unwrap_or_default()).into(),
            ),
            _ => {
                self.pos = mark; // rewind
                self.next_simple(stop, prefix_sign)
                    .map(|simp| SimpleScript::new(simp, self.next_script()).into())
            }
        };
        self.depth -= 1;
        result
    }

    fn next_intermediate(
        &mut self,
        stop: Option<Token>,
        prefix_sign: bool,
    ) -> Option<Intermediate<'a>> {
        let base = self.next_script_func(stop, prefix_sign)?;
        let mark = self.pos;
        if let Some((_, Token::Frac)) = self.advance() {
            let denominator = self.next_script_func(None, true).unwrap_or_default();
            Some(Intermediate::Frac(Frac::new(base, denominator)))
        } else {
            self.pos = mark; // rewind
            Some(Intermediate::ScriptFunc(base))
        }
    }

    fn parse(&mut self) -> Expression<'a> {
        let mut inters = Vec::new();
        let mut wraps = 0;
        loop {
            self.extend_intermediates(&mut inters, None);
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

/// Whether a [sign][Token::Sign] after `inter` joins the operands around it rather than
/// prefixing the one after it.
///
/// A sign prefixes what follows it exactly when what sits to its left isn't a complete operand:
/// nothing at all, because the sign starts the input, a group, or an argument; another operator or
/// sign; or a symbol that wants an operand of its own, which is anything that
/// [joins][SymbolClass::joins_operands] the operands around it, like `=`, anything that
/// [takes][SymbolClass::takes_operand] the one after it, like `sum`, a separator, and space
/// written as a symbol. Everything else to the left is a target the sign joins, which makes it
/// binary: an identifier, a number, text, a symbol that stands on its own like `alpha` or `dx`, a
/// group or matrix, and anything that already took its own argument. Scripts belong to the part
/// that carries them and don't answer for it, so the `-` of `x^2 - 1` subtracts while the one of
/// `sum_1^2 -x` prefixes.
///
/// Whatever is still waiting for an argument, like `sqrt`, a script marker, or the `/` of a
/// fraction, never reaches this: it parses the sign as the start of that argument.
fn ends_operand(inter: &Intermediate<'_>) -> bool {
    match inter {
        Intermediate::ScriptFunc(func) => script_func_ends_operand(func),
        Intermediate::Frac(frac) => script_func_ends_operand(&frac.denom),
        Intermediate::Space(_) => false,
    }
}

/// Whether a scripted part is a complete operand, for [`ends_operand`].
fn script_func_ends_operand(func: &ScriptFunc<'_>) -> bool {
    match func {
        ScriptFunc::Simple(SimpleScript { simple, .. }) => simple_ends_operand(simple),
        // a function or a prefixing sign has already taken its operand
        ScriptFunc::Func(_) | ScriptFunc::Signed(_) => true,
    }
}

/// Whether a simple expression is a complete operand, for [`ends_operand`].
fn simple_ends_operand(simple: &Simple<'_>) -> bool {
    match simple {
        Simple::Number(_)
        | Simple::Text(_)
        | Simple::Ident(_)
        | Simple::Signed(_)
        | Simple::Unary(_)
        | Simple::Func(_)
        | Simple::Binary(_)
        | Simple::Group(_)
        | Simple::Matrix(_) => true,
        Simple::Symbol(symbol) => matches!(symbol.class, SymbolClass::Glyph | SymbolClass::Name),
        Simple::Missing | Simple::Operator(_) | Simple::Sign(_) => false,
    }
}

/// Whether a matrix cell is a lone `|`, which marks a column line.
fn is_column_line(cell: &Expression<'_>) -> bool {
    matches!(
        **cell,
        [Intermediate::ScriptFunc(ScriptFunc::Simple(SimpleScript {
            simple: Simple::Symbol(Symbol { text: "|", .. }),
            script: Script::None,
        }))]
    )
}

/// Remove columns whose every cell is a lone `|`, returning the remaining cells and columns, and
/// the column boundaries where the removed columns were.
///
/// If every column is `|`, nothing is removed.
fn split_column_lines(
    cells: Vec<Expression<'_>>,
    num_cols: usize,
) -> (Vec<Expression<'_>>, usize, Vec<usize>) {
    let line_cols: Vec<bool> = (0..num_cols)
        .map(|col| cells.iter().skip(col).step_by(num_cols).all(is_column_line))
        .collect();
    let kept_cols = line_cols.iter().filter(|&&is_line| !is_line).count();
    if kept_cols == 0 || kept_cols == num_cols {
        (cells, num_cols, Vec::new())
    } else {
        let mut column_lines = Vec::new();
        let mut boundary = 0;
        for &is_line in &line_cols {
            if is_line {
                column_lines.push(boundary);
            } else {
                boundary += 1;
            }
        }
        let kept = cells
            .into_iter()
            .enumerate()
            .filter(|(index, _)| !line_cols[index % num_cols])
            .map(|(_, cell)| cell)
            .collect();
        (kept, kept_cols, column_lines)
    }
}

/// Parse a tokenized expression
///
/// # Panics
///
/// If two [`Token::Space`] tokens are next to each other.
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
    use crate::SymbolClass;
    use crate::tree::{
        Expression, Frac, Func, Group, Intermediate, Matrix, ScriptFunc, Signed, Simple,
        SimpleBinary, SimpleFunc, SimpleScript, SimpleSigned, SimpleUnary, Symbol,
    };

    /// A lone `|`, which is what a left-right bracket that couldn't pair falls back to
    fn bar<'a>() -> Simple<'a> {
        Symbol::new("|", SymbolClass::Glyph).into()
    }

    /// The separator between matrix cells and tuple elements
    fn comma<'a>() -> Simple<'a> {
        Symbol::new(",", SymbolClass::Separator).into()
    }

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
            Group::from_iter("(", [Simple::Ident("a"), bar(), Simple::Ident("b")], ")").into(),
            Simple::Ident("c"),
            bar(),
            Simple::Ident("d"),
        ]
        .into_iter()
        .collect();
        assert_eq!(expr, expected);
    }

    #[test]
    fn open_close_nonempty() {
        let expr = super::parse("| |");
        let expected = [
            Intermediate::from(bar()),
            Intermediate::Space(" "),
            bar().into(),
        ]
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
    fn open_close_closes_on_same_bracket() {
        // the "+" prefixes the "|" after it, which then can't pair, so both are their own part
        let expr = super::parse("||a| + |b||");
        let expected = Expression::from_iter([Group::from_iter(
            "||",
            [
                Intermediate::from(Simple::Ident("a")),
                bar().into(),
                Intermediate::Space(" "),
                Simple::Sign("+").into(),
                Intermediate::Space(" "),
                bar().into(),
                Simple::Ident("b").into(),
            ],
            "||",
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn open_close_mismatched_is_symbol() {
        let expr = super::parse("|a||");
        let expected = Expression::from_iter([
            bar(),
            Simple::Ident("a"),
            Simple::Symbol(Symbol::new("||", SymbolClass::Glyph)),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn signs_in_scripts() {
        let signed = |operand| SimpleSigned::new("-", operand);
        let expr = super::parse("x^-1e^-xy_-1^-2");
        let expected = Expression::from_iter([
            SimpleScript::with_super(Simple::Ident("x"), signed(Simple::Number("1"))),
            SimpleScript::with_super(Simple::Ident("e"), signed(Simple::Ident("x"))),
            SimpleScript::with_subsuper(
                Simple::Ident("y"),
                signed(Simple::Number("1")),
                signed(Simple::Number("2")),
            ),
        ]);
        assert_eq!(expr, expected);

        // a bracketed script is a group, so the sign inside it binds scripts again
        let expr = super::parse("x_(-1)");
        let expected = Expression::from_iter([SimpleScript::with_sub(
            Simple::Ident("x"),
            Group::from_iter("(", [Signed::new("-", Simple::Number("1"))], ")"),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    #[should_panic(expected = "two space tokens in a row")]
    fn neighboring_space_tokens_panic() {
        let _ = super::parse_tokens([(" ", crate::Token::Space), (" ", crate::Token::Space)]);
    }

    #[test]
    fn spaces_between_intermediates_are_kept() {
        let expr = super::parse(" a  + b ");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("a")),
            Intermediate::Space("  "),
            Simple::Sign("+").into(),
            Intermediate::Space(" "),
            Simple::Ident("b").into(),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn spaces_inside_an_intermediate_are_dropped() {
        let expr = super::parse("a / b sin x ^ 2");
        let expected = Expression::from_iter([
            Intermediate::from(Frac::new(Simple::Ident("a"), Simple::Ident("b"))),
            Intermediate::Space(" "),
            Func::without_scripts(
                "sin",
                SimpleScript::with_super(Simple::Ident("x"), Simple::Number("2")),
            )
            .into(),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn spaces_are_kept_inside_groups_and_matrix_cells() {
        let expr = super::parse("( a b )");
        let expected = Expression::from_iter([Group::from_iter(
            "(",
            [
                Intermediate::from(Simple::Ident("a")),
                Intermediate::Space(" "),
                Simple::Ident("b").into(),
            ],
            ")",
        )]);
        assert_eq!(expr, expected);

        let expr = super::parse("[[a b, c], [d, e]]");
        let cell = |ident| Expression::from_iter([Simple::Ident(ident)]);
        let expected = Expression::from_iter([Matrix::new(
            "[",
            [
                Expression::from_iter([
                    Intermediate::from(Simple::Ident("a")),
                    Intermediate::Space(" "),
                    Simple::Ident("b").into(),
                ]),
                cell("c"),
                cell("d"),
                cell("e"),
            ],
            2,
            "]",
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_in_function_script() {
        let expr = super::parse("sin^-1 x");
        let expected = Expression::from_iter([Func::with_super(
            "sin",
            SimpleSigned::new("-", Simple::Number("1")),
            Simple::Ident("x"),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_in_denominator() {
        let expr = super::parse("1/-2");
        let expected = Expression::from_iter([Frac::new(
            Simple::Number("1"),
            Signed::new("-", Simple::Number("2")),
        )]);
        assert_eq!(expr, expected);

        // the sign takes the scripts of what it prefixes with it
        let expr = super::parse("1/-2^3");
        let expected = Expression::from_iter([Frac::new(
            Simple::Number("1"),
            Signed::new(
                "-",
                SimpleScript::with_super(Simple::Number("2"), Simple::Number("3")),
            ),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_with_nothing_to_bind() {
        let expr = super::parse("(x^-)");
        let expected = Expression::from_iter([Group::from_iter(
            "(",
            [SimpleScript::with_super(
                Simple::Ident("x"),
                SimpleSigned::new("-", Simple::Missing),
            )],
            ")",
        )]);
        assert_eq!(expr, expected);

        let expr = super::parse("(-)");
        let expected = Expression::from_iter([Group::from_iter(
            "(",
            [Signed::new("-", Simple::Missing)],
            ")",
        )]);
        assert_eq!(expr, expected);

        // nothing to the left either, so it still prefixes a missing operand
        let expr = super::parse("-");
        let expected = Expression::from_iter([Signed::new("-", Simple::Missing)]);
        assert_eq!(expr, expected);

        // a target to the left makes it binary, even with nothing after it
        let expr = super::parse("x -");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("x")),
            Intermediate::Space(" "),
            Simple::Sign("-").into(),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn binary_sign_joins_its_operands() {
        let expr = super::parse("a-1");
        let expected =
            Expression::from_iter([Simple::Ident("a"), Simple::Sign("-"), Simple::Number("1")]);
        assert_eq!(expr, expected);

        // scripts make a target too
        let expr = super::parse("x^2-1");
        let expected = Expression::from_iter([
            Intermediate::from(SimpleScript::with_super(
                Simple::Ident("x"),
                Simple::Number("2"),
            )),
            Simple::Sign("-").into(),
            Simple::Number("1").into(),
        ]);
        assert_eq!(expr, expected);

        // so does a group, a matrix, or a left-right group that just closed
        for input in ["(a)-b", "[[a, b], [c, d]]-e", "|a|-b"] {
            let expr = super::parse(input);
            assert!(
                format!("{expr:?}").contains("Sign(\"-\")"),
                "{input}: {expr:?}"
            );
        }
    }

    #[test]
    fn sign_starts_an_expression() {
        let expr = super::parse("-x");
        let expected = Expression::from_iter([Signed::new("-", Simple::Ident("x"))]);
        assert_eq!(expr, expected);

        let expr = super::parse("+x");
        let expected = Expression::from_iter([Signed::new("+", Simple::Ident("x"))]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_binds_the_scripts_of_what_it_prefixes() {
        let expr = super::parse("-x^2");
        let expected = Expression::from_iter([Signed::new(
            "-",
            SimpleScript::with_super(Simple::Ident("x"), Simple::Number("2")),
        )]);
        assert_eq!(expr, expected);

        // and is the numerator of a fraction that follows
        let expr = super::parse("-x/y");
        let expected = Expression::from_iter([Frac::new(
            Signed::new("-", Simple::Ident("x")),
            Simple::Ident("y"),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_after_an_opening_bracket() {
        for (input, left, right) in [
            ("(-x)", "(", ")"),
            ("[-x]", "[", "]"),
            ("{-x}", "{", "}"),
            ("{:-x:}", "{:", ":}"),
            ("|-x|", "|", "|"),
            ("|__ -x __|", "|__", "__|"),
            ("|~ -x ~|", "|~", "~|"),
        ] {
            let expected = Expression::from_iter([Group::from_iter(
                left,
                [Signed::new("-", Simple::Ident("x"))],
                right,
            )]);
            assert_eq!(super::parse(input), expected, "{input}");
        }
    }

    #[test]
    fn sign_after_a_separator() {
        let expr = super::parse("a, -b");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("a")),
            comma().into(),
            Intermediate::Space(" "),
            Signed::new("-", Simple::Ident("b")).into(),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_in_a_matrix_cell() {
        let expr = super::parse("[(-a, b), (c, -d)]");
        let signed = |ident| Expression::from_iter([Signed::new("-", Simple::Ident(ident))]);
        let expected = Expression::from_iter([Matrix::new(
            "[",
            [
                signed("a"),
                Expression::from_iter([Simple::Ident("b")]),
                Expression::from_iter([Simple::Ident("c")]),
                signed("d"),
            ],
            2,
            "]",
        )]);
        assert_eq!(expr, expected);

        // a cell holding nothing but a sign doesn't swallow the separator after it
        let expr = super::parse("[(a, -, b), (c, d, e)]");
        let expected = Expression::from_iter([Matrix::new(
            "[",
            [
                Expression::from_iter([Simple::Ident("a")]),
                Expression::from_iter([Signed::new("-", Simple::Missing)]),
                Expression::from_iter([Simple::Ident("b")]),
                Expression::from_iter([Simple::Ident("c")]),
                Expression::from_iter([Simple::Ident("d")]),
                Expression::from_iter([Simple::Ident("e")]),
            ],
            3,
            "]",
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_after_an_operator_or_another_sign() {
        let expr = super::parse("a ? -b");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("a")),
            Intermediate::Space(" "),
            Simple::Operator("?").into(),
            Intermediate::Space(" "),
            Signed::new("-", Simple::Ident("b")).into(),
        ]);
        assert_eq!(expr, expected);

        let expr = super::parse("- - x");
        let expected =
            Expression::from_iter([Signed::new("-", Signed::new("-", Simple::Ident("x")))]);
        assert_eq!(expr, expected);

        let expr = super::parse("1 - - x");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Number("1")),
            Intermediate::Space(" "),
            Simple::Sign("-").into(),
            Intermediate::Space(" "),
            Signed::new("-", Simple::Ident("x")).into(),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_after_a_function_name() {
        let expr = super::parse("sin -x");
        let expected = Expression::from_iter([Func::without_scripts(
            "sin",
            Signed::new("-", Simple::Ident("x")),
        )]);
        assert_eq!(expr, expected);

        // the function still needs an argument after its scripts
        let expr = super::parse("sin^2 -x");
        let expected = Expression::from_iter([Func::with_super(
            "sin",
            Simple::Number("2"),
            Signed::new("-", Simple::Ident("x")),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_after_a_symbol_that_wants_an_operand_is_prefix() {
        let expr = super::parse("x = -y");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("x")),
            Intermediate::Space(" "),
            Symbol::new("=", SymbolClass::Joining).into(),
            Intermediate::Space(" "),
            Signed::new("-", Simple::Ident("y")).into(),
        ]);
        assert_eq!(expr, expected);

        let expr = super::parse("sum -x");
        let expected = Expression::from_iter([
            Intermediate::from(Symbol::new("sum", SymbolClass::Leading)),
            Intermediate::Space(" "),
            Signed::new("-", Simple::Ident("x")).into(),
        ]);
        assert_eq!(expr, expected);

        // a big operator keeps its scripts, and the sign after them still prefixes
        let expr = super::parse("sum_1^2 -x");
        let expected = Expression::from_iter([
            Intermediate::from(SimpleScript::with_subsuper(
                Symbol::new("sum", SymbolClass::Leading),
                Simple::Number("1"),
                Simple::Number("2"),
            )),
            Intermediate::Space(" "),
            Signed::new("-", Simple::Ident("x")).into(),
        ]);
        assert_eq!(expr, expected);

        // a joining operator written as letters, a space written as a symbol, and a separator
        // are all the same to the rule
        for input in [
            "a mod -b",
            "a quad -b",
            "a, -b",
            "x -> -y",
            "not -x",
            "a lim -b",
            "a // -b",
            "a \\\\ -b",
        ] {
            let expr = super::parse(input);
            assert!(format!("{expr:?}").contains("Signed"), "{input}: {expr:?}");
        }
    }

    #[test]
    fn sign_after_a_symbol_that_stands_alone_is_binary() {
        // a symbol that wants no operand of its own is a target like an identifier, and so is a
        // closed group, which is what a floor or ceiling mark completes
        for input in [
            "alpha - 1",
            "dx - 1",
            "oo - 1",
            "x' - 1",
            "n! - 1",
            "50% - 1",
            "|__ x __| - 1",
            "|~ x ~| - 1",
        ] {
            let expr = super::parse(input);
            assert!(
                format!("{expr:?}").contains("Sign(\"-\")"),
                "{input}: {expr:?}"
            );
        }
    }

    #[test]
    fn symbols_carry_their_class() {
        for (input, class) in [
            ("alpha", SymbolClass::Glyph),
            ("dx", SymbolClass::Name),
            ("=", SymbolClass::Joining),
            ("mod", SymbolClass::JoiningName),
            ("sum", SymbolClass::Leading),
            ("lim", SymbolClass::LeadingName),
            ("quad", SymbolClass::Space),
            (",", SymbolClass::Separator),
        ] {
            let expected = Expression::from_iter([Symbol::new(input, class)]);
            assert_eq!(super::parse(input), expected, "{input}");
        }
    }

    #[test]
    fn a_name_is_not_a_bare_variable() {
        let expr = super::parse("x dx");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("x")),
            Intermediate::Space(" "),
            Symbol::new("dx", SymbolClass::Name).into(),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn a_script_marker_with_nothing_to_script_is_a_plain_symbol() {
        let expr = super::parse("sqrt ^");
        let expected = Expression::from_iter([SimpleUnary::new(
            "sqrt",
            Symbol::new("^", SymbolClass::Glyph),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_spellings_from_the_symbol_table() {
        let expr = super::parse("a pm b");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("a")),
            Intermediate::Space(" "),
            Simple::Sign("pm").into(),
            Intermediate::Space(" "),
            Simple::Ident("b").into(),
        ]);
        assert_eq!(expr, expected);

        let expr = super::parse("a, pm b");
        let expected = Expression::from_iter([
            Intermediate::from(Simple::Ident("a")),
            comma().into(),
            Intermediate::Space(" "),
            Signed::new("pm", Simple::Ident("b")).into(),
        ]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn sign_prefixes_a_simple_argument() {
        let expr = super::parse("sqrt -x");
        let expected = Expression::from_iter([SimpleUnary::new(
            "sqrt",
            SimpleSigned::new("-", Simple::Ident("x")),
        )]);
        assert_eq!(expr, expected);

        // the second argument of a binary operator has nothing to its left either
        let expr = super::parse("root 3 -x");
        let expected = Expression::from_iter([SimpleBinary::new(
            "root",
            Simple::Number("3"),
            SimpleSigned::new("-", Simple::Ident("x")),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn deep_sign_chain_does_not_overflow() {
        // each sign prefixes the next, so a long run recurses (via next_script_func)
        let input = "-".repeat(100_000);
        let expr = super::parse(&input);
        assert!(!expr.is_empty());
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
        // as in asciimath, rows of "(" in "{...}" are a set of tuples
        let expr = super::parse("{(x, y), (a, b)}");
        let first = Group::from_iter(
            "(",
            [
                Intermediate::from(Simple::Ident("x")),
                comma().into(),
                Intermediate::Space(" "),
                Simple::Ident("y").into(),
            ],
            ")",
        );
        let second = Group::from_iter(
            "(",
            [
                Intermediate::from(Simple::Ident("a")),
                comma().into(),
                Intermediate::Space(" "),
                Simple::Ident("b").into(),
            ],
            ")",
        );
        let expected = Expression::from_iter([Group::from_iter(
            "{",
            [
                Intermediate::from(Simple::from(first)),
                comma().into(),
                Intermediate::Space(" "),
                Simple::from(second).into(),
            ],
            "}",
        )]);
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
    fn literal_text_command() {
        let expr = super::parse("text(hello world)");
        let expected = Expression::from_iter([SimpleUnary::new(
            "text",
            Group::from_iter("(", [Simple::Text("hello world")], ")"),
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn bare_symbol() {
        let expr = super::parse("alpha");
        let expected =
            Expression::from_iter([Simple::Symbol(Symbol::new("alpha", SymbolClass::Glyph))]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn open_close_multiple_intermediates() {
        // a left-right bracket group with more than one intermediate inside
        let expr = super::parse("|a b|");
        let expected = Expression::from_iter([Group::from_iter(
            "|",
            [
                Intermediate::from(Simple::Ident("a")),
                Intermediate::Space(" "),
                Simple::Ident("b").into(),
            ],
            "|",
        )]);
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
    fn floor_and_ceiling_marks_group() {
        for (input, left, right) in [
            ("|__ x __|", "|__", "__|"),
            ("|__x__|", "|__", "__|"),
            ("|~ x ~|", "|~", "~|"),
            ("lfloor x rfloor", "lfloor", "rfloor"),
            ("lceiling x rceiling", "lceiling", "rceiling"),
            // a mark pairs with any closing bracket, as "(" already pairs with "]"
            ("|__ x rfloor", "|__", "rfloor"),
            ("|__ x )", "|__", ")"),
            ("( x __|", "(", "__|"),
        ] {
            let expected =
                Expression::from_iter([Group::from_iter(left, [Simple::Ident("x")], right)]);
            assert_eq!(super::parse(input), expected, "{input}");
        }
    }

    #[test]
    fn floor_and_ceiling_marks_nest() {
        let expr = super::parse("|__ |~ x ~| __|");
        let ceiling = Group::from_iter("|~", [Simple::Ident("x")], "~|");
        let expected = Expression::from_iter([Group::from_iter("|__", [ceiling], "__|")]);
        assert_eq!(expr, expected);

        let expr = super::parse("(|__ x __|)");
        let floor = Group::from_iter("|__", [Simple::Ident("x")], "__|");
        let expected = Expression::from_iter([Group::from_iter("(", [floor], ")")]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn unmatched_floor_and_ceiling_marks_leave_the_other_bracket_empty() {
        let expr = super::parse("|__ x");
        let expected = Expression::from_iter([Group::from_iter("|__", [Simple::Ident("x")], "")]);
        assert_eq!(expr, expected);

        let expr = super::parse("x__|");
        let expected = Expression::from_iter([Group::from_iter("", [Simple::Ident("x")], "__|")]);
        assert_eq!(expr, expected);

        let expr = super::parse("__|");
        let expected = Expression::from_iter([Group::new("", Expression::default(), "__|")]);
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

    fn cells<'a>(idents: &[&'a str]) -> Vec<Expression<'a>> {
        idents
            .iter()
            .map(|&ident| Expression::from_iter([Simple::Ident(ident)]))
            .collect()
    }

    #[test]
    fn matrix_rows_differ_from_outer_brackets() {
        for (input, left, right) in [
            ("{:(a, b), (c, d):}", "{:", ":}"),
            ("{[a, b], [c, d]}", "{", "}"),
            ("[(a, b), (c, d)]", "[", "]"),
            ("|(a, b), (c, d)|", "|", "|"),
        ] {
            let expected =
                Expression::from_iter([Matrix::new(left, cells(&["a", "b", "c", "d"]), 2, right)]);
            assert_eq!(super::parse(input), expected, "{input}");
        }
    }

    #[test]
    fn piecewise_matrix() {
        let expr = super::parse("{(x, x>0),(y, x<0):}");
        let Some(Intermediate::ScriptFunc(ScriptFunc::Simple(SimpleScript {
            simple: Simple::Matrix(matrix),
            ..
        }))) = expr.first()
        else {
            panic!("not a matrix: {expr:?}");
        };
        assert_eq!((matrix.left_bracket, matrix.right_bracket), ("{", ":}"));
        assert_eq!(matrix.rows().len(), 2);
    }

    #[test]
    fn matrix_column_lines() {
        let line = || Expression::from_iter([bar()]);
        for (input, expected) in [
            (
                "[(a, |, b), (c, |, d)]",
                Matrix::new("[", cells(&["a", "b", "c", "d"]), 2, "]").with_column_lines([1]),
            ),
            (
                "[(|, a, b, |), (|, c, d, |)]",
                Matrix::new("[", cells(&["a", "b", "c", "d"]), 2, "]").with_column_lines([0, 2]),
            ),
            (
                "[(a, |, b, |, c), (d, |, e, |, h)]",
                Matrix::new("[", cells(&["a", "b", "c", "d", "e", "h"]), 3, "]")
                    .with_column_lines([1, 2]),
            ),
            (
                "[(a, |, |, b), (c, |, |, d)]",
                Matrix::new("[", cells(&["a", "b", "c", "d"]), 2, "]").with_column_lines([1, 1]),
            ),
            (
                // a line needs a "|" in every row, otherwise the "|" is a cell
                "[(a, |, b), (c, d, e)]",
                Matrix::new(
                    "[",
                    [cells(&["a"]), vec![line()], cells(&["b", "c", "d", "e"])].concat(),
                    3,
                    "]",
                ),
            ),
            (
                // with nothing but lines, the lines are cells
                "[(|, |), (|, |)]",
                Matrix::new("[", vec![line(); 4], 2, "]"),
            ),
        ] {
            assert_eq!(
                super::parse(input),
                Expression::from_iter([expected]),
                "{input}"
            );
        }
    }

    #[test]
    fn matrix_cell_bars_group_within_cell() {
        let expr = super::parse("[(|a|, b), (c, d)]");
        let mut cells = cells(&["a", "b", "c", "d"]);
        cells[0] = Expression::from_iter([Group::from_iter("|", [Simple::Ident("a")], "|")]);
        let expected = Expression::from_iter([Matrix::new("[", cells, 2, "]")]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn single_cell_with_column_line_is_group() {
        let expr = super::parse("[(a, |)]");
        assert!(!format!("{expr:?}").contains("Matrix"), "{expr:?}");
    }

    #[test]
    fn matrix_rows_need_parens_or_square_brackets() {
        let expr = super::parse("[{a, b}, {c, d}]");
        assert!(!format!("{expr:?}").contains("Matrix"), "{expr:?}");
    }

    #[test]
    fn unmatched_bar_matrix_is_symbol() {
        let expr = super::parse("|(a, b), (c, d)");
        assert_eq!(expr.first(), Some(&bar().into()));
    }

    #[test]
    fn ragged_matrix_is_group() {
        // mismatched column counts mean the second row doesn't match, so it isn't a matrix
        let expr = super::parse("[[a, b], [c]]");
        let first = Group::from_iter(
            "[",
            [
                Intermediate::from(Simple::Ident("a")),
                comma().into(),
                Intermediate::Space(" "),
                Simple::Ident("b").into(),
            ],
            "]",
        );
        let second = Group::from_iter("[", [Simple::Ident("c")], "]");
        let expected = Expression::from_iter([Group::from_iter(
            "[",
            [
                Intermediate::from(Simple::from(first)),
                comma().into(),
                Intermediate::Space(" "),
                Simple::from(second).into(),
            ],
            "]",
        )]);
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
        let row = Group::from_iter("[", [Simple::Ident("a")], "]");
        let expected = Expression::from_iter([Group::from_iter(
            "[",
            [
                Intermediate::from(Simple::from(row)),
                Intermediate::Space(" "),
                Simple::Ident("b").into(),
            ],
            "]",
        )]);
        assert_eq!(expr, expected);
    }

    #[test]
    fn matrix_row_with_bar() {
        // a "|" inside a matrix row is just a symbol in that cell; the rows still parse as a matrix
        let expr = super::parse("[[a|b],[c|d]]");
        let expected = [Matrix::new(
            "[",
            [
                [Simple::Ident("a"), bar(), Simple::Ident("b")]
                    .into_iter()
                    .collect(),
                [Simple::Ident("c"), bar(), Simple::Ident("d")]
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
