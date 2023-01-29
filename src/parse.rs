use crate::tree::{
    Expression, Frac, Func, Group, Intermediate, Matrix, Script, ScriptFunc, Simple, SimpleBinary,
    SimpleFunc, SimpleScript, SimpleUnary,
};
use crate::{Token, Tokenizer};

fn next_simple<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    stop: Option<Token>,
) -> Option<Simple<'a>> {
    let cloned = tokens.clone();
    match tokens.next() {
        Some((_, token)) if Some(token) == stop => {
            *tokens = cloned; // rewind
            None
        }
        Some((num, Token::Number)) => Some(Simple::Number(num)),
        Some((text, Token::Text)) => Some(Simple::Text(text)),
        Some((ident, Token::Ident)) => Some(Simple::Ident(ident)),
        Some((symb, Token::Symbol)) => Some(Simple::Symbol(symb)),
        Some((unary, Token::Unary)) => {
            Some(SimpleUnary::new(unary, next_simple(tokens, None).unwrap_or_default()).into())
        }
        Some((func, Token::Function)) => {
            Some(SimpleFunc::new(func, next_simple(tokens, None).unwrap_or_default()).into())
        }
        Some((binary, Token::Binary)) => Some(
            SimpleBinary::new(
                binary,
                next_simple(tokens, None).unwrap_or_default(),
                next_simple(tokens, None).unwrap_or_default(),
            )
            .into(),
        ),
        Some((_, Token::CloseBracket)) => {
            // always stop on close bracket
            *tokens = cloned; // rewind
            None
        }
        Some((open, Token::OpenBracket)) => {
            let cloned = tokens.clone();
            // first try to parse matrix
            Some(if let Some(matrix) = next_matrix(tokens, open) {
                matrix.into()
            } else {
                *tokens = cloned; // rewind before matrix
                next_open_group(tokens, open).into()
            })
        }
        Some((open, Token::OpenCloseBracket)) => Some(next_open_close_group(tokens, open)),
        Some((raw, Token::Frac | Token::Super | Token::Sub | Token::Sep)) => {
            Some(Simple::Symbol(raw))
        }
        None => None,
    }
}

fn next_open_group<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    open: &'a str,
) -> Group<'a> {
    let expr = next_expression(tokens, None);
    let close = match tokens.next() {
        Some((bracket, Token::CloseBracket)) => bracket,
        Some(_) => unreachable!("terminated on non-closing-bracket token"),
        None => "",
    };
    Group::new(open, expr, close)
}

fn next_open_close_group<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    open: &'a str,
) -> Simple<'a> {
    let cloned = tokens.clone();
    if let Some(first) = next_intermediate(tokens, None) {
        // Here we take the first token, even if it's another OpenCloseBracket
        let mut inters = vec![first];
        while let Some(inter) = next_intermediate(tokens, Some(Token::OpenCloseBracket)) {
            inters.push(inter);
        }
        match tokens.next() {
            Some((close, Token::OpenCloseBracket)) => {
                Simple::Group(Group::new(open, inters, close))
            }
            Some((_, Token::CloseBracket)) | None => {
                *tokens = cloned; // rewind
                Simple::Symbol(open)
            }
            Some(_) => unreachable!("terminated on non-bracket token"),
        }
    } else {
        // empty so must return symbol
        Simple::Symbol(open)
    }
}

fn next_expression<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    stop: Option<Token>,
) -> Expression<'a> {
    let mut inters = Vec::new();
    while let Some(inter) = next_intermediate(tokens, stop) {
        inters.push(inter);
    }
    inters.into()
}

fn next_matrix_row<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    exprs: &mut impl Extend<Expression<'a>>,
) -> Option<(&'a str, usize, &'a str)> {
    let open = match tokens.next() {
        Some((open, Token::OpenBracket)) => Some(open),
        _ => None,
    }?;
    let mut len = 1;
    exprs.extend([next_expression(tokens, Some(Token::Sep))]);
    loop {
        match tokens.next() {
            Some((_, Token::Sep)) => {
                exprs.extend([next_expression(tokens, Some(Token::Sep))]);
                len += 1;
            }
            Some((close, Token::CloseBracket)) => {
                return Some((open, len, close));
            }
            _ => return None,
        }
    }
}

fn next_matrix<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    left: &'a str,
) -> Option<Matrix<'a>> {
    let mut data = Vec::new();
    let (open, num_cols, close) = next_matrix_row(tokens, &mut data)?;
    loop {
        match tokens.next() {
            Some((_, Token::Sep)) => {
                let (no, ncols, nc) = next_matrix_row(tokens, &mut data)?;
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

fn next_script<'a>(tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone)) -> Script<'a> {
    let cloned = tokens.clone();
    match tokens.next() {
        Some((_, Token::Super)) => Script::Super(next_simple(tokens, None).unwrap_or_default()),
        Some((_, Token::Sub)) => {
            let sub = next_simple(tokens, None).unwrap_or_default();
            let cloned = tokens.clone();
            if let Some((_, Token::Super)) = tokens.next() {
                Script::Subsuper(sub, next_simple(tokens, None).unwrap_or_default())
            } else {
                *tokens = cloned; // rewind
                Script::Sub(sub)
            }
        }
        _ => {
            *tokens = cloned; // rewind
            Script::None
        }
    }
}

fn next_script_func<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    stop: Option<Token>,
) -> Option<ScriptFunc<'a>> {
    let cloned = tokens.clone();
    match tokens.next() {
        Some((func, Token::Function)) => Some(
            Func::new(
                func,
                next_script(tokens),
                next_script_func(tokens, None).unwrap_or_default(),
            )
            .into(),
        ),
        _ => {
            *tokens = cloned; // rewind
            next_simple(tokens, stop)
                .map(|simp| SimpleScript::new(simp, next_script(tokens)).into())
        }
    }
}

fn next_intermediate<'a>(
    tokens: &mut (impl Iterator<Item = (&'a str, Token)> + Clone),
    stop: Option<Token>,
) -> Option<Intermediate<'a>> {
    next_script_func(tokens, stop).map(|base| {
        let cloned = tokens.clone();
        match tokens.next() {
            Some((_, Token::Frac)) => Intermediate::Frac(Frac::new(
                base,
                next_script_func(tokens, None).unwrap_or_default(),
            )),
            _ => {
                *tokens = cloned; // rewind
                Intermediate::ScriptFunc(base)
            }
        }
    })
}

/// Parse a tokenized expression
pub fn parse_tokens<'a, T, I>(tokens: T) -> Expression<'a>
where
    I: Iterator<Item = (&'a str, Token)> + Clone,
    T: IntoIterator<IntoIter = I>,
{
    let mut tokens = tokens.into_iter();
    let mut inters = Vec::new();
    while let Some((close, Token::CloseBracket)) = {
        while let Some(inter) = next_intermediate(&mut tokens, None) {
            inters.push(inter);
        }
        tokens.next()
    } {
        // NOTE we could insert the token as an extra symbol instead of closing with an invisible
        // bracket
        let group = Simple::Group(Group::new("", inters, close));
        inters = vec![group.into()];
    }
    // NOTE this can still hide errors if the last token is unexpected
    assert!(tokens.next().is_none(), "didn't exhaust tokens");
    Expression::from(inters)
}

/// Parse a string returning an asciimath expression
///
/// This uses an extended set of asciimath tokens that are accessible in [crate::ASCIIMATH_TOKENS].
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
        let expected = Expression::from_iter([Group::from_iter(
            "|",
            [Group::from_iter("|", [Simple::Ident("x")], "|")],
            "|",
        )]);
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
}
