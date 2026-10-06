use crate::prefix_map::{HashPrefixMap, PrefixMap};
use std::collections::VecDeque;
use std::iter::FusedIterator;
use std::sync::LazyLock;

/// A parsed token label
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    /// A token indicating a fraction that has the lowest precedence `/`
    Frac,
    /// A token indicating a superscript `^`
    Super,
    /// A token indicating a subscript `_`
    Sub,
    /// A token indicating the separation of rows and cols in matrices `,`
    Sep,
    /// A number
    Number,
    /// Quoted text
    Text,
    /// A raw identifier
    Ident,
    /// Unmatched characters with no letters or digits, like `!`
    Operator,
    /// A defined symbol token
    Symbol,
    /// A token that can be a binary operator or a prefix sign, like `-`
    ///
    /// Whether it joins the operands around it or binds the one after it is settled while
    /// parsing, giving a [`Sign`][crate::tree::Simple::Sign] or a
    /// [`Signed`][crate::tree::Simple::Signed] operand.
    Sign,
    /// A function
    Function,
    /// A unary operation
    Unary,
    /// A binary operation
    Binary,
    /// An opening bracket
    OpenBracket,
    /// A closing bracket
    CloseBracket,
    /// A bracket that can either open or close
    OpenCloseBracket,
    /// A run of whitespace between other tokens
    ///
    /// The whitespace between `text` or `mbox` and its bracket isn't yielded.
    Space,
}

macro_rules! tokens {
    ($($type:ident => $($str:expr),+;)+) => {
        [
            $(
                $(
                    ($str, Token::$type),
                )+
            )+
        ]
    };
}

/// The tokens for standard asciimath
///
/// This a a constant exported to enable easily alternate parsing, or verification of string
/// slices.
pub const ASCIIMATH_TOKENS: [(&str, Token); 388] = tokens!(
    Frac => "/";
    Super => "^";
    Sub => "_";
    Sep => ",";
    Sign => "+", "-", "+-", "pm", "-+", "mp";
    Function => "sin", "cos", "tan", "sinh", "cosh", "tanh", "cot", "sec", "csc", "arcsin",
        "arccos", "arctan", "coth", "sech", "csch", "exp", "log", "ln", "det", "gcd", "lcm", "Sin",
        "Cos", "Tan", "Arcsin", "Arccos", "Arctan", "Sinh", "Cosh", "Tanh", "Cot", "Sec", "Csc",
        "Log", "Ln", "f", "g", "arcsec", "arccsc", "arccot";
    Unary => "sqrt", "abs", "norm", "floor", "ceil", "Abs", "hat", "bar", "overline", "vec", "dot",
        "ddot", "overarc", "overparen", "ul", "underline", "ubrace", "underbrace", "obrace",
        "overbrace", "text", "mbox", "cancel", "tilde";
    // font commands
    Unary => "bb", "mathbf", "sf", "mathsf", "bbb", "mathbb", "cc", "mathcal", "tt", "mathtt",
        "fr", "mathfrak", "mathit", "italic", "bold", "bbit", "bbsf", "sfit", "bbsfit", "bbcc",
        "bbfr";
    Binary => "frac", "root", "stackrel", "overset", "underset", "color", "id", "class";
    // greek symbols
    Symbol => "alpha", "beta", "chi", "delta", "Delta", "epsi", "epsilon", "varepsilon", "eta",
        "gamma", "Gamma", "iota", "kappa", "lambda", "Lambda", "lamda", "Lamda", "mu", "nu",
        "omega", "Omega", "phi", "varphi", "Phi", "pi", "Pi", "psi", "Psi", "rho", "sigma",
        "Sigma", "tau", "theta", "vartheta", "Theta", "upsilon", "xi", "Xi", "zeta";
    // operations
    Symbol => "*", "cdot", "**", "ast", "***", "star", "//", "\\\\", "backslash", "setminus", "xx",
        "times", "|><", "ltimes", "><|", "rtimes", "|><|", "bowtie", "-:", "div", "divide", "@",
        "circ", "o+", "oplus", "ox", "otimes", "o.", "odot", "sum", "prod", "^^", "wedge", "^^^",
        "bigwedge", "vv", "vee", "vvv", "bigvee", "nn", "cap", "nnn", "bigcap", "uu", "cup", "uuu",
        "bigcup", "o-", "ominus", "dag", "dagger", "ddag", "ddagger";
    // relations
    Symbol => "=", "!=", "ne", ":=", "<", "lt", "<=", "le", "lt=", "leq", ">", "gt", "mlt", "ll",
        ">=", "ge", "gt=", "geq", "mgt", "gg", "-<", "prec", "-lt", ">-", "succ", "-<=", "preceq",
        ">-=", "succeq", "in", "!in", "notin", "sub", "subset", "sup", "supset", "sube",
        "subseteq", "supe", "supseteq", "!sub", "notsubset", "!sube", "notsubseteq", "!sup",
        "notsupset", "!supe", "notsupseteq", "-=", "equiv", "!-=", "notequiv", "~=", "cong", "~~",
        "approx", "~", "sim", "prop", "propto";
    // logical
    Symbol => "and", "or", "not", "neg", "=>", "implies", "if", "<=>", "iff", "AA", "forall", "EE",
        "exists", "_|_", "bot", "TT", "top", "|--", "vdash", "|==", "models";
    // misc
    Symbol => ":|:", "int", "oint", "del", "partial", "grad", "nabla", "O/", "emptyset", "oo",
        "infty", "aleph", "...", "ldots", ":.", "therefore", ":'",
        "because", "/_", "angle", "/_\\", "triangle", "'", "prime", "\\ ", "frown", "quad",
        "qquad", "cdots", "vdots", "ddots", "diamond", "square", "|__", "lfloor", "__|", "rfloor",
        "|~", "lceiling", "~|", "rceiling", "CC", "NN", "QQ", "RR", "ZZ", "hbar", "enspace",
        "thinspace";
    // underover
    Symbol => "lim", "Lim", "dim", "mod", "lub", "glb", "min", "max";
    // arrows
    Symbol => "uarr", "uparrow", "darr", "downarrow", "rarr", "rightarrow", "->", "to", ">->",
        "rightarrowtail", "->>", "twoheadrightarrow", ">->>", "twoheadrightarrowtail", "|->",
        "mapsto", "larr", "leftarrow", "harr", "leftrightarrow", "rArr", "Rightarrow", "lArr",
        "Leftarrow", "hArr", "Leftrightarrow", "dArr", "Downarrow", "rightleftharpoons";
    // brackets
    OpenBracket => "(", "[", "{", "|:", "(:", "<<", "langle", "left(", "left[", "{:";
    CloseBracket => ")", "]", "}", ":|", ":)", ">>", "rangle", "right)", "right]", ":}";
    OpenCloseBracket => "|", "||";
    // defined identifiers
    Ident => "dx", "dy", "dz", "dt";
);

pub type DefaultTokens = HashPrefixMap<&'static str, Token>;

static DEFAULT_TOKENS: LazyLock<DefaultTokens> =
    LazyLock::new(|| HashPrefixMap::from_iter(ASCIIMATH_TOKENS));

// TODO allow for negative sign preceeding numbers?
fn strip_number(inp: &str) -> Option<(&str, &str)> {
    let mut seen_decimal = false;
    let len = inp
        .char_indices()
        .find(|(_, c)| match c {
            '.' if !seen_decimal => {
                seen_decimal = true;
                false
            }
            '0'..='9' => false,
            _ => true,
        })
        .map_or(inp.len(), |(i, _)| i);
    if len > 1 || (!seen_decimal && len > 0) {
        Some((&inp[..len], &inp[len..]))
    } else {
        None
    }
}

// TODO Add escape behind a tokenizer option
fn strip_text(inp: &str) -> Option<(&str, &str)> {
    if inp.chars().next()? != '"' {
        return None;
    }
    let (len, _) = inp[1..].char_indices().find(|(_, c)| c == &'"')?;
    // NOTE off by 1 because we skipped the first byte
    Some((&inp[1..=len], &inp[len + 2..]))
}

/// The token for characters that didn't match any token, number, or text
fn unmatched_token(raw: &str) -> Token {
    if raw.chars().any(char::is_alphanumeric) {
        Token::Ident
    } else {
        Token::Operator
    }
}

/// Unary tokens whose bracketed argument is literal text rather than math.
const TEXT_COMMANDS: [&str; 2] = ["text", "mbox"];

/// A tokenizer where unknown characters are parsed as individual identifiers
///
/// This is the compliant mode of tokenization for for asciimath and means that unknown characters
/// are identified individually. As in asciimath, unknown characters that aren't letters or digits,
/// like `!`, are [operators][Token::Operator] instead.
///
/// As in asciimath, when a `text` or `mbox` [unary][Token::Unary] token is followed by `(`, `[`,
/// or `{`, everything up to the first matching close bracket is a single [`Token::Text`], so
/// `text(a b)` yields `text`, `(`, `a b`, `)`. Without a close bracket the text runs to the end
/// of the input.
///
/// # Example
/// ```
/// use asciimath_parser::{Tokenizer, Token};
/// let res: Vec<_> = Tokenizer::new("ab").collect();
/// assert_eq!(res, [("a", Token::Ident), ("b", Token::Ident)]);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tokenizer<'a, 'b, T> {
    remaining: &'a str,
    token_map: &'b T,
    char_ident: bool,
    /// Tokens already split off `remaining`, emitted before tokenizing it further.
    queued: VecDeque<(&'a str, Token)>,
}

impl<'a> Tokenizer<'a, 'static, DefaultTokens> {
    /// Create a new tokenizer with the default tokens.
    ///
    /// Ignoring performance differences, this achieves the same result as:
    /// ```
    /// use asciimath_parser::prefix_map::HashPrefixMap;
    /// use asciimath_parser::{ASCIIMATH_TOKENS, Tokenizer, parse_tokens};
    ///
    /// Tokenizer::with_tokens("...", &HashPrefixMap::from_iter(ASCIIMATH_TOKENS), true);
    /// ```
    #[must_use]
    pub fn new(inp: &'a str) -> Self {
        Self::with_tokens(inp, &DEFAULT_TOKENS, true)
    }
}

impl<'a, 'b, T> Tokenizer<'a, 'b, T> {
    /// Create a new tokenizer with custom tokens
    ///
    /// # Parameters
    /// - `inp`: the string to tokenize
    /// - `token_map`: a prefix map of available tokens
    /// - `char_ident`: whether to parse individual characters as identifiers (standard) or to
    ///   treat entire sequences of unmatched characters as a single identifier. Either way, a token
    ///   with no letters or digits is an [operator][Token::Operator].
    pub fn with_tokens(inp: &'a str, token_map: &'b T, char_ident: bool) -> Self {
        Tokenizer {
            remaining: inp,
            token_map,
            char_ident,
            queued: VecDeque::new(),
        }
    }

    /// Queue a bracketed literal text argument at the start of `remaining`, if there is one.
    fn queue_literal_text(&mut self) {
        let rest = self.remaining.trim_start();
        let close = match rest.chars().next() {
            Some('(') => ')',
            Some('[') => ']',
            Some('{') => '}',
            _ => return,
        };
        let (open, after_open) = rest.split_at(1);
        self.queued.push_back((open, Token::OpenBracket));
        if let Some(len) = after_open.find(close) {
            let (text, after_text) = after_open.split_at(len);
            let (close, remaining) = after_text.split_at(1);
            self.queued.push_back((text, Token::Text));
            self.queued.push_back((close, Token::CloseBracket));
            self.remaining = remaining;
        } else {
            let (text, remaining) = after_open.split_at(after_open.len());
            self.queued.push_back((text, Token::Text));
            self.remaining = remaining;
        }
    }
}

impl<'a, T> Iterator for Tokenizer<'a, '_, T>
where
    T: PrefixMap<Token>,
{
    type Item = (&'a str, Token);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(queued) = self.queued.pop_front() {
            return Some(queued);
        }
        let trimmed = self.remaining.trim_start();
        let (space, rest) = self
            .remaining
            .split_at(self.remaining.len() - trimmed.len());
        if !space.is_empty() {
            self.remaining = rest;
            Some((space, Token::Space))
        } else if let Some((len, &token)) = self.token_map.get_longest_prefix(self.remaining)
            && len > 0
        {
            let (pref, rem) = self.remaining.split_at(len);
            self.remaining = rem;
            if token == Token::Unary && TEXT_COMMANDS.contains(&pref) {
                self.queue_literal_text();
            }
            Some((pref, token))
        } else if let Some((num, res)) = strip_number(self.remaining) {
            // number
            self.remaining = res;
            Some((num, Token::Number))
        } else if let Some((text, res)) = strip_text(self.remaining) {
            // text
            self.remaining = res;
            Some((text, Token::Text))
        } else if self.char_ident {
            // next char
            self.remaining.chars().next().map(|chr| {
                let len = chr.len_utf8();
                let raw = &self.remaining[..len];
                self.remaining = &self.remaining[len..];
                (raw, unmatched_token(raw))
            })
        } else {
            // one ident per run of unmatched chars; only break where a token/number/text starts, so
            // a non-decimal '.' or unclosed '"' stays in the run rather than ending it
            let len = self
                .remaining
                .char_indices()
                .find(|&(i, c)| {
                    let rest = &self.remaining[i..];
                    c.is_whitespace()
                        || c.is_ascii_digit()
                        || (c == '.' && strip_number(rest).is_some())
                        || (c == '"' && strip_text(rest).is_some())
                        || self
                            .token_map
                            .get_longest_prefix(rest)
                            .is_some_and(|(len, _)| len > 0)
                })
                .map_or(self.remaining.len(), |(i, _)| i);
            // len == 0 only at end of input, since nothing matches at the current position
            if len == 0 {
                None
            } else {
                let raw = &self.remaining[..len];
                self.remaining = &self.remaining[len..];
                Some((raw, unmatched_token(raw)))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let queued = self.queued.len();
        (queued, Some(queued + self.remaining.len()))
    }
}

impl<T> FusedIterator for Tokenizer<'_, '_, T> where T: PrefixMap<Token> {}

#[cfg(test)]
mod tests {
    use crate::prefix_map::HashPrefixMap;
    use crate::{ASCIIMATH_TOKENS, Token, Tokenizer};

    fn not_space(&(_, token): &(&str, Token)) -> bool {
        token != Token::Space
    }

    #[test]
    fn whitespace_is_a_token() {
        let tokens: Vec<_> = Tokenizer::new(" a  +\tb ").collect();
        assert_eq!(
            *tokens,
            [
                (" ", Token::Space),
                ("a", Token::Ident),
                ("  ", Token::Space),
                ("+", Token::Sign),
                ("\t", Token::Space),
                ("b", Token::Ident),
                (" ", Token::Space),
            ]
        );
    }

    #[test]
    fn decimal_numbers() {
        let tokens: Vec<_> = Tokenizer::new("3.14 .5 1.2.3").filter(not_space).collect();
        assert_eq!(
            *tokens,
            [
                ("3.14", Token::Number),
                (".5", Token::Number),
                ("1.2", Token::Number),
                (".3", Token::Number),
            ]
        );
    }

    #[test]
    fn unknown_non_letters_are_operators() {
        let tokens: Vec<_> = Tokenizer::new("a+b!α").filter(not_space).collect();
        assert_eq!(
            *tokens,
            [
                ("a", Token::Ident),
                ("+", Token::Sign),
                ("b", Token::Ident),
                ("!", Token::Operator),
                ("α", Token::Ident),
            ]
        );

        let token_map = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);
        let runs: Vec<_> = Tokenizer::with_tokens("ab ?! c", &token_map, false)
            .filter(not_space)
            .collect();
        assert_eq!(
            *runs,
            [
                ("ab", Token::Ident),
                ("?!", Token::Operator),
                ("c", Token::Ident),
            ]
        );
    }

    #[test]
    fn unterminated_text() {
        // an opening quote with no closing quote isn't text; the quote falls through to an operator
        let tokens: Vec<_> = Tokenizer::new(r#""ab"#).filter(not_space).collect();
        assert_eq!(
            *tokens,
            [
                ("\"", Token::Operator),
                ("a", Token::Ident),
                ("b", Token::Ident)
            ]
        );
    }

    #[test]
    fn char_tokenizer() {
        let tokens: Vec<_> = Tokenizer::new(r#"frac (abs x) xy / 7^2 "text with spaces""#)
            .filter(not_space)
            .collect();
        assert_eq!(
            *tokens,
            [
                ("frac", Token::Binary),
                ("(", Token::OpenBracket),
                ("abs", Token::Unary),
                ("x", Token::Ident),
                (")", Token::CloseBracket),
                ("x", Token::Ident),
                ("y", Token::Ident),
                ("/", Token::Frac),
                ("7", Token::Number),
                ("^", Token::Super),
                ("2", Token::Number),
                ("text with spaces", Token::Text),
            ]
        );
    }

    #[test]
    fn str_tokenizer() {
        let token_map = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);
        let tokens: Vec<_> = Tokenizer::with_tokens(
            r#"frac (abs x) xy / 7^2 "text with spaces""#,
            &token_map,
            false,
        )
        .filter(not_space)
        .collect();
        assert_eq!(
            *tokens,
            [
                ("frac", Token::Binary),
                ("(", Token::OpenBracket),
                ("abs", Token::Unary),
                ("x", Token::Ident),
                (")", Token::CloseBracket),
                ("xy", Token::Ident),
                ("/", Token::Frac),
                ("7", Token::Number),
                ("^", Token::Super),
                ("2", Token::Number),
                ("text with spaces", Token::Text),
            ]
        );
    }

    #[test]
    fn str_tokenizer_absorbs_stray_dot_and_unterminated_text() {
        let token_map = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);

        let dotted: Vec<_> = Tokenizer::with_tokens("a.b + c", &token_map, false)
            .filter(not_space)
            .collect();
        assert_eq!(
            *dotted,
            [
                ("a.b", Token::Ident),
                ("+", Token::Sign),
                ("c", Token::Ident),
            ]
        );

        let unterm: Vec<_> = Tokenizer::with_tokens(r#"x "unterm"#, &token_map, false)
            .filter(not_space)
            .collect();
        assert_eq!(*unterm, [("x", Token::Ident), ("\"unterm", Token::Ident)]);
    }

    #[test]
    fn signs_have_their_own_class() {
        let tokens: Vec<_> = Tokenizer::new("+ - +- pm -+ mp -= o- -> !")
            .filter(not_space)
            .collect();
        assert_eq!(
            *tokens,
            [
                ("+", Token::Sign),
                ("-", Token::Sign),
                ("+-", Token::Sign),
                ("pm", Token::Sign),
                ("-+", Token::Sign),
                ("mp", Token::Sign),
                ("-=", Token::Symbol),
                ("o-", Token::Symbol),
                ("->", Token::Symbol),
                ("!", Token::Operator),
            ]
        );
    }

    #[test]
    fn asciimath_symbols() {
        let tokens: Vec<_> = Tokenizer::new("x ~~ y !sube o- arcsec bbsfit dArr")
            .filter(not_space)
            .collect();
        assert_eq!(
            *tokens,
            [
                ("x", Token::Ident),
                ("~~", Token::Symbol),
                ("y", Token::Ident),
                ("!sube", Token::Symbol),
                ("o-", Token::Symbol),
                ("arcsec", Token::Function),
                ("bbsfit", Token::Unary),
                ("dArr", Token::Symbol),
            ]
        );
        assert!(ASCIIMATH_TOKENS.iter().any(|&(name, _)| name == "approx"));
        assert!(!ASCIIMATH_TOKENS.iter().any(|&(name, _)| name == "aprox"));
    }

    #[test]
    fn literal_text_commands() {
        let tokens: Vec<_> = Tokenizer::new("text(hello world) mbox [a+b] x text{ (c) } d")
            .filter(not_space)
            .collect();
        assert_eq!(
            *tokens,
            [
                ("text", Token::Unary),
                ("(", Token::OpenBracket),
                ("hello world", Token::Text),
                (")", Token::CloseBracket),
                ("mbox", Token::Unary),
                ("[", Token::OpenBracket),
                ("a+b", Token::Text),
                ("]", Token::CloseBracket),
                ("x", Token::Ident),
                ("text", Token::Unary),
                ("{", Token::OpenBracket),
                (" (c) ", Token::Text),
                ("}", Token::CloseBracket),
                ("d", Token::Ident),
            ]
        );
    }

    #[test]
    fn literal_text_edge_cases() {
        let empty: Vec<_> = Tokenizer::new("text()").filter(not_space).collect();
        assert_eq!(
            *empty,
            [
                ("text", Token::Unary),
                ("(", Token::OpenBracket),
                ("", Token::Text),
                (")", Token::CloseBracket),
            ]
        );

        let unclosed: Vec<_> = Tokenizer::new("mbox(a b").filter(not_space).collect();
        assert_eq!(
            *unclosed,
            [
                ("mbox", Token::Unary),
                ("(", Token::OpenBracket),
                ("a b", Token::Text),
            ]
        );

        let unbracketed: Vec<_> = Tokenizer::new("text ab").filter(not_space).collect();
        assert_eq!(
            *unbracketed,
            [
                ("text", Token::Unary),
                ("a", Token::Ident),
                ("b", Token::Ident),
            ]
        );

        let token_map = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);
        let words: Vec<_> = Tokenizer::with_tokens("text(a b)", &token_map, false)
            .filter(not_space)
            .collect();
        assert_eq!(
            *words,
            [
                ("text", Token::Unary),
                ("(", Token::OpenBracket),
                ("a b", Token::Text),
                (")", Token::CloseBracket),
            ]
        );
    }

    #[test]
    fn perverse_tokens() {
        let token_map = HashPrefixMap::from_iter([("", Token::Symbol), (" 4", Token::Symbol)]);
        let tokens: Vec<_> = Tokenizer::with_tokens(" 4 x 4 6", &token_map, false)
            .filter(not_space)
            .collect();
        assert_eq!(
            *tokens,
            [
                ("4", Token::Number),
                ("x", Token::Ident),
                ("4", Token::Number),
                ("6", Token::Number),
            ]
        );
    }
}
