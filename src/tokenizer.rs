#[cfg(not(feature = "qp-trie"))]
use crate::prefix_map::{HashPrefixMap, PrefixMap};
#[cfg(feature = "qp-trie")]
use crate::prefix_map::{PrefixMap, QpTriePrefixMap};
use lazy_static::lazy_static;
use std::iter::FusedIterator;

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
    /// A defined symbol token
    Symbol,
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
pub const ASCIIMATH_TOKENS: [(&str, Token); 351] = tokens!(
    Frac => "/";
    Super => "^";
    Sub => "_";
    Sep => ",";
    Function => "sin", "cos", "tan", "sinh", "cosh", "tanh", "cot", "sec", "csc", "arcsin",
        "arccos", "arctan", "coth", "sech", "csch", "exp", "log", "ln", "det", "gcd", "lcm", "Sin",
        "Cos", "Tan", "Arcsin", "Arccos", "Arctan", "Sinh", "Cosh", "Tanh", "Cot", "Sec", "Csc",
        "Log", "Ln", "f", "g";
    Unary => "sqrt", "abs", "norm", "floor", "ceil", "Abs", "hat", "bar", "overline", "vec", "dot",
        "ddot", "overarc", "overparen", "ul", "underline", "ubrace", "underbrace", "obrace",
        "overbrace", "text", "mbox", "cancel", "tilde";
    // font commands
    Unary => "bb", "mathbf", "sf", "mathsf", "bbb", "mathbb", "cc", "mathcal", "tt", "mathtt",
        "fr", "mathfrak";
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
        "bigcup";
    // relations
    Symbol => "=", "!=", "ne", ":=", "<", "lt", "<=", "le", "lt=", "leq", ">", "gt", "mlt", "ll",
        ">=", "ge", "gt=", "geq", "mgt", "gg", "-<", "prec", "-lt", ">-", "succ", "-<=", "preceq",
        ">-=", "succeq", "in", "!in", "notin", "sub", "subset", "sup", "supset", "sube",
        "subseteq", "supe", "supseteq", "-=", "equiv", "~=", "cong", "~~", "aprox", "~", "sim",
        "prop", "propto";
    // logical
    Symbol => "and", "or", "not", "neg", "=>", "implies", "if", "<=>", "iff", "AA", "forall", "EE",
        "exists", "_|_", "bot", "TT", "top", "|--", "vdash", "|==", "models";
    // misc
    Symbol => ":|:", "int", "oint", "del", "partial", "grad", "nabla", "+-", "pm", "-+", "mp",
        "O/", "emptyset", "oo", "infty", "aleph", "...", "ldots", ":.", "therefore", ":'",
        "because", "/_", "angle", "/_\\", "triangle", "'", "prime", "\\ ", "frown", "quad",
        "qquad", "cdots", "vdots", "ddots", "diamond", "square", "|__", "lfloor", "__|", "rfloor",
        "|~", "lceiling", "~|", "rceiling", "CC", "NN", "QQ", "RR", "ZZ";
    // underover
    Symbol => "lim", "Lim", "dim", "mod", "lub", "glb", "min", "max";
    // arrows
    Symbol => "uarr", "uparrow", "darr", "downarrow", "rarr", "rightarrow", "->", "to", ">->",
        "rightarrowtail", "->>", "twoheadrightarrow", ">->>", "twoheadrightarrowtail", "|->",
        "mapsto", "larr", "leftarrow", "harr", "leftrightarrow", "rArr", "Rightarrow", "lArr",
        "Leftarrow", "hArr", "Leftrightarrow";
    // brackets
    OpenBracket => "(", "[", "{", "|:", "(:", "<<", "langle", "left(", "left[", "{:";
    CloseBracket => ")", "]", "}", ":|", ":)", ">>", "rangle", "right)", "right]", ":}";
    OpenCloseBracket => "|";
    // defined identifiers
    Ident => "dx", "dy", "dz", "dt";
);

#[cfg(feature = "qp-trie")]
pub type DefaultTokens = QpTriePrefixMap<&'static str, Token>;
#[cfg(not(feature = "qp-trie"))]
pub type DefaultTokens = HashPrefixMap<&'static str, Token>;

#[cfg(feature = "qp-trie")]
lazy_static! {
    static ref DEFAULT_TOKENS: DefaultTokens = QpTriePrefixMap::from_iter(ASCIIMATH_TOKENS);
}
#[cfg(not(feature = "qp-trie"))]
lazy_static! {
    static ref DEFAULT_TOKENS: DefaultTokens = HashPrefixMap::from_iter(ASCIIMATH_TOKENS);
}

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
        .map(|(i, _)| i)
        .unwrap_or(inp.len());
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
    };
    let (len, _) = inp[1..].char_indices().find(|(_, c)| c == &'"')?;
    // NOTE off by 1 because we skipped the first byte
    Some((&inp[1..len + 1], &inp[len + 2..]))
}

/// A tokenizer where unknown characters are parsed as individual identifiers
///
/// This is the compliant mode of tokenization for for asciimath and means that unknown characters
/// are identified individually
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
    ///   treat entire sequences of unmatched characters as a single identifier.
    pub fn with_tokens(inp: &'a str, token_map: &'b T, char_ident: bool) -> Self {
        Tokenizer {
            remaining: inp,
            token_map,
            char_ident,
        }
    }
}

impl<'a, 'b, T> Iterator for Tokenizer<'a, 'b, T>
where
    T: PrefixMap<Token>,
{
    type Item = (&'a str, Token);

    fn next(&mut self) -> Option<Self::Item> {
        // remove whitespace
        self.remaining = self.remaining.trim_start();
        if let Some((len, &token)) = self.token_map.get_longest_prefix(self.remaining) {
            if len > 0 {
                let (pref, rem) = self.remaining.split_at(len);
                self.remaining = rem;
                return Some((pref, token));
            }
        }
        // number
        if let Some((num, res)) = strip_number(self.remaining) {
            self.remaining = res;
            return Some((num, Token::Number));
        }
        // text
        if let Some((text, res)) = strip_text(self.remaining) {
            self.remaining = res;
            return Some((text, Token::Text));
        }
        // next char
        if self.char_ident {
            self.remaining.chars().next().map(|chr| {
                let len = chr.len_utf8();
                let raw = &self.remaining[..len];
                self.remaining = &self.remaining[len..];
                (raw, Token::Ident)
            })
        } else {
            let len = self
                .remaining
                .char_indices()
                .find(|&(i, c)| {
                    // NOTE using the strip would be guaranteed to be correct, but less efficient
                    matches!(c, '.' | '"' | '0'..='9')
                        || c.is_whitespace()
                        || self
                            .token_map
                            .get_longest_prefix(&self.remaining[i..])
                            .map(|(i, _)| i > 0)
                            .unwrap_or(false)
                })
                .map(|(i, _)| i)
                .unwrap_or(self.remaining.len());
            if len == 0 {
                None
            } else {
                let raw = &self.remaining[..len];
                self.remaining = &self.remaining[len..];
                Some((raw, Token::Ident))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.remaining.len()))
    }
}

impl<'a, 'b, T> FusedIterator for Tokenizer<'a, 'b, T> where T: PrefixMap<Token> {}

#[cfg(test)]
mod tests {
    use crate::prefix_map::HashPrefixMap;
    use crate::{Token, Tokenizer, ASCIIMATH_TOKENS};

    #[test]
    fn char_tokenizer() {
        let tokens: Vec<_> =
            Tokenizer::new(r#"frac (abs x) xy / 7^2 "text with spaces""#).collect();
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
    fn perverse_tokens() {
        let token_map = HashPrefixMap::from_iter([("", Token::Symbol), (" 4", Token::Symbol)]);
        let tokens: Vec<_> = Tokenizer::with_tokens(" 4 x 4 6", &token_map, false).collect();
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
