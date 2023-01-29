//! The module containing all of the structures that defined a parsed representation of asciimath
//!
//! - [`Expression`] - A full expression containing a sequence of intermediate expressions
//! - [`Intermediate`] - The most complicated single expression, this can be a high level fraction
//! - [`Frac`] - A high level fraction, often parsed with a `/`
//! - [`ScriptFunc`] - A scripted expression that can be a [`Func`] or just a simple expression
//! - [`Func`] - A function like `sin` that can contain independent super- and subscripts
//!       prior to its argument
//! - [`SimpleScript`] - A simple expression that has super- and subscripts
//! - [`Simple`] - A simple expression like a [`Symbol`][Simple::Symbol] or
//!       [`Ident`][Simple::Ident]ifier
//!
//! The exceptions to this hierarchy are [`Group`] and [`Matrix`] that "reset" the hierarchy by
//! wrapping expressions.
use std::iter::FusedIterator;
use std::ops::{Deref, Index};
use std::slice::ChunksExact;

/// A unary operator like "sqrt"
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleUnary<'a> {
    /// The operator name
    pub op: &'a str,
    /// The operator argument
    arg: Box<Simple<'a>>,
}

impl<'a> SimpleUnary<'a> {
    /// Create a unary with its op and argument
    pub fn new<S>(op: &'a str, arg: S) -> Self
    where
        S: Into<Simple<'a>>,
    {
        SimpleUnary {
            op,
            arg: Box::new(arg.into()),
        }
    }

    /// The operator argument
    pub fn arg(&self) -> &Simple<'a> {
        &self.arg
    }
}

/// A simple func like "sin"
///
/// When encountering a func keyword while parsing a simple context, it can't have attached power
/// arguments or complicated arguments, but it will still be parsed as a unary function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleFunc<'a> {
    /// The function name
    pub func: &'a str,
    /// The function argument
    arg: Box<Simple<'a>>,
}

impl<'a> SimpleFunc<'a> {
    /// Create a simple function from its name and argument
    pub fn new<S>(func: &'a str, arg: S) -> Self
    where
        S: Into<Simple<'a>>,
    {
        SimpleFunc {
            func,
            arg: Box::new(arg.into()),
        }
    }

    /// The function argument
    pub fn arg(&self) -> &Simple<'a> {
        &self.arg
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A binary operator like "root"
pub struct SimpleBinary<'a> {
    /// The operator name
    pub op: &'a str,
    /// The first operator argument
    first: Box<Simple<'a>>,
    /// The second operator argument
    second: Box<Simple<'a>>,
}

impl<'a> SimpleBinary<'a> {
    /// Create a binary operator with its name and both arguments
    pub fn new<F, S>(op: &'a str, first: F, second: S) -> Self
    where
        F: Into<Simple<'a>>,
        S: Into<Simple<'a>>,
    {
        SimpleBinary {
            op,
            first: Box::new(first.into()),
            second: Box::new(second.into()),
        }
    }

    /// The first operator argument
    pub fn first(&self) -> &Simple<'a> {
        &self.first
    }

    /// The second operator argument
    pub fn second(&self) -> &Simple<'a> {
        &self.second
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A bracketd group that allows inserting complicated expressions in simplex contexts
///
/// In some instances a bracket won't be parsed to close a group out, in that case the bracket will
/// be the empty string.
pub struct Group<'a> {
    /// The left bracket
    pub left_bracket: &'a str,
    /// The grouped expression
    pub expr: Expression<'a>,
    /// The right bracket
    pub right_bracket: &'a str,
}

impl<'a> Group<'a> {
    /// Create a new group with two brackets and an expression
    pub fn new<E: Into<Expression<'a>>>(
        left_bracket: &'a str,
        expr: E,
        right_bracket: &'a str,
    ) -> Self {
        Group {
            left_bracket,
            expr: expr.into(),
            right_bracket,
        }
    }

    /// Create a new bracket with an iterable of intermediates instead of a fully formed expression
    pub fn from_iter<T, I>(left_bracket: &'a str, inters: T, right_bracket: &'a str) -> Self
    where
        T: IntoIterator<Item = I>,
        I: Into<Intermediate<'a>>,
    {
        Group {
            left_bracket,
            expr: inters.into_iter().collect(),
            right_bracket,
        }
    }
}

/// A matrix e.g. "[[a, b], [x, y]]"
///
/// Individual expressions can be accessed with [rows][Matrix::rows] to get a random access iterator of row
/// slices, or by indexing with a 2d array of indices, e.g. `matrix[[0, 0]]`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matrix<'a> {
    /// The matrix's left bracket
    pub left_bracket: &'a str,
    /// The cells in the matrix in colum-wise order
    cells: Box<[Expression<'a>]>,
    /// The number of columns in the matrix
    num_cols: usize,
    /// The matrix's right bracket
    pub right_bracket: &'a str,
}

impl<'a> Matrix<'a> {
    /// Create a new matrix
    ///
    /// `cells` is a slice of expressiongs in row-major top-down order. `num_cols` is how many
    /// columns exist in the final matrix.
    ///
    /// # Panics
    /// When `cells.into().len()` is not divisible by `num_cols`.
    pub fn new<E>(left_bracket: &'a str, cells: E, num_cols: usize, right_bracket: &'a str) -> Self
    where
        E: Into<Box<[Expression<'a>]>>,
    {
        let cells = cells.into();
        assert_eq!(cells.len() / num_cols * num_cols, cells.len());
        Matrix {
            left_bracket,
            right_bracket,
            cells,
            num_cols,
        }
    }

    /// The number of columns
    pub fn num_cols(&self) -> usize {
        self.num_cols
    }

    /// The number of rows
    pub fn num_rows(&self) -> usize {
        self.cells.len() / self.num_cols
    }

    /// The number of total cells
    pub fn num_cells(&self) -> usize {
        self.cells.len()
    }

    /// A top-down iterator over rows as slices
    pub fn rows(&self) -> MatrixRows<'a, '_> {
        MatrixRows(self.cells.chunks_exact(self.num_cols))
    }
}

/// Matrix references iterate over rows
impl<'a, 'b> IntoIterator for &'b Matrix<'a> {
    type IntoIter = MatrixRows<'a, 'b>;
    type Item = &'b [Expression<'a>];

    fn into_iter(self) -> Self::IntoIter {
        self.rows()
    }
}

/// usize indices get rows
impl<'a> Index<usize> for Matrix<'a> {
    type Output = [Expression<'a>];

    /// Get an individual expression
    ///
    /// # Panics
    /// When index is out of bounds `idx >= num_rows`
    fn index(&self, row: usize) -> &Self::Output {
        self.rows().nth(row).expect("index out of bounds")
    }
}

/// 2D indices get individual expressions
impl<'a> Index<[usize; 2]> for Matrix<'a> {
    type Output = Expression<'a>;

    /// Get an individual expression
    ///
    /// # Panics
    /// When indices are out of bounds `[x, y]`, `x >= num_cols`, `y >= num_rows`
    fn index(&self, idx: [usize; 2]) -> &Self::Output {
        let [x, y] = idx;
        assert!(x < self.num_cols, "index out of bounds");
        &self.cells[x + self.num_cols * y]
    }
}

/// An iterator over rows of a matrix
#[derive(Debug, Clone)]
pub struct MatrixRows<'a, 'b>(ChunksExact<'b, Expression<'a>>);

impl<'a, 'b> Iterator for MatrixRows<'a, 'b> {
    type Item = &'b [Expression<'a>];

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }

    fn count(self) -> usize {
        self.len()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.0.nth(n)
    }

    fn last(mut self) -> Option<Self::Item> {
        self.next_back()
    }
}
impl<'a, 'b> FusedIterator for MatrixRows<'a, 'b> {}

impl<'a, 'b> DoubleEndedIterator for MatrixRows<'a, 'b> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.0.nth_back(n)
    }
}

impl<'a, 'b> ExactSizeIterator for MatrixRows<'a, 'b> {}

/// A simple element of parsing
///
/// This is the lowest level in the parse tree, but can be scaled back up with a group or matrix
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Simple<'a> {
    /// A missing expression
    ///
    /// This is found in places where something didn't exist, e.g. "sin" by itself will have a
    /// missing argument, but still be parsed.
    #[default]
    Missing,
    /// A raw number
    Number(&'a str),
    /// Raw text
    Text(&'a str),
    /// An identity, usually a single character of something that doesn't have asciimath meaning
    Ident(&'a str),
    /// A recognized symbol
    Symbol(&'a str),
    /// A unary operator
    Unary(SimpleUnary<'a>),
    /// A simple unary function
    Func(SimpleFunc<'a>),
    /// A binary function
    Binary(SimpleBinary<'a>),
    /// A bracketed group
    Group(Group<'a>),
    /// A matrix
    Matrix(Matrix<'a>),
}

impl<'a> Simple<'a> {
    /// Get as an option where Missing is None
    pub fn as_option(&self) -> Option<&Self> {
        match self {
            Simple::Missing => None,
            simple => Some(simple),
        }
    }
}

// macro to derive from for component types
macro_rules! simple_from {
    ($from:ty => $to:ident) => {
        impl<'a> From<$from> for Simple<'a> {
            fn from(inp: $from) -> Self {
                Simple::$to(inp)
            }
        }
    };
}

simple_from!(SimpleUnary<'a> => Unary);
simple_from!(SimpleFunc<'a> => Func);
simple_from!(SimpleBinary<'a> => Binary);
simple_from!(Group<'a> => Group);
simple_from!(Matrix<'a> => Matrix);

/// scripts attached to some other object
///
/// Note that an object can have a script attached, but the corresponding [Simple] expression can
/// still be [missing][Simple::Missing]. Objects with Scripts Deref to them so these raw objects
/// probably aren't necessary.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Script<'a> {
    /// No super or subscripts
    #[default]
    None,
    /// Only a subscript
    Sub(Simple<'a>),
    /// Only a superscript
    Super(Simple<'a>),
    /// A sub and superscript
    Subsuper(Simple<'a>, Simple<'a>),
}

impl<'a> Script<'a> {
    /// Get the subscript
    pub fn sub(&self) -> Option<&Simple<'a>> {
        match self {
            Script::Sub(sub) => Some(sub),
            Script::Subsuper(sub, _) => Some(sub),
            _ => None,
        }
    }

    /// Get the superscript
    pub fn sup(&self) -> Option<&Simple<'a>> {
        match self {
            Script::Super(sup) => Some(sup),
            Script::Subsuper(_, sup) => Some(sup),
            _ => None,
        }
    }
}

/// A simple expressiong with attached scripts
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SimpleScript<'a> {
    /// The simple argument
    pub simple: Simple<'a>,
    /// Any script modifications
    pub script: Script<'a>,
}

impl<'a> SimpleScript<'a> {
    /// Create a new simple script with a simple and script
    pub fn new<S>(simple: S, script: Script<'a>) -> Self
    where
        S: Into<Simple<'a>>,
    {
        SimpleScript {
            simple: simple.into(),
            script,
        }
    }

    /// Create an unscripted simple expression
    pub fn without_scripts<S>(simple: S) -> Self
    where
        S: Into<Simple<'a>>,
    {
        Self::new(simple, Script::None)
    }

    /// Create a simple script with a subscript
    pub fn with_sub<S, Sub>(simple: S, sub: Sub) -> Self
    where
        S: Into<Simple<'a>>,
        Sub: Into<Simple<'a>>,
    {
        Self::new(simple, Script::Sub(sub.into()))
    }

    /// Create a simple script with a super script
    pub fn with_super<S, Sup>(simple: S, sup: Sup) -> Self
    where
        S: Into<Simple<'a>>,
        Sup: Into<Simple<'a>>,
    {
        Self::new(simple, Script::Super(sup.into()))
    }

    /// Create a simple script with sub and super scripts
    pub fn with_subsuper<S, Sub, Sup>(simple: S, sub: Sub, sup: Sup) -> Self
    where
        S: Into<Simple<'a>>,
        Sub: Into<Simple<'a>>,
        Sup: Into<Simple<'a>>,
    {
        Self::new(simple, Script::Subsuper(sub.into(), sup.into()))
    }
}

impl<'a, S> From<S> for SimpleScript<'a>
where
    S: Into<Simple<'a>>,
{
    fn from(inp: S) -> Self {
        SimpleScript::without_scripts(inp.into())
    }
}

/// Add sub and super
impl<'a> Deref for SimpleScript<'a> {
    type Target = Script<'a>;

    fn deref(&self) -> &Self::Target {
        &self.script
    }
}

/// A full function that has complicated arguments and attached scripts
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func<'a> {
    /// The function name
    pub func: &'a str,
    /// Any script modifications of the function
    pub script: Script<'a>,
    /// The function argument
    arg: Box<ScriptFunc<'a>>,
}

impl<'a> Func<'a> {
    /// Create a new function with all attached parts
    pub fn new<Arg>(func: &'a str, script: Script<'a>, arg: Arg) -> Self
    where
        Arg: Into<ScriptFunc<'a>>,
    {
        Func {
            func,
            script,
            arg: Box::new(arg.into()),
        }
    }

    /// Create a new function without scripts
    pub fn without_scripts<Arg>(func: &'a str, arg: Arg) -> Self
    where
        Arg: Into<ScriptFunc<'a>>,
    {
        Self::new(func, Script::None, arg)
    }

    /// Create a new function with subscripts
    pub fn with_sub<Sub, Arg>(func: &'a str, sub: Sub, arg: Arg) -> Self
    where
        Sub: Into<Simple<'a>>,
        Arg: Into<ScriptFunc<'a>>,
    {
        Self::new(func, Script::Sub(sub.into()), arg)
    }

    /// Create a new function with superscripts
    pub fn with_super<Sup, Arg>(func: &'a str, sup: Sup, arg: Arg) -> Self
    where
        Sup: Into<Simple<'a>>,
        Arg: Into<ScriptFunc<'a>>,
    {
        Self::new(func, Script::Super(sup.into()), arg)
    }

    /// Create a new function with sub and superscripts
    pub fn with_subsuper<Sub, Sup, Arg>(func: &'a str, sub: Sub, sup: Sup, arg: Arg) -> Self
    where
        Sub: Into<Simple<'a>>,
        Sup: Into<Simple<'a>>,
        Arg: Into<ScriptFunc<'a>>,
    {
        Self::new(func, Script::Subsuper(sub.into(), sup.into()), arg)
    }

    /// The function argument
    pub fn arg(&self) -> &ScriptFunc<'a> {
        &self.arg
    }
}

impl<'a> Deref for Func<'a> {
    type Target = Script<'a>;

    fn deref(&self) -> &Self::Target {
        &self.script
    }
}

/// A scripted object or a scripted function
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScriptFunc<'a> {
    /// A scripted simple expression
    Simple(SimpleScript<'a>),
    /// A function with sub abd superscripts
    Func(Func<'a>),
}

impl<'a> Default for ScriptFunc<'a> {
    fn default() -> Self {
        ScriptFunc::Simple(SimpleScript::default())
    }
}

impl<'a> From<Func<'a>> for ScriptFunc<'a> {
    fn from(func: Func<'a>) -> Self {
        ScriptFunc::Func(func)
    }
}

impl<'a, S> From<S> for ScriptFunc<'a>
where
    S: Into<SimpleScript<'a>>,
{
    fn from(inp: S) -> Self {
        ScriptFunc::Simple(inp.into())
    }
}

/// A high level fraction
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frac<'a> {
    /// The numerator
    pub numer: ScriptFunc<'a>,
    /// The denominator
    pub denom: ScriptFunc<'a>,
}

impl<'a> Frac<'a> {
    /// Create a high level fraction
    pub fn new<N, D>(numer: N, denom: D) -> Self
    where
        N: Into<ScriptFunc<'a>>,
        D: Into<ScriptFunc<'a>>,
    {
        Frac {
            numer: numer.into(),
            denom: denom.into(),
        }
    }
}

/// An intermediate expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Intermediate<'a> {
    /// A simple scripted object
    ScriptFunc(ScriptFunc<'a>),
    /// A fraction between scripted objects
    Frac(Frac<'a>),
}

impl<'a> Default for Intermediate<'a> {
    fn default() -> Self {
        Intermediate::ScriptFunc(ScriptFunc::default())
    }
}

impl<'a> From<Frac<'a>> for Intermediate<'a> {
    fn from(frac: Frac<'a>) -> Self {
        Intermediate::Frac(frac)
    }
}

impl<'a, S> From<S> for Intermediate<'a>
where
    S: Into<ScriptFunc<'a>>,
{
    fn from(inp: S) -> Self {
        Intermediate::ScriptFunc(inp.into())
    }
}

/// A full expression
///
/// This is a sequence of intermediate expressions and Derefs to a slice of them.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Expression<'a>(Box<[Intermediate<'a>]>);

impl<'a> Deref for Expression<'a> {
    type Target = [Intermediate<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, I> FromIterator<I> for Expression<'a>
where
    I: Into<Intermediate<'a>>,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = I>,
    {
        Expression(iter.into_iter().map(|v| v.into()).collect())
    }
}

impl<'a, B> From<B> for Expression<'a>
where
    B: Into<Box<[Intermediate<'a>]>>,
{
    fn from(inp: B) -> Self {
        Expression(inp.into())
    }
}
