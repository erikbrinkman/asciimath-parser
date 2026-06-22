//! The module containing all of the structures that defined a parsed representation of asciimath
//!
//! - [`Expression`] - A full expression containing a sequence of intermediate expressions
//! - [`Intermediate`] - The most complicated single expression, this can be a high level fraction
//! - [`Frac`] - A high level fraction, often parsed with a `/`
//! - [`ScriptFunc`] - A scripted expression that can be a [`Func`] or just a simple expression
//! - [`Func`] - A function like `sin` that can contain independent super- and subscripts
//!   prior to its argument
//! - [`SimpleScript`] - A simple expression that has super- and subscripts
//! - [`Simple`] - A simple expression like a [`Symbol`][Simple::Symbol] or
//!   [`Ident`][Simple::Ident]ifier
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
    #[must_use]
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
    #[must_use]
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
    #[must_use]
    pub fn first(&self) -> &Simple<'a> {
        &self.first
    }

    /// The second operator argument
    #[must_use]
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
            cells,
            num_cols,
            right_bracket,
        }
    }

    /// The number of columns
    #[must_use]
    pub fn num_cols(&self) -> usize {
        self.num_cols
    }

    /// The number of rows
    #[must_use]
    pub fn num_rows(&self) -> usize {
        self.cells.len() / self.num_cols
    }

    /// The number of total cells
    #[must_use]
    pub fn num_cells(&self) -> usize {
        self.cells.len()
    }

    /// A top-down iterator over rows as slices
    #[must_use]
    pub fn rows(&self) -> MatrixRows<'a, '_> {
        MatrixRows(self.cells.chunks_exact(self.num_cols))
    }

    /// A top-down iterator over rows as slices
    #[must_use]
    pub fn iter(&self) -> MatrixRows<'a, '_> {
        self.into_iter()
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
impl FusedIterator for MatrixRows<'_, '_> {}

impl DoubleEndedIterator for MatrixRows<'_, '_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back()
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.0.nth_back(n)
    }
}

impl ExactSizeIterator for MatrixRows<'_, '_> {}

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

impl Simple<'_> {
    /// Get as an option where Missing is None
    #[must_use]
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
    #[must_use]
    pub fn sub(&self) -> Option<&Simple<'a>> {
        match self {
            Script::Sub(sub) | Script::Subsuper(sub, _) => Some(sub),
            _ => None,
        }
    }

    /// Get the superscript
    #[must_use]
    pub fn sup(&self) -> Option<&Simple<'a>> {
        match self {
            Script::Super(sup) | Script::Subsuper(_, sup) => Some(sup),
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
    #[must_use]
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

impl Default for ScriptFunc<'_> {
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

impl Default for Intermediate<'_> {
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
        Expression(iter.into_iter().map(std::convert::Into::into).collect())
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

#[cfg(test)]
mod tests {
    use super::{
        Expression, Frac, Func, Group, Intermediate, Matrix, Script, ScriptFunc, Simple,
        SimpleBinary, SimpleFunc, SimpleScript, SimpleUnary,
    };

    #[test]
    fn simple_unary() {
        let unary = SimpleUnary::new("sqrt", Simple::Ident("x"));
        assert_eq!(unary.op, "sqrt");
        assert_eq!(unary.arg(), &Simple::Ident("x"));
        assert_eq!(
            Simple::from(unary),
            Simple::Unary(SimpleUnary::new("sqrt", Simple::Ident("x")))
        );
    }

    #[test]
    fn simple_func() {
        let func = SimpleFunc::new("sin", Simple::Ident("x"));
        assert_eq!(func.func, "sin");
        assert_eq!(func.arg(), &Simple::Ident("x"));
        assert_eq!(
            Simple::from(func),
            Simple::Func(SimpleFunc::new("sin", Simple::Ident("x")))
        );
    }

    #[test]
    fn simple_binary() {
        let binary = SimpleBinary::new("root", Simple::Number("3"), Simple::Ident("x"));
        assert_eq!(binary.op, "root");
        assert_eq!(binary.first(), &Simple::Number("3"));
        assert_eq!(binary.second(), &Simple::Ident("x"));
        assert_eq!(
            Simple::from(binary),
            Simple::Binary(SimpleBinary::new(
                "root",
                Simple::Number("3"),
                Simple::Ident("x")
            )),
        );
    }

    #[test]
    fn group() {
        let from_expr = Group::new("(", Expression::from_iter([Simple::Ident("x")]), ")");
        let from_inters = Group::from_iter("(", [Simple::Ident("x")], ")");
        assert_eq!(from_expr, from_inters);
        assert_eq!(from_expr.left_bracket, "(");
        assert_eq!(from_expr.right_bracket, ")");
        assert_eq!(from_expr.expr.len(), 1);
        assert_eq!(Simple::from(from_expr.clone()), Simple::Group(from_expr));
    }

    fn cell(ident: &str) -> Expression<'_> {
        Expression::from_iter([Simple::Ident(ident)])
    }

    #[test]
    fn matrix() {
        let matrix = Matrix::new("[", [cell("a"), cell("b"), cell("c"), cell("d")], 2, "]");
        assert_eq!(matrix.left_bracket, "[");
        assert_eq!(matrix.right_bracket, "]");
        assert_eq!(matrix.num_cols(), 2);
        assert_eq!(matrix.num_rows(), 2);
        assert_eq!(matrix.num_cells(), 4);

        let rows: Vec<_> = matrix.rows().collect();
        assert_eq!(rows, [[cell("a"), cell("b")], [cell("c"), cell("d")]]);
        assert_eq!(matrix.iter().count(), 2);
        assert_eq!((&matrix).into_iter().count(), 2);

        assert_eq!(matrix[0], [cell("a"), cell("b")]);
        assert_eq!(matrix[1], [cell("c"), cell("d")]);
        assert_eq!(matrix[[0, 0]], cell("a"));
        assert_eq!(matrix[[1, 0]], cell("b"));
        assert_eq!(matrix[[0, 1]], cell("c"));
        assert_eq!(matrix[[1, 1]], cell("d"));

        assert_eq!(Simple::from(matrix.clone()), Simple::Matrix(matrix));
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn matrix_row_out_of_bounds() {
        let matrix = Matrix::new("[", [cell("a"), cell("b")], 2, "]");
        let _ = &matrix[1];
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn matrix_col_out_of_bounds() {
        let matrix = Matrix::new("[", [cell("a"), cell("b")], 2, "]");
        let _ = &matrix[[2, 0]];
    }

    #[test]
    #[should_panic(expected = "assertion")]
    fn matrix_ragged() {
        let _ = Matrix::new("[", [cell("a"), cell("b"), cell("c")], 2, "]");
    }

    #[test]
    fn matrix_rows_iterator() {
        let matrix = Matrix::new("[", [cell("a"), cell("b"), cell("c"), cell("d")], 1, "]");
        assert_eq!(matrix.rows().len(), 4);
        assert_eq!(matrix.rows().size_hint(), (4, Some(4)));
        assert_eq!(matrix.rows().nth(2), Some(&[cell("c")][..]));
        assert_eq!(matrix.rows().last(), Some(&[cell("d")][..]));

        let mut back = matrix.rows();
        assert_eq!(back.next_back(), Some(&[cell("d")][..]));
        assert_eq!(back.nth_back(1), Some(&[cell("b")][..]));
    }

    #[test]
    fn simple_as_option() {
        assert_eq!(Simple::Missing.as_option(), None);
        assert_eq!(Simple::Ident("x").as_option(), Some(&Simple::Ident("x")));
        assert_eq!(Simple::default(), Simple::Missing);
    }

    #[test]
    fn script_accessors() {
        assert_eq!(Script::default(), Script::None);
        assert_eq!(Script::<'_>::None.sub(), None);
        assert_eq!(Script::<'_>::None.sup(), None);

        let sub = Script::Sub(Simple::Ident("i"));
        assert_eq!(sub.sub(), Some(&Simple::Ident("i")));
        assert_eq!(sub.sup(), None);

        let sup = Script::Super(Simple::Number("2"));
        assert_eq!(sup.sub(), None);
        assert_eq!(sup.sup(), Some(&Simple::Number("2")));

        let both = Script::Subsuper(Simple::Ident("i"), Simple::Number("2"));
        assert_eq!(both.sub(), Some(&Simple::Ident("i")));
        assert_eq!(both.sup(), Some(&Simple::Number("2")));
    }

    #[test]
    fn simple_script() {
        let none = SimpleScript::without_scripts(Simple::Ident("x"));
        assert_eq!(none.simple, Simple::Ident("x"));
        // Deref to Script
        assert_eq!(none.sub(), None);
        assert_eq!(none.sup(), None);
        assert_eq!(SimpleScript::default().simple, Simple::Missing);

        let sub = SimpleScript::with_sub(Simple::Ident("x"), Simple::Ident("i"));
        assert_eq!(sub.sub(), Some(&Simple::Ident("i")));

        let sup = SimpleScript::with_super(Simple::Ident("x"), Simple::Number("2"));
        assert_eq!(sup.sup(), Some(&Simple::Number("2")));

        let both = SimpleScript::with_subsuper(
            Simple::Ident("x"),
            Simple::Ident("i"),
            Simple::Number("2"),
        );
        assert_eq!(both.sub(), Some(&Simple::Ident("i")));
        assert_eq!(both.sup(), Some(&Simple::Number("2")));

        // From any Into<Simple>
        assert_eq!(SimpleScript::from(Simple::Ident("x")), none);
    }

    #[test]
    fn func() {
        let none = Func::without_scripts("sum", Simple::Ident("x"));
        assert_eq!(none.func, "sum");
        assert_eq!(none.arg(), &ScriptFunc::from(Simple::Ident("x")));
        // Deref to Script
        assert_eq!(none.sub(), None);

        let sub = Func::with_sub("sum", Simple::Ident("i"), Simple::Ident("x"));
        assert_eq!(sub.sub(), Some(&Simple::Ident("i")));

        let sup = Func::with_super("sum", Simple::Number("n"), Simple::Ident("x"));
        assert_eq!(sup.sup(), Some(&Simple::Number("n")));

        let both = Func::with_subsuper(
            "sum",
            Simple::Ident("i"),
            Simple::Number("n"),
            Simple::Ident("x"),
        );
        assert_eq!(both.sub(), Some(&Simple::Ident("i")));
        assert_eq!(both.sup(), Some(&Simple::Number("n")));
    }

    #[test]
    fn script_func() {
        let from_func = ScriptFunc::from(Func::without_scripts("sum", Simple::Ident("x")));
        assert!(matches!(from_func, ScriptFunc::Func(_)));
        let from_simple = ScriptFunc::from(Simple::Ident("x"));
        assert!(matches!(from_simple, ScriptFunc::Simple(_)));
        assert_eq!(
            ScriptFunc::default(),
            ScriptFunc::Simple(SimpleScript::default())
        );
    }

    #[test]
    fn frac_and_intermediate() {
        let frac = Frac::new(Simple::Number("1"), Simple::Number("2"));
        assert_eq!(frac.numer, ScriptFunc::from(Simple::Number("1")));
        assert_eq!(frac.denom, ScriptFunc::from(Simple::Number("2")));

        assert_eq!(Intermediate::from(frac.clone()), Intermediate::Frac(frac));
        assert!(matches!(
            Intermediate::from(Simple::Ident("x")),
            Intermediate::ScriptFunc(_)
        ));
        assert!(matches!(
            Intermediate::default(),
            Intermediate::ScriptFunc(_)
        ));
    }

    #[test]
    fn expression() {
        let expr = Expression::from_iter([Simple::Ident("x"), Simple::Ident("y")]);
        // Deref to slice
        assert_eq!(expr.len(), 2);
        assert_eq!(expr[0], Intermediate::from(Simple::Ident("x")));
        assert_eq!(Expression::default().len(), 0);

        let boxed: Box<[Intermediate<'_>]> = Box::new([Intermediate::from(Simple::Ident("x"))]);
        assert_eq!(Expression::from(boxed).len(), 1);
    }
}
