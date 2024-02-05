//
// Welcome to the introduction to Flix!
//
// In this tutorial we will develop a simple intra-procedural dataflow analysis
// that performs constant propagation for local variables. The program language
// we consider is very simple, it has three types of statements:
//
//      x = c        [assignment statement]
//      x = y + z    [addition statement]
//      x = y / z    [division statement]
//

//
// The next step is to define the lattice LocalVar which maps every local
// variable to an element of the constant propagation lattice (which we
// define below.)
//
// We can think of this lattice as the result of the analysis.
//
enum LocalVar({k = String, v = Constant})

//
// To define the constant propagation lattice, we begin by defining the
// type of constants. We define an enum with three variants: bottom and
// top elements, and a constructor Constant.Cst(i) for any integer i.
//
enum Constant {
      case Top,

    case Cst(Int32),

      case Bot
}

instance LowerBound[Constant] {
    pub def minValue(): Constant = Constant.Bot
}

//
// We define equality of two lattice elements by structural equality.
//
instance Eq[Constant] {
    pub def eq(x: Constant, y: Constant): Bool = match (x, y) {
        case (Constant.Top, Constant.Top)       => true
        case (Constant.Cst(a), Constant.Cst(b)) => a == b
        case (Constant.Bot, Constant.Bot)       => true
        case _                                  => false
    }
}

//
// The next task is to define the partial order on Constant.
// This is straightforward with pattern matching on the arguments:
//
instance PartialOrder[Constant] {
    pub def lessEqual(e1: Constant, e2: Constant): Bool = match (e1, e2) {
        case (Constant.Bot, _)                    => true
        case (Constant.Cst(n1), Constant.Cst(n2)) => n1 == n2
        case (_, Constant.Top)                    => true
        case _                                    => false
    }
}

//
// Then it's on to the least upper bound:
//
instance JoinLattice[Constant] {
    pub def leastUpperBound(x: Constant, y: Constant): Constant = match (x, y) {
        case (Constant.Bot, _)                    => y
        case (_, Constant.Bot)                    => x
        case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) x else Constant.Top
        case _                                    => Constant.Top
    }
}

//
// ... and the greatest lower bound:
//
instance MeetLattice[Constant] {
    pub def greatestLowerBound(e1: Constant, e2: Constant): Constant = match (e1, e2) {
        case (Constant.Top, x)                    => x
        case (x, Constant.Top)                    => x
        case (Constant.Cst(n1), Constant.Cst(n2)) => if (n1 == n2) e1 else Constant.Bot
        case _                                    => Constant.Bot
    }
}

instance Order[Constant] {
    pub def compare(x: Constant, y: Constant): Comparison = match (x, y) {
        case (Constant.Bot, Constant.Bot)         => Comparison.EqualTo
        case (Constant.Bot, Constant.Cst(_))      => Comparison.LessThan
        case (Constant.Bot, Constant.Top)         => Comparison.LessThan
        case (Constant.Cst(_), Constant.Bot)      => Comparison.GreaterThan
        case (Constant.Cst(v1), Constant.Cst(v2)) => v1 <=> v2
        case (Constant.Cst(_), Constant.Top)      => Comparison.LessThan
        case (Constant.Top, Constant.Bot)         => Comparison.GreaterThan
        case (Constant.Top, Constant.Cst(_))      => Comparison.GreaterThan
        case (Constant.Top, Constant.Top)         => Comparison.EqualTo
    }
}

//
// We need an instance of ToString for pretty printing:
//
instance ToString[Constant] {
    pub def toString(x: Constant): String = match x {
        case Constant.Top    => "Constant.Top"
        case Constant.Cst(n) => "Constant.Cst(${n})"
        case Constant.Bot    => "Constant.Bot"
    }
}

//
// Finally it is handy to have a small function to lift an integer into
// an element of the constant propagation lattice. Strictly speaking,
// this is not necessary.
//
def alpha(i: Int32): Constant = Constant.Cst(i)

//
// We must define the sum transfer function:
//
def sum(e1: Constant, e2: Constant): Constant = match (e1, e2) {
    case (Constant.Bot, _)                    => Constant.Bot
    case (_, Constant.Bot)                    => Constant.Bot
    case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 + n2)
    case _                                    => Constant.Top
}

//
// And the div transfer function:
//
def div(e1: Constant, e2: Constant): Constant = match (e1, e2) {
    case (_, Constant.Bot)                    => Constant.Bot
    case (Constant.Bot, _)                    => Constant.Bot
    case (Constant.Cst(n1), Constant.Cst(n2)) => if (n2 == 0) Constant.Bot else Constant.Cst(n1 / n2)
    case _                                    => Constant.Top
}

// !! Notice the special case to avoid division-by-zero in the analysis itself !!

//
// This complete the analysis itself. But before we try to run it, let us
// add some features to look for a simple type of bugs: Possible division-
// by-zero errors.
//



//
// Defining the filter function is straightforward:
//
def isMaybeZero(e: Constant): Bool = match e {
    case Constant.Bot    => false
    case Constant.Cst(n) => n == 0
    case Constant.Top    => true
}

def main(): Unit \ IO =
    let p = #{
        //
        // We can now define the semantic rules of the analysis.
        //

        //
        // The first rule models literal statements r = c and lifts the constant
        // into the constant propagation lattice. Intuitively, if the body of
        // the rule holds (whatever is to the right of ':-') then the head (what
        // is on the left of ':-') is inferred.
        //
        LocalVar(r; alpha(c)) :- LitStm(r, c).

        //
        // The second rule is more interesting. This says that if we have an
        // addition statement r = x + y, and the value of x is v1 and of y is v2
        // then the value of r is at least the value of the sum of v1 and v2.
        //
        // Here sum is a reference to a transfer function implemented below.
        //
        LocalVar(r; sum(v1, v2)) :- AddStm(r, x, y),
                                    LocalVar(x; v1),
                                    LocalVar(y; v2).

        //
        // The third rule is similar, but for division:
        //
        LocalVar(r; div(v1, v2)) :- DivStm(r, x, y),
                                    LocalVar(x; v1),
                                    LocalVar(y; v2).

        //
        // We populate this relation with the rule below. This rule states that if
        // there is a division statement r = n / d, the value of the local variable
        // d is y, and y is possibly zero according to the `isMaybeZero` filter
        // function then we infer a possible error for `r`.
        //
        ArithmeticError(r) :- DivStm(r, _n, d),
                              LocalVar(d; y),
                              if isMaybeZero(y).

        //
        // Let us now try to analyse the following small program:
        //
        // x = 3;
        // y = 7;
        // z = 0;
        // w = x + y;
        // v = w / z;

        //
        // We encode this program using the input facts:
        //
        LitStm("x", 3).        // x = 3
        LitStm("y", 7).        // y = 7
        LitStm("z", 0).        // z = 0
        AddStm("w", "x", "y"). // w = x + y
        DivStm("v", "w", "z"). // v = w / z
    };
    let vars = query p select (a, b) from LocalVar(a; b);
    let errs = query p select r from ArithmeticError(r);
    println(vars);
    println(errs)

//
// We can now run our program with Flix and inspect the output!
//
// This should produce output like:
//

// LocalVar
// +---------+------------------+
// | k       | v                |
// +---------+------------------+
// | List(x) | Constant.Cst(3)  |
// | List(v) | Constant.Bot(()) |
// | List(y) | Constant.Cst(7)  |
// | List(w) | Constant.Cst(10) |
// | List(z) | Constant.Cst(0)  |
// +---------+------------------+

// ArithmeticError
// +---+
// | r |
// +---+
// | v |
// +---+

//
// Here are some exercises for learning Flix:
//
// (0) Write some input programs and run the analysis on them.
// (1) Extend the analysis with a variable assignment statement: x = y.
// (2) Extend the analysis with operators for subtraction and multiplication.
// (3) Modify the (constant propagation) lattice to have two extra elements Neg and Pos,
//     and re-implement the partial order, least upper bound, greatest lower bound,
//     and operations on the lattice to support these two new elements.
//
