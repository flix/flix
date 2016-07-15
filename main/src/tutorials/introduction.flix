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
// We begin by defining a namespace for all our declarations. We could have
// used the root namespace, but to avoid clashes with other examples we
// create a new namespace.
//
namespace Tutorial.CP {

    //
    // We will represent the program-under-analysis using three input
    // relations. One for each type of statement:
    //
    rel LitStm(r: Str, c: Int)         // r = c
    rel AddStm(r: Str, x: Str, y: Str) // r = x + y
    rel DivStm(r: Str, x: Str, y: Str) // r = x / y

    //
    // The next step is to define the lattice LocalVar which maps every local
    // variable to an element of the constant propagation lattice (which we
    // define below.)
    //
    // We can think of this lattice the result of the analysis.
    //
    lat LocalVar(k: Str, v: Constant)

    //
    // To define the constant propagation lattice, we begin by defining the
    // type of constants. We define an enum with three variants: bottom and
    // top elements, and a constructor Cst(i) for any integer i.
    //
    enum Constant {
          case Top,

        case Cst(Int),

          case Bot
    }

    //
    // The next task is to define the partial order on Constant.
    // This is straightforward with pattern matching on the arguments:
    //
    def leq(e1: Constant, e2: Constant): Bool = match (e1, e2) with {
        case (Constant.Bot, _)                      => true
        case (Constant.Cst(n1), Constant.Cst(n2))   => n1 == n2
        case (_, Constant.Top)                      => true
        case _                                      => false
    }

    //
    // Then its on to the least upper bound:
    //
    def lub(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, x)                      => x
        case (x, Constant.Bot)                      => x
        case (Constant.Cst(n1), Constant.Cst(n2))   => if (n1 == n2) e1 else Constant.Top
        case _                                      => Constant.Top
    }

    //
    // ... and the greatest lower bound:
    //
    def glb(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Top, x)                      => x
        case (x, Constant.Top)                      => x
        case (Constant.Cst(n1), Constant.Cst(n2))   => if (n1 == n2) e1 else Constant.Bot
        case _                                      => Constant.Bot
    }

    //
    // This complete the specification of the lattice. We associate the
    // lattice components with the Constant type in the following way:
    //
    let Constant<> = (Constant.Bot, Constant.Top, leq, lub, glb)
    // NB: In the future, this syntax is subject to change.

    //
    // Finally it is handy to have a small function to lift an integer into
    // an element of the constant propagation lattice. Strictly speaking,
    // this is not necessary.
    //
    def alpha(i: Int): Constant = Constant.Cst(i)

    //
    // We can now define the semantic rules of the analysis.
    //

    //
    // The first rule models literal statements r = c and lifts the constant
    // into the constant propagation lattice. Intuitively, if the body of
    // the rule (whatever is to the right of ':-') then the head (what is on
    // the left of ':-') is inferred.
    //
    LocalVar(r, alpha(c)) :- LitStm(r, c).

    //
    // The second rule is more interesting. This says that if we have an
    // addition statement r = x + y, and the value of x is v1 and of y is v2
    // then the value of r is at least the value of the sum of v1 and v2.
    //
    // Here sum is a reference to a transfer function implemented below.
    //
    LocalVar(r, sum(v1, v2)) :- AddStm(r, x, y),
                                LocalVar(x, v1),
                                LocalVar(y, v2).

    //
    // The third rule is similar, but for division:
    //
    LocalVar(r, div(v1, v2)) :- DivStm(r, x, y),
                                LocalVar(x, v1),
                                LocalVar(y, v2).

    //
    // We must define the sum transfer function:
    //
    def sum(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 + n2)
        case _                                      => Constant.Top
    }

    //
    // And the div transfer function:
    //
    def div(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (_, Constant.Bot) => Constant.Bot
        case (Constant.Bot, _) => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2)) => if (n2 == 0) Constant.Bot else Constant.Cst(n1 / n2)
        case _ => Constant.Top
    }

    // !! Notice the special case to avoid division-by-zero in the analysis itself !!

    //
    // This complete the analysis itself. But before we try to run it, let us
    // add some features to look for a simple type of bugs: Possible division-
    // by-zero errors.
    //

    //
    // We define a relation to hold the names of local variables possibly
    // involved in a division-by-zero error:
    //
    rel ArithmeticError(r: Str)

    //
    // We populate this relation with the rule below. This rule states that if
    // there is a division statement r = n / d, the value of the local variable
    // d is y, and y is possibly zero according to the `isMaybeZero` filter
    // function then we infer a possible error for `r`.
    //
    ArithmeticError(r) :- isMaybeZero(y),
                          DivStm(r, n, d),
                          LocalVar(d, y).

    //
    // Defining the filter function is straightforward:
    //
    def isMaybeZero(e: Constant): Bool = match e with {
        case Constant.Bot => false
        case Constant.Cst(n) => n == 0
        case Constant.Top => true
    }

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

    //
    // We can now run our program with Flix and inspect the output!
    //
    // Run: flix -p Tutorial.CP/LocalVar,Tutorial.CP/ArithmeticError tutorial.flix
    //
    // This should produce output like:
    //

    // Tutorial.CP/LocalVar:
    //
    //    +---+---------+
    //    | k | v       |
    //    +---+---------+
    //    | x | Cst(3)  |
    //    | y | Cst(7)  |
    //    | z | Cst(0)  |
    //    | w | Cst(10) |
    //    +---+---------+

    // Tutorial.CP/ArithmeticError
    //
    //    +---+
    //    | r |
    //    +---+
    //    | v |
    //    +---+

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

}
