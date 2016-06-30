//
// Welcome to the introductory Flix Tutorial!
//
// In this tutorial we will develop a simple intra-procedural dataflow analysis
// that performs constant propagation for local variables. The program language
// we consider is very simple, it has three types of statements:
//
//      x = c        [literal statement]
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
    rel LitStm(r: Str, c: Int)
    rel AddStm(r: Str, x: Str, y: Str)
    rel DivStm(r: Str, x: Str, y: Str)

    //
    // Here the LitStm relation holds every literal statement of the form r = c.
    // Similarly, for AddStm and DivStm.
    //

    lat LocalVar(k: Str, v: Constant)

    /**
     * The elements of the constant propagation lattice.
     */
    enum Constant {
          case Top,

        case Cst(Int),

          case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: Constant, e2: Constant): Bool = match (e1, e2) with {
        case (Constant.Bot, _)                      => true
        case (Constant.Cst(n1), Constant.Cst(n2))   => n1 == n2
        case (_, Constant.Top)                      => true
        case _                                      => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, x)                      => x
        case (x, Constant.Bot)                      => x
        case (Constant.Cst(n1), Constant.Cst(n2))   => if (n1 == n2) e1 else Constant.Top
        case _                                      => Constant.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Top, x)                      => x
        case (x, Constant.Top)                      => x
        case (Constant.Cst(n1), Constant.Cst(n2))   => if (n1 == n2) e1 else Constant.Bot
        case _                                      => Constant.Bot
    }

    /**
     * Abstracts a concrete integer into the Sign domain.
     */
    def alpha(i: Int): Constant = Constant.Cst(i)



    /**
     * Over-approximates integer `addition`.
     */
    @strict @monotone @commutative @associative
    def plus(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 + n2)
        case _                                      => Constant.Top
    }


    /**
     * Over-approximates integer `division`.
     */
    @strict @monotone
    def divide(e1: Constant, e2: Constant): Constant = match (e1, e2) with {
        case (Constant.Bot, _)                      => Constant.Bot
        case (_, Constant.Bot)                      => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2))   => Constant.Cst(n1 / n2)
        case _                                      => Constant.Top
    }

    let Constant<> = (Constant.Bot, Constant.Top, leq, lub, glb)


    LocalVar(r, lift(c)) :- LitStm(r, c).

    def lift(i: Int): Constant = Constant.Cst(i)

    LocalVar(r, sum(v1, v2)) :- AddStm(r, x, y),
                                LocalVar(x, v1),
                                LocalVar(y, v2).

    LocalVar(r, div(v1, v2)) :- DivStm(r, x, y),
                                LocalVar(x, v1),
                                LocalVar(y, v2).

    def sum(e1: Constant, e2: Constant): Constant
        = match (e1, e2) with {
            case (_, Constant.Bot) => Constant.Bot
            case (Constant.Bot, _) => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 + n2)
            case _ => Constant.Top
        }

    def div(e1: Constant, e2: Constant): Constant
        = match (e1, e2) with {
            case (_, Constant.Bot) => Constant.Bot
            case (Constant.Bot, _) => Constant.Bot
            case (Constant.Cst(n1), Constant.Cst(n2)) => if (n2 == 0) Constant.Bot else Constant.Cst(n1 / n2)
            case _ => Constant.Top
        }


        def isMaybeZero(e: Constant): Bool
            = match e with {
                case Constant.Bot => false
                case Constant.Cst(n) => n == 0
                case Constant.Top => true
            }



    rel ArithmeticError(r: Str)

    ArithmeticError(r) :- isMaybeZero(y),
                          DivStm(r, n, d),
                          LocalVar(d, y).

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

    LitStm("x", 3).
    LitStm("y", 7).
    LitStm("z", 0).
    AddStm("w", "x", "y").
    DivStm("v", "w", "z").

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

}
