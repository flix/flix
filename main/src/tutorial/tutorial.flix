//
//   Welcome to the Flix Tutorial!
//
//   In this tutorial
//

namespace Tutorial.ConstantPropagation {

    rel LitExp(r: Str, c: Int)
    rel AddExp(r: Str, x: Str, y: Str)
    rel DivExp(r: Str, x: Str, y: Str)

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


    LocalVar(r, lift(c)) :- LitExp(r, c).

    def lift(i: Int): Constant = Constant.Cst(i)

    LocalVar(r, sum(v1, v2)) :- AddExp(r, x, y),
                                LocalVar(x, v1),
                                LocalVar(y, v2).

    LocalVar(r, div(v1, v2)) :- DivExp(r, x, y),
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
            // TODO: zero case
            case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 / n2)
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
                          DivExp(r, n, d),
                          LocalVar(d, y).

    //
    // Let us now try to analyse the following small program:
    //
    // x = 42;
    // y = 21;
    // z = 0;

    LitStm("x", 42).
    LitStm("y", 21).

}