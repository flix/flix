/**
 * The ParitySign lattice is the reduced product of the Parity and Sign lattices.
 */
namespace ParitySign {

    // http://www.pietro.ferrara.name/2013_Dale.PDF
    // The lattice and semantic structures of the reduced product are exactly the same as those of the Cartesian
    // product. In addition, a reduction operator aimed at refining the information tracked by the two
    // domains is introduced, and it is used after each lattice or semantic operator application.

    /**
     * The elements of the Parity lattice.
     */
    enum Parity {
                  case Top,

        case Odd,          case Even,

                  case Bot
    }

    /**
     * The elements of the Sign lattice.
     */
    enum Sign {
                  case Top,

        case Neg,          case Pos,

                  case Zer,

                  case Bot
    }

    /**
     * The Cartesian product of the two lattices.
     */
    enum ParitySign {
        case PS(Parity, Sign)
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(x: ParitySign, y: ParitySign): Bool = match (x, y) with {
        case (_, ParitySign.PS(Parity.Top, Sign.Top)) => true

        case _                                        => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) with {
        case _ => ParitySign.PS(Parity.Top, Sign.Top)
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: ParitySign, e2: ParitySign): ParitySign = match (e1, e2) with {
        case _ => ParitySign.PS(Parity.Bot, Sign.Bot)
    }

    let ParitySign<> = (ParitySign.PS(Parity.Bot, Sign.Bot), ParitySign.PS(Parity.Top, Sign.Top), leq, lub, glb)

}