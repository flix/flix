/**
 * An implementation of the (alternative) sign domain
 * lifted over the constant propagation domain.
 */
namespace ConstantSign {

    /**
     * The elements of the constant-sign lattice.
     */
    enum ConstSign {
                  case Top,

        case Neg,          case Pos

                  case Cst(Int)

                  case Bot
    }

    // TODO ...

}