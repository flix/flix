// TODO: Does this make sense?
// TODO: Is this the reduced product in disguise?
namespace ConstantSign {

    /**
     * The elements of the constant-parity lattice.
     */
    enum ConstParity {
                  case Top,

        case Odd,          case Even,

                case Cst(BigInt),

                  case Bot
    }

}