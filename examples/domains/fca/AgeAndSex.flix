/**
 * An implementation of a lattice from Formal Concept Analysis.
 *
 * http://www.upriss.org.uk/fca/fcaintro.html
 */
namespace AgeAndSex {

    enum AgeAndSex {
                             case Top,

        case Female, case Juvenile, case Adult, case Male,

        case Girl,    case Woman,    case Boy,  case Man,

                             case Bot
    }

    def leq(x: AgeAndSex, y: AgeAndSex): Bool = match (x, y) with {
        case (_, AgeAndSex.Top) => true
        case _ => false
    }

    //let AgeAndSex<> = (Belnap.Bot, Belnap.Top, leq, lub, glb)

}