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
        case (_, AgeAndSex.Top)                         => true
        case (AgeAndSex.Girl, AgeAndSex.Female)         => true
        case (AgeAndSex.Girl, AgeAndSex.Juvenile)       => true
        case (AgeAndSex.Woman, AgeAndSex.Female)        => true
        case (AgeAndSex.Woman, AgeAndSex.Adult)         => true
        case (AgeAndSex.Boy, AgeAndSex.Juvenile)        => true
        case (AgeAndSex.Boy, AgeAndSex.Male)            => true
        case (AgeAndSex.Man, AgeAndSex.Adult)           => true
        case (AgeAndSex.Man, AgeAndSex.Male)            => true
        case _                                          => false
    }

    def lub(x: AgeAndSex, y: AgeAndSex): AgeAndSex = match (x, y) with {
        case (AgeAndSex.Bot, _)                         => y
        case (_, AgeAndSex.Bot)                         => x
        case (AgeAndSex.Girl, AgeAndSex.Female)         => AgeAndSex.Female
        case (AgeAndSex.Girl, AgeAndSex.Juvenile)       => AgeAndSex.Juvenile
        case (AgeAndSex.Girl, AgeAndSex.Boy)            => AgeAndSex.Juvenile
        case (AgeAndSex.Woman, AgeAndSex.Female)        => AgeAndSex.Female
        case (AgeAndSex.Woman, AgeAndSex.Adult)         => AgeAndSex.Adult
        case (AgeAndSex.Woman, AgeAndSex.Man)           => AgeAndSex.Adult

        case _ => AgeAndSex.Top
    }

   let AgeAndSex<> = (AgeAndSex.Bot, AgeAndSex.Top, leq, lub, lub)

}