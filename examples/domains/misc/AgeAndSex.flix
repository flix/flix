/**
 * An implementation of a lattice from Formal Concept Analysis.
 *
 * http://www.upriss.org.uk/fca/fcaintro.html
 */
namespace AgeAndSex {

    // TODO: Throw away?

    enum AgeAndSex {
                             case Top,

        case Female, case Juvenile, case Adult, case Male,

        case Girl,    case Woman,    case Boy,  case Man,

                             case Bot
    }

    // TODO: Here I actually made a mistake forgetting reflexivity.
    // As simple, as that may sound.
    // And I also forgot the bottom case.

    def leq(x: AgeAndSex, y: AgeAndSex): Bool = match (x, y) with {
        case (_, AgeAndSex.Top)                         => true
        case (AgeAndSex.Bot, _)                         => true
        case (AgeAndSex.Girl, AgeAndSex.Female)         => true
        case (AgeAndSex.Girl, AgeAndSex.Juvenile)       => true
        case (AgeAndSex.Woman, AgeAndSex.Female)        => true
        case (AgeAndSex.Woman, AgeAndSex.Adult)         => true
        case (AgeAndSex.Boy, AgeAndSex.Juvenile)        => true
        case (AgeAndSex.Boy, AgeAndSex.Male)            => true
        case (AgeAndSex.Man, AgeAndSex.Adult)           => true
        case (AgeAndSex.Man, AgeAndSex.Male)            => true
        case (AgeAndSex.Juvenile, AgeAndSex.Juvenile)   => true
        case (AgeAndSex.Adult, AgeAndSex.Adult)         => true
        case _                                          => x == y
    }

    @commutative @associative
    def lub(x: AgeAndSex, y: AgeAndSex): AgeAndSex = match (x, y) with {
        case (AgeAndSex.Bot, _)                         => y
        case (_, AgeAndSex.Bot)                         => x
        case (AgeAndSex.Girl, AgeAndSex.Female)         => AgeAndSex.Female
        case (AgeAndSex.Girl, AgeAndSex.Juvenile)       => AgeAndSex.Juvenile
        case (AgeAndSex.Girl, AgeAndSex.Boy)            => AgeAndSex.Juvenile
        case (AgeAndSex.Woman, AgeAndSex.Female)        => AgeAndSex.Female
        case (AgeAndSex.Woman, AgeAndSex.Adult)         => AgeAndSex.Adult
        case (AgeAndSex.Woman, AgeAndSex.Man)           => AgeAndSex.Adult
        case (AgeAndSex.Boy, AgeAndSex.Juvenile)        => AgeAndSex.Juvenile
        case (AgeAndSex.Boy, AgeAndSex.Male)            => AgeAndSex.Male
        case (AgeAndSex.Boy, AgeAndSex.Girl)            => AgeAndSex.Juvenile
        case (AgeAndSex.Man, AgeAndSex.Adult)           => AgeAndSex.Adult
        case (AgeAndSex.Man, AgeAndSex.Male)            => AgeAndSex.Male
        case (AgeAndSex.Man, AgeAndSex.Woman)           => AgeAndSex.Adult
        case _ => if (x == y) x else AgeAndSex.Top
    }

   let AgeAndSex<> = (AgeAndSex.Bot, AgeAndSex.Top, leq, lub, lub) // TODO: GLB

}