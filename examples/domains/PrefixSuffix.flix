// Giulia Costantini1
//   , Pietro Ferrara2
  // , and Agostino Cortesi1
  // https://www.researchgate.net/profile/Agostino_Cortesi/publication/220744419_Static_Analysis_of_String_Values/links/0fcfd513df66e75c12000000.pdf

//  TODO: Good use case for lists etc.

namespace PrefixSuffix {

    /**
     * The elements of the prefix lattice.
     */
    enum Prefix {
        case Pre(List[Char]),
        case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Prefix, e2: Prefix): Bool = match (e1, e2) with {
        case (Prefix.Bot, _)                    => true
        case (Prefix.Pre(s1), Prefix.Pre(s2))   =>
            ???: Bool

        case (_, Prefix.Pre(Nil))               => true
        case _                                  => false
    }


}