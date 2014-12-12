/**
 * A parity analysis is a simple numeric analysis which
 * determines whether a number is even or odd.
 */
namespace ParityAnalysis {

    // the parity lattice consists of just four elements:
    enum Parity {
                 case Top,
        case Odd,         case Even,
                 case Bot
    };

    // the partial order is easily defined using pattern matching:
    def leq(e1: Parity, e2: Parity): Bool = match (e1, e2) with {
        case (Bot, _)       => true;
        case (Odd, Odd)     => true;
        case (Even, Even)   => true;
        case (_, Top)       => true;
        case _              => false;
    };

    // and similarly for the least upper bound:
    def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Bot, _)       => e2;
        case (_, Bot)       => e1;
        case (Odd, Odd)     => Odd;
        case (Even, Even)   => Even;
        case _              => Top;
    };



};