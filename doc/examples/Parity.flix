/**
 * A parity analysis is a simple numeric analysis which
 * determines whether a number is even or odd.
 */
namespace ParityAnalysis {

    // the parity lattice consists of just four elements:
    enum Parity {
                  case Top,
        case Odd,          case Even,
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

    // the lattice is defined as a record:
    lat Parity = record {
        bot = Bot, leq = leq, lub = lub
    };

    // NB: Notice that the "Parity" name is overloaded;
    // it is used both as the name of the type of the enum
    // and as a lattice declaraction. However, these live in two
    // different namespaces and thus there is no ambiguity.

    // we can define monotone operations on lattice elements. Here is addition:
    @strict @monotone
    def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Bot)       => Bot;
        case (Bot, _)       => Bot;
        case (Odd, Odd)     => Even;
        case (Odd, Even)    => Odd;
        case (Even, Odd)    => Odd;
        case (Even, Even)   => Even;
        case _              => Top;
    };

    // and substraction:
    @strict @monotone
    def minus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Bot)       => Bot;
        case (Bot, _)       => Bot;
        case (Odd, Odd)     => Even;
        case (Odd, Even)    => Odd;
        case (Even, Odd)    => Odd;
        case (Even, Even)   => Even;
        case _              => Top;
    };

    // and multiplication:
    @strict @monotone
    def times(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Bot)       => Bot;
        case (Bot, _)       => Bot;
        case (Odd, Odd)     => Odd;
        case (Odd, Even)    => Even;
        case (Even, Odd)    => Even;
        case (Even, Even)   => Even;
        // ...
    };

    // and division:


    // and whether the number may posible be zero.
    @strict @monotone
    def isMaybeZero(e: Parity): Bool = match e with {
        case Bot    => false;
        case Odd    => false;
        case Even   => true;
        case Top    => true;
    };

    // we can abstract integers into the parity domain:
    def lift(i: Int): Parity = if (i % 2 == 0) Even else Odd;

};