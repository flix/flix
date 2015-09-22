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
        case (Parity.Bot, _)              => true;
        case (Parity.Odd, Parity.Odd)     => true;
        case (Parity.Even, Parity.Even)   => true;
        case (_, Parity.Top)              => true;
        case _                            => false;
    };

    // and similarly for the least upper bound:
    def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Parity.Bot, _)              => e2;
        case (_, Parity.Bot)              => e1;
        case (Parity.Odd, Parity.Odd)     => Odd;
        case (Parity.Even, Parity.Even)   => Parity.Even;
        case _                            => Parity.Top;
    };

    // we can define monotone operations on lattice elements. Here is addition:
    def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot;
        case (Parity.Bot, _)              => Parity.Bot;
        case (Parity.Odd, Parity.Odd)     => Parity.Even;
        case (Parity.Odd, Parity.Even)    => Parity.Odd;
        case (Parity.Even, Parity.Odd)    => Parity.Odd;
        case (Parity.Even, Parity.Even)   => Parity.Even;
        case _                            => Parity.Top;
    };

    // and subtraction:
    def minus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot;
        case (Parity.Bot, _)              => Parity.Bot;
        case (Parity.Odd, Parity.Odd)     => Parity.Even;
        case (Parity.Odd, Parity.Even)    => Parity.Odd;
        case (Parity.Even, Parity.Odd)    => Parity.Odd;
        case (Parity.Even, Parity.Even)   => Parity.Even;
        case _                            => Parity.Top;
    };

    // and multiplication:
    def times(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot;
        case (Parity.Bot, _)              => Parity.Bot;
        case (Parity.Odd, Parity.Odd)     => Odd;
        case (Parity.Odd, Parity.Even)    => Parity.Even;
        case (Parity.Even, Parity.Odd)    => Parity.Even;
        case (Parity.Even, Parity.Even)   => Parity.Even;
        case _                            => Parity.Top;
    };

    // and division:
     def divide(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot;
        case (Parity.Bot, _)              => Parity.Bot;
        case (Parity.Odd, Parity.Odd)     => ???;
        case (Parity.Odd, Parity.Even)    => ???;
        case (Parity.Even, Parity.Odd)    => ???;
        case (Parity.Even, Parity.Even)   => ???;
        case _                            => Parity.Top;
     };

    // we can test whether a number may be zero:
    def isMaybeZero(e: Parity): Bool = match e with {
        case Parity.Bot    => false;
        case Parity.Odd    => false;
        case Parity.Even   => true;
        case Parity.Top    => true;
    };

    // or may be odd:
    def isMaybeOdd(e: Parity): Bool = match e with {
        case Parity.Bot    => false;
        case Parity.Odd    => true;
        case Parity.Even   => false;
        case Parity.Top    => true;
    };

    // we abstract integers into the parity domain by using modulo.
    def lift(i: Int): Parity = if (i % 2 == 0) Even else Odd;

// TODO: Allow expressions here.
    lat <Parity> (Bot, leq, ParityAnalysis::lub);

    rel SumOp(result: Int, left: Int, right: Int);
    rel VarPointsTo(localVal: Int, value: <Parity>);

    SumOp(3, 1, 2).
    VarPointsTo(1, Parity.Odd).
    VarPointsTo(2, Parity.Even).

    VarPointsTo(t1, plus(v2, v3)) :- SumOp(t1, t2, t3), VarPointsTo(t2, v2), VarPointsTo(t3, v3).

};