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
        case (Parity.Odd, Parity.Odd)     => Parity.Odd;
        case (Parity.Even, Parity.Even)   => Parity.Even;
        case _                            => Parity.Top;
    };

    // and likewise for the greatest lower bound:
    def glb(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (Parity.Top, _) => e2;
        case (_, Parity.Top) => e1;
        case (Parity.Odd, Parity.Odd) => Parity.Odd;
        case (Parity.Even, Parity.Even) => Parity.Even;
        case _ => Parity.Bot;
    }

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
        case (Parity.Odd, Parity.Odd)     => Parity.Odd;
        case (Parity.Odd, Parity.Even)    => Parity.Even;
        case (Parity.Even, Parity.Odd)    => ??? : Parity;
        case (Parity.Even, Parity.Even)   => Parity.Even;
        case _                            => Parity.Top;
    };

    // and division:
     def divide(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        case (_, Parity.Bot)              => Parity.Bot;
        case (Parity.Bot, _)              => Parity.Bot;
        case (Parity.Odd, Parity.Odd)     => ???: Parity;
        case (Parity.Odd, Parity.Even)    => ???: Parity;
        case (Parity.Even, Parity.Odd)    => ???: Parity;
        case (Parity.Even, Parity.Even)   => ???: Parity;
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

    let Parity<> = (Parity.Bot, Parity.Top, leq, lub, glb);

    rel SumOp(result: Int, left: Int, right: Int);
    lat VarPointsTo(localVal: Int, value: Parity<>);

    let Str<> = (
        #ca.uwaterloo.flix.util.misc.SampleLattice.BOT,
        #ca.uwaterloo.flix.util.misc.SampleLattice.TOP,
        #ca.uwaterloo.flix.util.misc.SampleLattice.leq,
        #ca.uwaterloo.flix.util.misc.SampleLattice.lub,
        #ca.uwaterloo.flix.util.misc.SampleLattice.glb
    );

    SumOp(3, 1, 2).
    VarPointsTo(1, Parity.Odd).
    VarPointsTo(2, Parity.Even).

    VarPointsTo(7, plus(Parity.Odd, Parity.Even)).
    VarPointsTo(8, times(Parity.Odd, Parity.Top)).

    VarPointsTo(t1, result) :- SumOp(t1, t2, t3), VarPointsTo(t2, v2), VarPointsTo(t3, v3),
                               isMaybeZero(v3),
                               t1 != t2,
                               result := plus(v2, v3).

    print VarPointsTo.

    val top: Str = #ca.uwaterloo.flix.util.misc.SampleLattice.TOP
    val bot: Str = #ca.uwaterloo.flix.util.misc.SampleLattice.BOT
    def foo(i: Int): Bool = #ca.uwaterloo.flix.util.misc.SampleLattice.leq(top, "<<top>>")
    def testBool(i: Int): Bool = #ca.uwaterloo.flix.util.misc.SampleLattice.testBool(#java.lang.Boolean.TRUE, false)

    rel R(b: Bool)
    R(foo(42)).
//    R(testBool(4)).
    print R.



};