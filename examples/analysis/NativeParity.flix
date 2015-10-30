/**
 * A parity analysis is a simple numeric analysis which
 * determines whether a number is even or odd.
 */
namespace ParityAnalysis {

    enum Parity {
                  case Top,
        case Odd,          case Even,
                  case Bot
    };

    let #ca.uwaterloo.flix.util.misc.NativeParity<> = (
        #ca.uwaterloo.flix.util.misc.NativeParity.BOT,
        #ca.uwaterloo.flix.util.misc.NativeParity.TOP,
        #ca.uwaterloo.flix.util.misc.NativeParity.leq,
        #ca.uwaterloo.flix.util.misc.NativeParity.lub,
        #ca.uwaterloo.flix.util.misc.NativeParity.glb
    );

    def flix2native(e: Parity): #ca.uwaterloo.flix.util.misc.NativeParity = match e with {
        case Parity.Top =>  #ca.uwaterloo.flix.util.misc.NativeParity.TOP;
        case Parity.Odd =>  #ca.uwaterloo.flix.util.misc.NativeParity.ODD;
        case Parity.Even => #ca.uwaterloo.flix.util.misc.NativeParity.EVEN;
        case Parity.Bot =>  #ca.uwaterloo.flix.util.misc.NativeParity.BOT;
    };

    def plus(e1: #ca.uwaterloo.flix.util.misc.NativeParity,
             e2: #ca.uwaterloo.flix.util.misc.NativeParity): #ca.uwaterloo.flix.util.misc.NativeParity =
        #ca.uwaterloo.flix.util.misc.NativeParity.plus(e1, e2);

    def times(e1: #ca.uwaterloo.flix.util.misc.NativeParity,
              e2: #ca.uwaterloo.flix.util.misc.NativeParity): #ca.uwaterloo.flix.util.misc.NativeParity =
        #ca.uwaterloo.flix.util.misc.NativeParity.times(e1, e2);

    def isMaybeZero(e: #ca.uwaterloo.flix.util.misc.NativeParity): Bool =
        #ca.uwaterloo.flix.util.misc.NativeParity.isMaybeZero(e)

    rel SumOp(result: Int, left: Int, right: Int);
    lat VarPointsTo(localVal: Int, value: #ca.uwaterloo.flix.util.misc.NativeParity<>);

    SumOp(3, 1, 2).
    VarPointsTo(1, flix2native(Parity.Odd)).
    VarPointsTo(2, flix2native(Parity.Even)).

    VarPointsTo(7, plus(flix2native(Parity.Odd), flix2native(Parity.Even))).
    VarPointsTo(8, times(flix2native(Parity.Odd), flix2native(Parity.Top))).

    VarPointsTo(t1, result) :- SumOp(t1, t2, t3), VarPointsTo(t2, v2), VarPointsTo(t3, v3),
                               isMaybeZero(v3),
                               t1 != t2,
                               result := plus(v2, v3).

    print VarPointsTo.
};