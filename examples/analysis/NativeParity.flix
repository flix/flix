/**
 * A parity analysis is a simple numeric analysis which
 * determines whether a number is even or odd.
 * This example defines all the lattice operations in Java
 * static methods, as a demonstration of Flix's native interface.
 */
namespace ParityAnalysis {

    // define the lattice in terms of static native methods and fields.
    let #ca.uwaterloo.flix.util.misc.NativeParity<> = (
        #ca.uwaterloo.flix.util.misc.NativeParity.BOT,
        #ca.uwaterloo.flix.util.misc.NativeParity.TOP,
        #ca.uwaterloo.flix.util.misc.NativeParity.leq,
        #ca.uwaterloo.flix.util.misc.NativeParity.lub,
        #ca.uwaterloo.flix.util.misc.NativeParity.glb
    );

    // the following three functions are more workarounds.
    // note how these take native types as parameters.
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

    VarPointsTo(1, #ca.uwaterloo.flix.util.misc.NativeParity.ODD).
    VarPointsTo(2, #ca.uwaterloo.flix.util.misc.NativeParity.EVEN).

    VarPointsTo(7, plus(#ca.uwaterloo.flix.util.misc.NativeParity.ODD, #ca.uwaterloo.flix.util.misc.NativeParity.EVEN)).
    VarPointsTo(8, times(#ca.uwaterloo.flix.util.misc.NativeParity.ODD, #ca.uwaterloo.flix.util.misc.NativeParity.TOP)).

    VarPointsTo(t1, result) :- SumOp(t1, t2, t3), VarPointsTo(t2, v2), VarPointsTo(t3, v3),
                               isMaybeZero(v3),
                               t1 != t2,
                               result := plus(v2, v3).

    print VarPointsTo.
};