namespace Sign {

    /**
     * A
     * multi-line
     * comment.
    */

    // A single-line comment
    // uh, one more :)

    type Sign = enum {
        case Top,
        case Neg,
        case Zer,
        case Pos,
        case Top
    };

    val bot: Sign = Bot;
    val top: Sign = Top;

    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Bot, x)   => true;
        case (Neg, Neg) => true;
        case (Zer, Zer) => true;
        case (Zer, Zer) => true;
        case (_, Top)   => true;
        case _          => false;
    };

    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, x)   => x;
        case (x, Bot)   => x;
        case (Neg, Neg) => Neg;
        case (Zer, Zer) => Zer;
        case (Pos, Pos) => Pos;
        case _          => Top;
    };

    def glb(e1: Sign, e2: Sign): Sign = ???;

    def ga2(e1: Sign, e2: Sign): Sign = !!!;

    def height(e: Sign): Int = match e with {
        case Bot => 3;
        case Neg => 2;
        case Zer => 2;
        case Pos => 2;
        case Top => 1;
    };

    def foo(x: Int, y: Int): Int =
        let a = 1 in
        let b = 2 in
            b;

    def lift(i: Int): Sign =
        if (true) Even else Odd;

    lat Sign = record { bot = Sign.Bot, leq = Sign.leq, lub = Sign.lub };

    @monotone @strict
    def sum(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot;
        case (_, Bot)   => Bot;
        case (Neg, Neg) => Neg;
        case (Neg, Zer) => Neg;
        case (Neg, Pos) => Top;
    };
};

namespace Example {

    // TODO: Read about sub-typing and figure this out.
    var Constant:   Set[(Str, Str)];
    var foo:        Rel[Str, Str];
    var PlusOp:     Map[Str, Str, Sign.Sign];
    var Value:      Map[Str, Sign.Sign];

    fact Fact1 = Constant <- ("x", 17);
    fact Fact2 = Constant <- ("y", 21);
    fact Fact3 = Constant <- ("z", 42);

    rule Lift = Value with [x -> v] if
        n <- Constant(x),
        v = Sign.lift(n);

    rule Sum = Value with [r -> v] if
        (r, x, y) <- PlusOp,
        (x, v1) <- Value,
        (y, v2) <- Value,
        v = Sign.sum(v1, v2);
};
