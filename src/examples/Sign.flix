namespace Sign {

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

    lat Sign = lattice { bot = Sign.Bot, leq = Sign.leq, lub = Sign.lub };

    @monotone @strict
    def sum(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot;
        case (_, Bot)   => Bot;
        case (Neg, Neg) => Neg;
        case (Neg, Zer) => Neg;
        case (Neg, Pos) => Top;
    };
};

namespace Sign {

    var NumberOne:  Int -> (Int, Int, Str) -> Int;
    var NumberTwo:  Sign;
    var NumberTree: Set[Int];
    var Result:     Sign;

    fact FactOne = NumberOne <- lift(1);
    fact FactTwo = NumberTwo <- lift(2);
    fact FactTree = NumberTree <- Set(1, 2, 3);

    rule TheSum = Result <- r if
        Set(x) <- FactTree;

    rule TheSum = Result(42) <- r if
        x    <- A(0),
        y    <- B(1),
        r    <- sum(x, y);

    def foo(): Set[Int] = Set(1, 2, 3);

    def bar(): Map[Int, Int] = Map((1 -> 2), (2 -> 3));
};
