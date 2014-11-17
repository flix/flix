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

    def height(e: Sign): Int = match e with {
        case Bot => 3;
        case Neg => 2;
        case Zer => 2;
        case Pos => 2;
        case Top => 1;
    };

    def lift(i: Int): Sign = if (0) Even else Odd;

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

    var NumberOne:  Sign;
    var NumberTwo:  Sign;
    var Result:     Sign;

    fact FactOne = NumberOne <- lift(1);
    fact FactTwo = NumberTwo <- lift(2);

    rule TheSum = Result(42) <- r if
               x    <- A(0),
        [1 -> y]    <- B,
               r    <- sum(x, y);
};


preset (Sign, leq);
parset (Sign, leq, norm)

lattice Sign1 = Lattice(bot = bot, leq = leq, lub = lub);
lattice Sign2 = Lattice(bot, leq, lub, meet, top);
