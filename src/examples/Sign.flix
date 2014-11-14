namespace Sign {

    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Bot, x)   => true;
        case (Neg, Neg) => true;
        case (Zer, Zer) => true;
        case (Zer, Zer) => true;
        case (_ Top)    => true;
        case _          => false;
    };

    val bot: Sign = Bot;
    val top: Sign = Top;



    type Sign = Top | Neg | Zer | Pos | Bot;

    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, x)   => x;
        case (x, Bot)   => x;
        case (Neg, Neg) => Neg;
        case (Zer, Zer) => Zer;
        case (Pos, Pos) => Pos;
        cas _           => Top;
    };

    def glb(e1: Sign, e2: Sign): Sign = ???;

       // TODO preset, posset,
    preset (Sign, leq);
    parset (Sign, leq, norm)

    lattice Sign1 = Lattice(bot = bot, leq = leq, lub = lub);
    lattice Sign2 = Lattice(bot, leq, lub, meet, top);

    // join-lattice
    // meet-lattice
    // lattice

    def height(e: Sign): Int = match e with {
        case Bot => 3;
        case Neg => 2;
        case Zer => 2;
        case Pos => 2;
        case Top => 1;
    }

    type Sign = Top | Neg | Zer | Pos | Bot;


    /** Comment

    **/
    def transitive(xs: Set[Int]) = fixpoint {
      // nested contraints
      var Result: Set[Int];

      rule Input = Result <- (i, i) if
        i <- xs.

      rule Reflexivity1 = Result <- (x, x) if
        (x, _) <- Result.
      rule Reflexivity2 = Result <- (x, x) if
        (_, x) <- Result.
    }

    def lift(i: Int): Sign = if (i % 2 == 0) Even else Odd;

    @monotone @strict
    def sum(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot;
        case (_, Bot)   => Bot;
        case (Neg, Neg) => Neg;
        case (Neg, Zer) => Neg;
        case (Neg, Pos) => Top;
        // ....
    }
};

namespace Sign {

    var NumberOne:  Sign;
    var NumberTwo:  Sign;
    var Result:     Int -> Sign;

    fact Fact-One = NumberOne <- lift(1);
    fact Fact-Two = NumberTwo <- lift(2);

    rule TheSum = Result(42) <- r if
               x    <- A(0),
        [1 -> y]    <- B,
               r    <- sum(x, y);
};
