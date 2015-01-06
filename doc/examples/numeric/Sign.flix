namespace SignDomain {

    enum Sign {
                  case Top,

        case Neg, case Zer, case Pos,

                  case Bot
    };

    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Bot, x)   => true;
        case (Neg, Neg) => true;
        case (Zer, Zer) => true;
        case (Pos, Pos) => true;
        case (_, Top)   => true;
        case _          => false;
    };

    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => e2;
        case (_, Bot)   => e1;
        case (Neg, Neg) => Neg;
        case (Zer, Zer) => Zer;
        case (Pos, Pos) => Pos;
        case _          => Top;
    };

    lat Sign = record { bot = Bot, leq = leq, lub = lub };

    @monotone @strict
    def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot;
        case (_, Bot)   => Bot;
        case (Neg, Neg) => Neg;
        case (Neg, Zer) => Neg;
        case (Neg, Pos) => Top;
    };
};



