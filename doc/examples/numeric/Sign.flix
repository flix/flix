namespace Sign {

    /**
     * The Sign domain is a simple numeric domain for determining
     * whether a number is negative, zero or positive.
     */
    enum Sign {
                  case Top,

        case Neg, case Zer, case Pos,

                  case Bot
    };

    /**
     *  Returns `true` iff `e1` is less than or equal to `e2`
     *  according to the partial order of Sign.
     */
    def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Bot, x)   => true;
        case (Neg, Neg) => true;
        case (Zer, Zer) => true;
        case (Pos, Pos) => true;
        case (_, Top)   => true;
        case _          => false;
    };

    /**
     * Returns the least upper bound of `e1` and `e2`
     * according to the partial order of Sign.
     */
    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => e2;
        case (_, Bot)   => e1;
        case (Neg, Neg) => Neg;
        case (Zer, Zer) => Zer;
        case (Pos, Pos) => Pos;
        case _          => Top;
    };

    /**
     * Defines the Sign lattice.
     */
    lat Sign = record { bot = Bot, leq = leq, lub = lub };

    /**
     * Returns the sum of `e1` and `e2`.
     */
    @monotone @strict
    def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot;
        case (_, Bot)   => Bot;
        case (Neg, Neg) => Neg;
        case (Neg, Zer) => Neg;
        case (Neg, Pos) => Top;
        case (Zer, Neg) => Neg;
        case (Zer, Zer) => Zer;
        case (Zer, Pos) => Pos;
        case (Pos, Neg) => Top;
        case (Pos, Zer) => Pos;
        case (Pos, Pos) => Pos;
        case _          => Top;
    };

    /**
     * Returns the product of `e1` and `e2`.
     */
    @monotone @strict
    def times(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Bot, _)   => Bot;
        case (_, Bot)   => Bot;
        case (Neg, Neg) => Pos;
        case (Neg, Zer) => Zer;
        case (Neg, Pos) => Neg;
        case (Zer, Neg) => Zer;
        case (Zer, Zer) => Zer;
        case (Zer, Pos) => Zer;
        case (Pos, Neg) => Neg;
        case (Pos, Zer) => Zer;
        case (Pos, Pos) => Pos;
        case _          => Top;
    };

    /**
     * Returns the inverted sign of `e`.
     */
    @monotone @strict
    def invert(e: Sign): Sign = times(Neg, e);

    /**
     * Returns `true` if `e` may be zero.
     */
    @monotone @strict
    def isZero(e: Sign): Bool = match e with {
        case Zer => true;
        case Top => true;
        case _   => false;
    };

    /**
     * Returns `true` if `e` maybe non-zero.
     */
    @monotone @strict
    def isNonZero(e: Sign): Bool = match e with {
        case Neg => true;
        case Pos => true;
        case Top => true;
        case _   => false;
    };

};



