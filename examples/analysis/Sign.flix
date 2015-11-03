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
        case (Sign.Bot, x     )   => true;
        case (Sign.Neg, Sign.Neg) => true;
        case (Sign.Zer, Sign.Zer) => true;
        case (Sign.Pos, Sign.Pos) => true;
        case (_, Sign.Top)        => true;
        case _                    => false;
    };

    /**
     * Returns the least upper bound of `e1` and `e2`
     * according to the partial order of Sign.
     */
    def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)        => e2;
        case (_, Sign.Bot)        => e1;
        case (Sign.Neg, Sign.Neg) => Sign.Neg;
        case (Sign.Zer, Sign.Zer) => Sign.Zer;
        case (Sign.Pos, Sign.Pos) => Sign.Pos;
        case _                    => Sign.Top;
    };

    /**
     *  Returns the great lower bound of `e1` and `e2`
     *  according to the partial order of Sign.
     */
    def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Top, _)        => e2;
        case (_, Sign.Top)        => e1;
        case (Sign.Neg, Sign.Neg) => Sign.Neg;
        case (Sign.Zer, Sign.Zer) => Sign.Zer;
        case (Sign.Pos, Sign.Pos) => Sign.Pos;
        case _                    => Sign.Bot;
    };

    let Sign<> = (Sign.Bot, Sign.Top, leq, lub, glb);

    /**
     * Returns the sum of `e1` and `e2`.
     */
    def plus(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)        => Sign.Bot;
        case (_, Sign.Bot)        => Sign.Bot;
        case (Sign.Neg, Sign.Neg) => Sign.Neg;
        case (Sign.Neg, Sign.Zer) => Sign.Neg;
        case (Sign.Neg, Sign.Pos) => Sign.Top;
        case (Sign.Zer, Sign.Neg) => Sign.Neg;
        case (Sign.Zer, Sign.Zer) => Sign.Zer;
        case (Sign.Zer, Sign.Pos) => Sign.Pos;
        case (Sign.Pos, Sign.Neg) => Sign.Top;
        case (Sign.Pos, Sign.Zer) => Sign.Pos;
        case (Sign.Pos, Sign.Pos) => Sign.Pos;
        case _                    => Sign.Top;
    };

    /**
     * Returns the product of `e1` and `e2`.
     */
    def times(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
        case (Sign.Bot, _)        => Sign.Bot;
        case (_, Sign.Bot)        => Sign.Bot;
        case (Sign.Neg, Sign.Neg) => Sign.Pos;
        case (Sign.Neg, Sign.Zer) => Sign.Zer;
        case (Sign.Neg, Sign.Pos) => Sign.Neg;
        case (Sign.Zer, Sign.Neg) => Sign.Zer;
        case (Sign.Zer, Sign.Zer) => Sign.Zer;
        case (Sign.Zer, Sign.Pos) => Sign.Zer;
        case (Sign.Pos, Sign.Neg) => Sign.Neg;
        case (Sign.Pos, Sign.Zer) => Sign.Zer;
        case (Sign.Pos, Sign.Pos) => Sign.Pos;
        case _                    => Sign.Top;
    };

    /**
     * Returns a boolean representing whether `e1` may equal `e2`.
     */
     def isMaybeEq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
        case (Sign.Top, _)        => e2 != Sign.Bot;
        case (_, Sign.Top)        => e1 != Sign.Bot;
        case (Sign.Neg, Sign.Neg) => true;
        case (Sign.Zer, Sign.Zer) => true;
        case (Sign.Pos, Sign.Pos) => true;
        case _                    => false;
     };

    lat Result(x: Int, v: Sign<>);

    Result(0, Sign.Bot).

    Result(1, Sign.Neg).
    Result(2, Sign.Zer).
    Result(3, Sign.Pos).

    Result(4, Sign.Neg).
    Result(4, Sign.Zer).
    Result(4, Sign.Pos).

    Result(5, plus(v1, v2)) :- isMaybeEq(v1, v2), Result(1, v1), Result(4, v2).

    Error#(v) :- Result(1, v), Result(2, v), Result(3, v), Result(4, v).

    print Result;

};



