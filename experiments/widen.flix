enum Interval with Eq, Order, ToString {
                case Top,

    case Range(BigInt, BigInt),

                case Bot
}

instance LowerBound[Interval] {
    pub def minValue(_: Unit): Interval = Interval.Bot
}


instance PartialOrder[Interval] {
    pub def lessEqual(x: Interval, y: Interval): Bool = match (x, y) {
        case (Bot, _)                       => true
        case (Range(b1, e1), Range(b2, e2)) => b2 <= b1 and e1 <= e2
        case (_, Top)                       => true
        case _                              => false
    }
}

instance JoinLattice[Interval] {
    pub def leastUpperBound(x: Interval, y: Interval): Interval = match (x, y) {
        case (Bot, _)                       => y
        case (_, Bot)                       => x
        case (Range(b1, e1), Range(b2, e2)) => Range(BigInt.min(b1, b2), BigInt.max(e1, e2))
        case _                              => Top
    }
}


instance MeetLattice[Interval] {
    pub def greatestLowerBound(x: Interval, y: Interval): Interval = match (x, y) {
        case (Top, _)                       => y
        case (_, Top)                       => x
        case (Range(b1, e1), Range(b2, e2)) => Range(BigInt.max(b1, b2), BigInt.min(e1, e2))
        case _                              => Bot
    }
}

pub def alpha(i: BigInt): Interval = Range(i, i)

pub def inc(x: Interval): Interval = match x {
    case Bot            => Bot
    case Range(b, e)    => debug!!(Range(b + 1ii, e + 1ii))
    case Top            => Top
}


///
/// The naive widen function only looks at a single lattice element.
///
def naiveWiden(x: Interval): Interval = match x {
    case Bot            => Bot
    case Range(b, e)    =>
        // Jump to top if the width has become more than 25.
        if (e - b <= 25ii)
            Range(b, e)
        else
            Top
    case Top            => Top
}

def main(): Unit \ IO =
    let pr = #{
        //
        //      x = 42
        //      A
        // inc  | \       widen if span > 100
        //      B--C
        //      |
        //      D
        //
        Edge("A", "B").
        Edge("B", "C").
        Edge("C", "A").
        Edge("B", "D").

        // Initial state at A.
        L("A", "x"; alpha(42ii)).

        // Widening on A-C if the interval becomes too large.
        L("A", "x"; naiveWiden(v)) :- L("C", "x"; v).

        // Transfer function from A-B.
        L("B", x; inc(v)) :- L("A", x; v).

        // Propagate function (excluding A-B)
        L(succ, x; v) :- Edge(pred, succ), L(pred, x; v), if pred != "A".
    };
    query pr select (pp, x, v) from L(pp, x; v) |> println


