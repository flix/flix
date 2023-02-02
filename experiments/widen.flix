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

///
/// The general widen function takes both the old and new lattice elements.
/// This is often called nabla (upside triangle.)
///
def smartWiden(oldElm: Interval, newElm: Interval): Interval = match (oldElm, newElm) {
    case (Range(oldB, oldE), Range(newB, newE))    =>
        // Compute the length of each interval.
        let oldLen = oldE - oldB;
        let newLen = newE - newB;
        // If the interval only increased by 1 then we jump to top.
        let delta = debug!!(newLen - oldLen);
        if (delta <= 1ii)
            debug!!(Top)
        else
            newElm
    case _            => newElm
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

        // To avoid some issues with bottom, we assume the constant 42 is everywhere.
        L("A", "x"; alpha(42ii)).
        L("B", "x"; alpha(42ii)).
        L("C", "x"; alpha(42ii)).
        L("D", "x"; alpha(42ii)).

        // Transfer function from A-B with naive widening.
        //L("B", x; naiveWiden(inc(v))) :- L("A", x; v).
        // Note how the above rule requires several iterations through the cycle.

        // Transfer function from A-B with smart widening.
        L("B", x; smartWiden(vold, inc(v))) :- L("A", x; v), L("B", x; vold).  // Implicit assumption that vold is not bottom.
        // Note how this reaches the fixpoint faster.

        // Propagate function (excluding A-B)
        L(succ, x; v) :- Edge(pred, succ), L(pred, x; v), if pred != "A".
    };
    query pr select (pp, x, v) from L(pp, x; v) |> println


