/*
 *  Copyright 2022 Nina Andrup Pedersen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Down {

    ///
    /// The `Down` type allows you to reverse the sort order of `a` conveniently.
    ///
    pub enum Down[a] {
        case Down(a)
    }

    instance Eq[Down[a]] with Eq[a] {
        pub def eq(x: Down[a], y: Down[a]): Bool = match (x, y) {
            case (Down.Down(xx), Down.Down(yy)) => xx `Eq.eq` yy
        }
    }

    instance Coerce[Down[a]] {
        type Out = a
        pub def coerce(x: Down[a]): a = match x {
            case Down.Down(xx) => xx
        }
    }

    instance ToString[Down[a]] with ToString[a] {
        pub def toString(x: Down[a]): String = match x {
            case Down.Down(xx) => ToString.toString(xx)
        }
    }

    instance Add[Down[a]] with Add[a] {
        pub def add(x: Down[a], y: Down[a]): Down[a] = match (x, y) {
            case (Down.Down(xx), Down.Down(yy)) => Down.Down(xx + yy) // no consideration for overflow
        }
    }

    instance PartialOrder[Down[a]] with PartialOrder[a] {
        pub def lessEqual(x: Down[a], y: Down[a]): Bool = match (x, y) {
            case (Down.Down(xx), Down.Down(yy)) => yy `PartialOrder.lessEqual` xx
        }
    }

    instance LowerBound[Down[a]] with UpperBound[a] {
        pub def minValue(): Down[a] = Down.Down(UpperBound.maxValue())
    }

    instance UpperBound[Down[a]] with LowerBound[a] {
        pub def maxValue(): Down[a] = Down.Down(LowerBound.minValue())
    }

    instance JoinLattice[Down[a]] with MeetLattice[a] {
        pub def leastUpperBound(x: Down[a], y: Down[a]): Down[a] = match (x, y) {
            case (Down.Down(xx), Down.Down(yy)) => Down.Down(xx `MeetLattice.greatestLowerBound` yy)
        }
    }

    instance MeetLattice[Down[a]] with JoinLattice[a] {
        pub def greatestLowerBound(x: Down[a], y: Down[a]): Down[a] = match (x, y) {
            case (Down.Down(xx), Down.Down(yy)) => Down.Down(xx `JoinLattice.leastUpperBound` yy)
        }
    }

    instance Order[Down[a]] with Order[a] {
        pub def compare(x: Down[a], y: Down[a]): Comparison = match (x, y) {
            case (Down.Down(xx), Down.Down(yy)) => yy <=> xx
        }
    }

}
