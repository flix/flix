/*
 *  Copyright 2022 Nina Andrup Pedersen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// The `Down` type allows you to reverse the sort order of `a` conveniently.
///
pub enum Down[a] with Sendable {
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
