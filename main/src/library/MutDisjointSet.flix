/*
 *  Copyright 2023 Jakob Schneider Villumsen
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

pub enum MutDisjointSet[t: Type, r: Eff]({
    rc = Region[r],
    forest = MutList[MutDisjointSet.Node[t, r], r]
})

mod MutDisjointSet {

    pub enum Node[t: Type, r: Eff]({
        rc = Region[r],
        value = t,
        parent = Ref[Option[Node[t, r]], r],
        rank = Ref[Int32, r]
    })

    instance Eq[Node[t, r]] with Eq[t] {
        pub def eq(x: Node[t, r], y: Node[t, r]): Bool = {
            let Node.Node({ value = xval | _ }) = x;
            let Node.Node({ value = yval | _ }) = y;
            Eq.eq(xval, yval)
        }
    }

    instance Order[Node[t, r]] with Order[t] {
        pub def compare(x: Node[t, r], y: Node[t, r]): Comparison = {
            let Node.Node({ value = xval | _ }) = x;
            let Node.Node({ value = yval | _ }) = y;
            Order.compare(xval, yval)
        }
    }

    ///
    /// Returns a new empty `MutDisjointSet` in the region `rc`.
    ///
    pub def new(rc: Region[r]): MutDisjointSet[t, r] \ r =
        MutDisjointSet({ rc = rc, forest = MutList.new(rc) })

    ///
    /// Returns `true` iff `s` contains `x`.
    ///
    pub def contains(x: t, s: MutDisjointSet[t, r]): Bool \ r with Eq[t] = {
        // Speed up with binary search
        let MutDisjointSet({ forest | _ }) = s;
        match MutList.find(match Node.Node({ value | _ }) -> value == x, forest) {
            case Some(_) => true
            case None    => false
        }
    }

    ///
    /// Updates `s` with a new disjoint set containing `x`
    /// if `x` is not already in `s`.
    ///
    pub def makeSet(x: t, s: MutDisjointSet[t, r]): Unit \ r with Eq[t] = {
        let MutDisjointSet({ rc, forest }) = s;
        if (not contains(x, s)) {
            MutList.push!(singleton(rc, x), forest)
        } else {
            ()
        }
    }

    ///
    /// Updates `s` with collection of disjoint singleton sets containing
    /// the elements of the iterable `m`.
    ///
    pub def makeSets(m: m[t], s: MutDisjointSet[t, r]): Unit \ r with Iterable[m], Eq[t] = region rc {
        Iterable.iterator(rc, m) |> Iterator.forEach(flip(makeSet)(s))
    }

    ///
    /// Returns a new `Node` from `x` in `rc`.
    ///
    def singleton(rc: Region[r], x: t): Node[t, r] \ r =
        Node.Node({ rc = rc, value = x, parent = Ref.new(rc, None), rank = Ref.new(rc, 0) })

    ///
    /// Returns the representative / root of the set that contains `x`.
    ///
    pub def find(x: t, s: MutDisjointSet[t, r]): Option[Node[t, r]] \ r with Eq[t] = {
        def findInternal(y) = {
            let Node.Node({ parent | _ }) = y;
            match Ref.get(parent) {
                case Some(p) => {
                    let root = findInternal(p);
                    Ref.put(Some(root), parent);
                    root
                }
                case None => y
            }
        };
        let MutDisjointSet({ forest | _ }) = s;
        // Use binary search for speed up
        MutList.find(match Node.Node({ value | _ }) -> value == x, forest)
        |> Option.map(findInternal)
    }

    ///
    /// Merges the sets that contain `x` and `y`.
    ///
    pub def union(x: t, y: t, s: MutDisjointSet[t, r]): Unit \ r with Eq[t] =
        def link(x1, y1) = {
            let Node.Node({ parent = yparent, rank = yr | _ }) = y1;
            let Node.Node({ parent = xparent, rank = xr | _ }) = x1;
            let xrank = Ref.get(xr);
            let yrank = Ref.get(yr);
            if (xrank > yrank) {
                Ref.put(Some(x1), yparent)
            } else {
                Ref.put(Some(y1), xparent)
            };
            if (xrank == yrank) {
                Ref.put(yrank + 1, yr)
            } else {
                ()
            }
        };
        match (find(x, s), find(y, s)) {
            case (Some(xn), Some(yn)) => link(xn, yn)
            case _ => ()
        }

    pub def equivalent(x: MutDisjointSet[t, r], y: MutDisjointSet[t, r]): Bool \ {} = ???

}
