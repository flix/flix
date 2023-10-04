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
    forest = MutMap[t, MutDisjointSet.Node[t, r], r]
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

    ///
    /// Returns a new empty `MutDisjointSet` in the region `rc`.
    ///
    pub def new(rc: Region[r]): MutDisjointSet[t, r] \ r =
        MutDisjointSet({ rc = rc, forest = MutMap.new(rc) })

    ///
    /// Updates `s` with a new disjoint set containing `x`
    /// if `x` is not already in `s`.
    ///
    pub def makeSet(x: t, s: MutDisjointSet[t, r]): Unit \ r with Order[t] = {
        let MutDisjointSet({ rc, forest }) = s;
        if (not contains(x, s)) {
            MutMap.put!(x, singleton(rc, x), forest)
        } else {
            ()
        }
    }

    ///
    /// Returns `true` iff `s` contains `x`.
    ///
    pub def contains(x: t, s: MutDisjointSet[t, r]): Bool \ r with Order[t] = {
        let MutDisjointSet({ forest | _ }) = s;
        match MutMap.get(x, forest) {
            case Some(_) => true
            case None    => false
        }
    }

    ///
    /// Returns a new `Node` from `x` in `rc`.
    ///
    def singleton(rc: Region[r], x: t): Node[t, r] \ r =
        Node.Node({ rc = rc, value = x, parent = Ref.new(rc, None), rank = Ref.new(rc, 0) })

    ///
    /// Updates `s` with collection of disjoint singleton sets containing
    /// the elements of the iterable `m`.
    ///
    pub def makeSets(m: m[t], s: MutDisjointSet[t, r]): Unit \ r with Iterable[m], Order[t] = region rc {
        Iterable.iterator(rc, m) |> Iterator.forEach(flip(makeSet)(s))
    }

    ///
    /// Returns the representative / root of the set that contains `x`.
    ///
    pub def find(x: t, s: MutDisjointSet[t, r]): Option[t] \ r with Order[t] =
        findSet(x, s) |> Option.map(match Node.Node({ value | _ }) -> value)

    ///
    /// Returns the representative / root `Node` of the set that contains `x`.
    ///
    def findSet(x: t, s: MutDisjointSet[t, r]): Option[Node[t, r]] \ r with Order[t] = {
        def findRoot(y) = {
            let Node.Node({ parent | _ }) = y;
            match Ref.get(parent) {
                case Some(p) => {
                    let root = findRoot(p);
                    Ref.put(Some(root), parent);
                    root
                }
                case None => y
            }
        };
        let MutDisjointSet({ forest | _ }) = s;
        MutMap.get(x, forest)
        |> Option.map(findRoot)
    }

    ///
    /// Merges the sets that contain `x` and `y` if both `x` and `y` are in `s`.
    /// If `x` and `y` have the same rank, then `y` becomes the new parent of `x`.
    ///
    pub def union(x: t, y: t, s: MutDisjointSet[t, r]): Unit \ r with Order[t] =
        def link(x1, y1) = {
            if (x1 == y1) {
                ()
            } else {
                let Node.Node({ parent = xparent, rank = xr | _ }) = x1;
                let Node.Node({ parent = yparent, rank = yr | _ }) = y1;
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
            }
        };
        match (findSet(x, s), findSet(y, s)) {
            case (Some(xn), Some(yn)) => link(xn, yn)
            case _ => ()
        }

    pub def equivalent(x: MutDisjointSet[t, r], y: MutDisjointSet[t, r]): Bool \ {} = ???

}
