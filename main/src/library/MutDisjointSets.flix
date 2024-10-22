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

///
/// Represents a mutable disjoint set data structure
/// using union-by-rank and path compression.
///
pub enum MutDisjointSets[t: Type, r: Eff]({
    rc = Region[r],
    forest = MutMap[t, MutDisjointSets.Node[t, r], r]
})

mod MutDisjointSets {

    ///
    /// Represents an element in the disjoint set forest.
    ///
    @Internal
    pub enum Node[t: Type, r: Eff]({
        rc = Region[r],
        value = t,
        parent = Ref[Option[Node[t, r]], r],
        rank = Ref[Int32, r]
    })

    ///
    /// Returns new and empty `MutDisjointSets` in the region `rc`.
    ///
    pub def empty(rc: Region[r]): MutDisjointSets[t, r] \ r =
        MutDisjointSets({ rc = rc, forest = MutMap.empty(rc) })

    ///
    /// Updates `s` with a new disjoint set containing `x`
    /// if `x` is not already in `s`.
    ///
    pub def makeSet(x: t, s: MutDisjointSets[t, r]): Unit \ r with Order[t] = {
        def singleton(rc, y) = {
            Node.Node({ rc = rc, value = y, parent = Ref.fresh(rc, None), rank = Ref.fresh(rc, 0) })
        };
        let MutDisjointSets({ rc, forest }) = s;
        if (not memberOf(x, s)) {
            MutMap.put!(x, singleton(rc, x), forest)
        } else {
            ()
        }
    }

    ///
    /// Updates `s` with the elements of the iterable `m`.
    ///
    pub def makeSets(m: m, s: MutDisjointSets[elt, r]): Unit \ (r + Iterable.Aef[m]) with Iterable[m], Order[elt] where Iterable.Elm[m] ~ elt = region rc {
        Iterable.iterator(rc, m) |> Iterator.forEach(x -> makeSet(x, s))
    }

    ///
    /// Returns `true` iff `x` is a member of `s`.
    ///
    pub def memberOf(x: t, s: MutDisjointSets[t, r]): Bool \ r with Order[t] = {
        let MutDisjointSets({ forest | _ }) = s;
        MutMap.memberOf(x, forest)
    }

    ///
    /// Returns the representative / root of the set that contains `x`.
    ///
    pub def find(x: t, s: MutDisjointSets[t, r]): Option[t] \ r with Order[t] =
        findSet(x, s) |> Option.map(match Node.Node({ value | _ }) -> value)

    ///
    /// Returns the representative / root `Node` of the set that contains `x`.
    ///
    def findSet(x: t, s: MutDisjointSets[t, r]): Option[Node[t, r]] \ r with Order[t] = {
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
        let MutDisjointSets({ forest | _ }) = s;
        MutMap.get(x, forest)
        |> Option.map(findRoot)
    }

    ///
    /// Merges the sets that contain `x` and `y` if both `x` and `y` are in `s`.
    /// If `x` and `y` have the same rank, then `y` becomes the new parent of `x`.
    /// Otherwise, the element with the highest rank becomes the new representative.
    ///
    /// The rank of an element is determined by how many times it is made
    /// the new representative after a union with another element of the
    /// same rank.
    ///
    /// Further reading:
    /// https://en.wikipedia.org/wiki/Disjoint-set_data_structure
    ///
    pub def union(x: t, y: t, s: MutDisjointSets[t, r]): Unit \ r with Order[t] =
        def link(x1, y1) = {
            if (not equal(x1, y1)) {
                let Node.Node({ parent = xParent, rank = xr | _ }) = x1;
                let Node.Node({ parent = yParent, rank = yr | _ }) = y1;
                let xRank = Ref.get(xr);
                let yRank = Ref.get(yr);
                if (xRank > yRank) {
                    Ref.put(Some(x1), yParent)
                } else {
                    Ref.put(Some(y1), xParent)
                };
                if (xRank == yRank) {
                    Ref.put(yRank + 1, yr)
                } else {
                    ()
                }
            }
            else {
                ()
            }
        };
        match (findSet(x, s), findSet(y, s)) {
            case (Some(xn), Some(yn)) => link(xn, yn)
            case (Some(_), None) => {
                makeSet(y, s);
                union(x, y, s)
            }
            case (None, Some(_)) => {
                makeSet(y, s);
                union(x, y, s)
            }
            case (None, None)     => {
                makeSet(x, s);
                makeSet(y, s);
                union(x, y, s)
            }
        }

    ///
    /// Returns true if `x` and `y` are in the same disjoint set.
    /// This is equivalent to having the same representative / root.
    /// Returns false if either `x` or `y` is not in `s`.
    ///
    pub def equivalent(x: t, y: t, s: MutDisjointSets[t, r]): Bool \ r with Order[t] =
        match (findSet(x, s), findSet(y, s)) {
            case (Some(xn), Some(yn)) => equal(xn, yn)
            case _                    => false
        }

    ///
    /// Returns true if `x` and `y` have the same value.
    ///
    def equal(x: Node[t, r], y: Node[t, r]): Bool with Eq[t] = match (x, y) {
        case (Node.Node({ value = xVal | _ }), Node.Node({ value = yVal | _ })) => xVal == yVal
    }

    ///
    /// Returns the number of elements in `s`.
    ///
    pub def size(s: MutDisjointSets[t, r]): Int32 \ r with Order[t] =
        let MutDisjointSets({ forest | _ }) = s;
        MutMap.size(forest)

}
