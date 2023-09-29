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
    nodes = Vector[(t, MutDisjointSet.Node[t, r])]
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
    /// Returns the node in the set `s` that contains `x`.
    /// Returns `None` if `x` is not in `s`.
    ///
    pub def node(x: t, s: MutDisjointSet[t, r]): Option[Node[t, r]] with Eq[t] = {
        let MutDisjointSet({ nodes | _ }) = s;
        Vector.find(fst >> Eq.eq(x), nodes)
        |> Option.map(snd)
    }

    ///
    /// Returns a new collection of disjoint singletong sets containing
    /// the elements of the iterable `m` in the region `rc`.
    ///
    pub def makeSet(rc: Region[r], m: m[t]): MutDisjointSet[t, r] \ r with Iterable[m], Order[t] = {
        let nodes =
            foreach (
                x <- m
            ) yield (x, singleton(rc, x));
        MutDisjointSet({ rc = rc, nodes = nodes })
    }

    ///
    /// Returns a new `Node` from `x` in `rc`.
    ///
    def singleton(rc: Region[r], x: t): Node[t, r] \ r =
        Node.Node({ rc = rc, value = x, parent = Ref.new(rc, None), rank = Ref.new(rc, 0) })

    ///
    /// Returns the representative / root of the set that contains `x`.
    ///
    pub def find(x: Node[t, r]): Node[t, r] \ r =
        let Node.Node({ parent | _ }) = x;
        match Ref.get(parent) {
            case Some(p) => {
                let root = find(p);
                Ref.put(Some(root), parent);
                root
            }
            case None => x
        }

    ///
    /// Merges the sets that contain `x` and `y`.
    ///
    pub def union(x: Node[t, r], y: Node[t, r]): Unit \ r =
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
        link(find(x), find(y))

    pub def equivalent(x: MutDisjointSet[t, r], y: MutDisjointSet[t, r]): Bool \ {} = ???

}
