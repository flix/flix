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

pub enum MutDisjointSet[t: Type, r: Eff] {
    case Node({ rc = Region[r], value = t, parent = Ref[Option[MutDisjointSet[t, r]], r], rank = Ref[Int32, r] })
}

mod MutDisjointSet {

    ///
    ///
    ///
    pub def makeSet(rc: Region[r], x: m[t]): MutDisjointSet[t, r] with Iterable[m] = ???

    ///
    ///
    ///
    pub def singleton(rc: Region[r], x: t): MutDisjointSet[t, r] \ r =
        Node({ rc = rc, value = x, parent = Ref.new(rc, None), rank = Ref.new(rc, 0) })

    ///
    /// Returns the representative of the set that contains x.
    ///
    pub def find(x: MutDisjointSet[t, r]): MutDisjointSet[t, r] \ r =
        let Node({ parent | _ }) = x;
        match Ref.get(parent) {
            case Some(p) => {
                let root = find(p);
                Ref.put(Some(root), parent);
                root
            }
            case None => x
        }

    ///
    ///
    ///
    pub def union(x: MutDisjointSet[t, r], y: MutDisjointSet[t, r]): Unit \ r =
        def link(x1, y1) = {
            let Node({ parent = xparent, rank = xr | _ }) = x1;
            let Node({ parent = yparent, rank = yr | _ }) = y1;
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
