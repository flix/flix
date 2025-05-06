/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
mod Fixpoint.Counter {
    pub type alias Counter[r: Eff] = Ref[Int32, r]
    pub def fresh(rc: Region[r]): Counter[r] \ r = Ref.fresh(rc, 0)

    pub def getNewID(c: Counter[r]): Int32 \ r =
        let newID = Ref.get(c);
        Ref.put(newID + 1, c);
        newID

    pub def resetTo(i: Int32, c: Counter[r]): Unit \ r =
        Ref.put(i, c)

    pub def increment(c: Counter[r]): Unit \ r =
        Ref.put(Ref.get(c) + 1, c);
        ()

    pub def peek(c: Counter[r]): Int32 \ r =
        Ref.get(c)

    pub def clone(c: Counter[r1], rc: Region[r2]): Counter[r2] \ {r1, r2} =
        Ref.fresh(rc, Ref.get(c))

    pub def ++(c: Counter[r]): Int32 \ r = getNewID(c)

}