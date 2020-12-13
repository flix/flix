/*
 * Copyright 2020 Esben Bjerre
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

namespace TestMutMap {

use ToString.toString;

/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toString01(): Bool & Impure =
    let s = MutMap.empty();
    MutMap.put!(1, 45, s);
    toString(s) == "MutMap(1 -> 45)"

@test
def toString02(): Bool & Impure =
    let s = MutMap.empty();
    MutMap.put!(1, 3, s);
    MutMap.put!(2, 4, s);
    toString(s) == "MutMap(1 -> 3, 2 -> 4)"

@test
def toString03(): Bool & Impure =
    let s = MutMap.empty();
    MutMap.put!(1, 'a', s);
    MutMap.put!(92, 'b', s);
    MutMap.put!(94, 'c', s);
    MutMap.put!(97, 'd', s);
    toString(s) == "MutMap(1 -> a, 92 -> b, 94 -> c, 97 -> d)"

@test
def toString04(): Bool & Impure =
    let s = MutMap.empty();
    MutMap.put!('a', 6, s);
    MutMap.put!('b', 7, s);
    MutMap.put!('c', 10, s);
    MutMap.put!('d', 1337, s);
    toString(s) == "MutMap(a -> 6, b -> 7, c -> 10, d -> 1337)"

}