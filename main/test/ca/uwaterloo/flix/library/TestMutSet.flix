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

namespace TestMutSet {

    use ToString.toString;

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, s);
        toString(s) == "MutSet#{1}"

    @test
    def toString02(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        toString(s) == "MutSet#{1, 2}"

    @test
    def toString03(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, s);
        MutSet.add!(92, s);
        MutSet.add!(94, s);
        MutSet.add!(97, s);
        toString(s) == "MutSet#{1, 92, 94, 97}"

    @test
    def toString04(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!('a', s);
        MutSet.add!('b', s);
        MutSet.add!('c', s);
        MutSet.add!('d', s);
        toString(s) == "MutSet#{a, b, c, d}"

}