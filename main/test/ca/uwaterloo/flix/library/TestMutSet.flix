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

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool & Impure =
        let s = MutSet.empty(): MutSet[Int32];
        let s2 = MutSet.empty(): MutSet[Int32];
        MutSet.sameElements(s, s2)

    @test
    def sameElements02(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, s);

        let s2 = MutSet.empty();
        MutSet.add!(1, s2);

        MutSet.sameElements(s, s2)

    @test
    def sameElements03(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, s);

        let s2 = MutSet.empty();
        MutSet.add!(2, s2);

        not MutSet.sameElements(s, s2)

    @test
    def sameElements04(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);

        let s2 = MutSet.empty();
        MutSet.add!(2, s2);
        MutSet.add!(3, s2);
        MutSet.add!(1, s2);

        MutSet.sameElements(s, s2)

    @test
    def sameElements05(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!("a", s);
        MutSet.add!("b", s);
        MutSet.add!("c", s);

        let s2 = MutSet.empty();
        MutSet.add!("c", s2);
        MutSet.add!("a", s2);
        MutSet.add!("b", s2);

        MutSet.sameElements(s, s2)

    @test
    def setSameElements06(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!("a", s);
        MutSet.add!("b", s);
        MutSet.add!("c", s);

        let s2 = MutSet.empty();
        MutSet.add!("c", s2);
        MutSet.add!("a", s2);

        not MutSet.sameElements(s, s2)

}
