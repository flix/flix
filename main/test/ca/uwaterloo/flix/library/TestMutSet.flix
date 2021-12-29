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
        let s = MutSet.empty();
        let s2 = MutSet.empty();
        MutSet.sameElements(d, d2)

    @test
    def sameElements02(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, d);

        let s2 = MutSet.empty();
        MutSet.add!(1, d2);

        MutSet.sameElements(d, d2)

    @test
    def sameElements03(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, d);

        let s2 = MutSet.empty();
        MutSet.add!(2, d2);

        not MutSet.sameElements(d, d2)

    @test
    def sameElements04(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!(1, d);
        MutSet.add!(2, d);
        MutSet.add!(3, d);

        let s2 = MutSet.empty();
        MutSet.add!(2, d2);
        MutSet.add!(3, d2);
        MutSet.add!(1, d2);

        MutSet.sameElements(d, d2)

    @test
    def sameElements05(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!("a", d);
        MutSet.add!("b", d);
        MutSet.add!("c", d);

        let s2 = MutSet.empty();
        MutSet.add!("c", d2);
        MutSet.add!("a", d2);
        MutSet.add!("b", d2);

        MutSet.sameElements(d, d2)

    @test
    def setSameElements06(): Bool & Impure =
        let s = MutSet.empty();
        MutSet.add!("a", d);
        MutSet.add!("b", d);
        MutSet.add!("c", d);

        let s2 = MutSet.empty();
        MutSet.add!("c", d2);
        MutSet.add!("a", d2);

        not MutSet.sameElements(d, d2)

}
