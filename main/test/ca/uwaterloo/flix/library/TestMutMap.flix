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

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool & Impure =
        let m = MutMap.empty();
        let m2 = MutMap.empty();

        MutMap.sameElements(m, m2)

    @test
    def sameElements02(): Bool & Impure =
        let m = MutMap.empty();
        MutMap.put!(1, "a", m);

        let m2 = MutMap.empty();
        MutMap.put!(1, "a", m2);

        MutMap.sameElements(m, m2)

    @test
    def sameElements03(): Bool & Impure =
        let m = MutMap.empty();
        MutMap.put!(1, "a", m);
        MutMap.put!(1, "b", m);

        let m2 = MutMap.empty();
        MutMap.put!(1, "a", m2);

        not MutMap.sameElements(m, m2)

    @test
    def sameElements04(): Bool & Impure =
        let m = MutMap.empty();
        MutMap.put!(1, "a", m);
        MutMap.put!(2, "b", m);

        let m2 = MutMap.empty();
        MutMap.put!(1, "a", m2);

        not MutMap.sameElements(m, m2)

    @test
    def sameElements05(): Bool & Impure =
        let m = MutMap.empty();
        MutMap.put!(1, "a", m);

        let m2 = MutMap.empty();
        MutMap.put!(1, "a", m2);
        MutMap.put!(2, "b", m2);

        not MutMap.sameElements(m, m2)

    @test
    def sameElements06(): Bool & Impure =
        let m = MutMap.empty();
        MutMap.put!(2, "b", m);
        MutMap.put!(3, "c", m);
        MutMap.put!(1, "a", m);

        let m2 = MutMap.empty();
        MutMap.put!(1, "a", m2);
        MutMap.put!(2, "b", m2);
        MutMap.put!(3, "c", m2);

        MutMap.sameElements(m, m2)

}
