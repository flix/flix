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


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool & Impure =
        MutSet.empty() |> MutSet.sum == 0

    @test
    def sum02(): Bool & Impure =
        Set#{1} |> Set.toMutSet |> MutSet.sum == 1

    @test
    def sum03(): Bool & Impure =
        Set#{1, 2, 3} |> Set.toMutSet |> MutSet.sum == 6

    @test
    def sum04(): Bool & Impure =
        Set#{1, 2, 3, -3} |> Set.toMutSet |> MutSet.sum == 3

    @test
    def sum05(): Bool & Impure =
        Set#{1, 2, -3, -4} |> Set.toMutSet |> MutSet.sum == -4

    @test
    def sum06(): Bool & Impure =
        Set#{10, -10} |> Set.toMutSet |> MutSet.sum == 0

    @test
    def sum07(): Bool & Impure =
        Set.range(1, 101) |> Set.toMutSet |> MutSet.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool & Impure =
        MutSet.empty() |> MutSet.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool & Impure =
        Set#{1} |> Set.toMutSet |> MutSet.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool & Impure =
        Set#{1, 2, 3} |> Set.toMutSet |> MutSet.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool & Impure =
        Set#{1, 2, 3, -3} |> Set.toMutSet |> MutSet.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool & Impure =
        Set#{-1, -2, -3, -4} |> Set.toMutSet |> MutSet.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool & Impure =
        Set#{10, -10} |> Set.toMutSet |> MutSet.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool & Impure =
        MutSet.product(MutSet.empty()) == 0

    @test
    def product02(): Bool & Impure =
        Set#{1} |> Set.toMutSet |> MutSet.product == 1

    @test
    def product03(): Bool & Impure =
        Set#{1, 2, 3} |> Set.toMutSet |> MutSet.product == 6

    @test
    def product04(): Bool & Impure =
        Set#{1, 2, 3, -3} |> Set.toMutSet |> MutSet.product == -18

    @test
    def product05(): Bool & Impure =
        Set#{-1, -2, -3, -4} |> Set.toMutSet |> MutSet.product == 24

    @test
    def product06(): Bool & Impure =
        Set#{10, -10} |> Set.toMutSet |> MutSet.product == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool & Impure =
        MutSet.empty() |> MutSet.productWith(x -> x + 1) == 0

    @test
    def productWith02(): Bool & Impure =
        Set#{1} |> Set.toMutSet |> MutSet.productWith(x -> x + 1) == 2

    @test
    def productWith03(): Bool & Impure =
        Set#{1, 2, 3} |> Set.toMutSet |> MutSet.productWith(x -> x + 1) == 24

    @test
    def productWith04(): Bool & Impure =
        Set#{1, 2, 3, -3} |> Set.toMutSet |> MutSet.productWith(x -> x + 1) == -48

    @test
    def productWith05(): Bool & Impure =
        Set#{-2, -3, -4, -5} |> Set.toMutSet |> MutSet.productWith(x -> x + 1) == 24

    @test
    def productWith06(): Bool & Impure =
        Set#{10, -10} |> Set.toMutSet |> MutSet.productWith(x -> x + 1) == -99


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool & Impure =
        MutSet.join(",", MutSet.empty(): MutSet[Int32]) == ""

    @test
    def join02(): Bool & Impure =
        Set#{1} |> Set.toMutSet |> MutSet.join(",") == "1"

    @test
    def join03(): Bool & Impure =
        Set#{1, 2, 3} |> Set.toMutSet |> MutSet.join(",") == "1,2,3"

    @test
    def join04(): Bool & Impure =
        Set#{"1", "2", "3"} |> Set.toMutSet |> MutSet.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // foldRightLazy                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def foldRightLazy01(): Bool & Impure = MutSet.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, MutSet.empty()) == 100

    @test
    def foldRightLazy02(): Bool & Impure = MutSet.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, MutSet.singleton(1)) == 198

    @test
    def foldRightLazy03(): Bool & Impure =
        let s = MutSet(ref Set#{2, 1});
        MutSet.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, s) == 194

    @test
    def foldRightLazy04(): Bool & Impure =
        let s = MutSet(ref Set#{3, 2, 1});
        MutSet.foldRightLazy((e, i) -> (force i - e) * (e rem 2 + 1), 100, s) == 382

}
