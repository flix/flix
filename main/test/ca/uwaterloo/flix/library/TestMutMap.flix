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
        let m = MutMap.empty(): MutMap[Int32, Int32];
        let m2 = MutMap.empty(): MutMap[Int32, Int32];

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


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool & Impure =
        MutMap.empty() |> MutMap.sumValues == 0

    @test
    def sumValues02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |>
            MutMap.sumValues == 1

    @test
    def sumValues03(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toMutMap |>
            MutMap.sumValues == 6

    @test
    def sumValues04(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toMutMap |>
            MutMap.sumValues == 3

    @test
    def sumValues05(): Bool & Impure =
        Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> Map.toMutMap |>
            MutMap.sumValues == -10

    @test
    def sumValues06(): Bool & Impure =
        Map#{10 => 10, -10 => -10} |> Map.toMutMap |>
            MutMap.sumValues == 0

    @test
    def sumValues07(): Bool & Impure =
        List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |> Map.toMutMap |>
            MutMap.sumValues == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool & Impure =
        MutMap.empty() |> MutMap.sumKeys == 0

    @test
    def sumKeys02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |>
            MutMap.sumKeys == 1

    @test
    def sumKeys03(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toMutMap |>
            MutMap.sumKeys == 6

    @test
    def sumKeys04(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toMutMap |>
            MutMap.sumKeys == 3

    @test
    def sumKeys05(): Bool & Impure =
        Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4} |> Map.toMutMap |>
            MutMap.sumKeys == -12

    @test
    def sumKeys06(): Bool & Impure =
        Map#{10 => 10, -10 => -10} |> Map.toMutMap |>
            MutMap.sumKeys == 0

    @test
    def sumKeys07(): Bool & Impure =
        List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |> Map.toMutMap |>
            MutMap.sumKeys == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool & Impure =
        MutMap.empty() |> MutMap.sumWith((k, v) -> k + v) == 0

    @test
    def sumWith02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |>
            MutMap.sumWith((k, v) -> k + v) == 2

    @test
    def sumWith03(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toMutMap |>
            MutMap.sumWith((k, v) -> k + v) == 12

    @test
    def sumWith04(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toMutMap |>
            MutMap.sumWith((k, v) -> k + v) == 6

    @test
    def sumWith05(): Bool & Impure =
        Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> Map.toMutMap |>
            MutMap.sumWith((k, v) -> k + v) == -20

    @test
    def sumWith06(): Bool & Impure =
        Map#{10 => 10, -10 => -10} |> Map.toMutMap |>
            MutMap.sumWith((k, v) -> k + v) == 0


    /////////////////////////////////////////////////////////////////////////////
    // productValues                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productValues01(): Bool & Impure =
        MutMap.empty() |> MutMap.productValues == 0

    @test
    def productValues02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |>
            MutMap.productValues == 1

    @test
    def productValues03(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toMutMap |>
            MutMap.productValues == 6

    @test
    def productValues04(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toMutMap |>
            MutMap.productValues == -18

    @test
    def productValues05(): Bool & Impure =
        Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4} |> Map.toMutMap |>
            MutMap.productValues == 24

    @test
    def productValues06(): Bool & Impure =
        Map#{10 => 10, -10 => -10} |> Map.toMutMap |>
            MutMap.productValues == -100


    /////////////////////////////////////////////////////////////////////////////
    // productKeys                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productKeys01(): Bool & Impure =
        MutMap.empty() |> MutMap.productValues == 0

    @test
    def productKeys02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |>
            MutMap.productKeys == 1

    @test
    def productKeys03(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toMutMap |>
            MutMap.productKeys == 6

    @test
    def productKeys04(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toMutMap |>
            MutMap.productKeys == -18

    @test
    def productKeys05(): Bool & Impure =
        Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4} |> Map.toMutMap |>
            MutMap.productKeys == 40

    @test
    def productKeys06(): Bool & Impure =
        Map#{10 => 10, -10 => -10} |> Map.toMutMap |>
            MutMap.productKeys == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool & Impure =
        MutMap.empty() |> MutMap.productWith((k, v) -> k + v) == 0

    @test
    def productWith02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |>
            MutMap.productWith((k, v) -> k + v) == 2

    @test
    def productWith03(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toMutMap |>
            MutMap.productWith((k, v) -> k + v) == 48

    @test
    def productWith04(): Bool & Impure =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toMutMap |>
            MutMap.productWith((k, v) -> k + v) == -288

    @test
    def productWith05(): Bool & Impure =
        Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> Map.toMutMap |>
            MutMap.productWith((k, v) -> k + v) == 384

    @test
    def productWith06(): Bool & Impure =
        Map#{10 => 10, -10 => -10} |> Map.toMutMap |>
            MutMap.productWith((k, v) -> k + v) == -400


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinKeys01(): Bool & Impure =
        MutMap.empty(): MutMap[Int32, Int32] |> MutMap.joinKeys(",") == ""

    @test
    def joinKeys02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |> MutMap.joinKeys(",") == "1"

    @test
    def joinKeys03(): Bool & Impure =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.toMutMap |> MutMap.joinKeys(",") == "0,1,2"

    @test
    def joinKeys04(): Bool & Impure =
        Map#{"0" => 1, "1" => 2, "2" => 2} |> Map.toMutMap |> MutMap.joinKeys(",") == "0,1,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinValues01(): Bool & Impure =
        MutMap.empty(): MutMap[Int32, Int32] |> MutMap.joinValues(",") == ""

    @test
    def joinValues02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |> MutMap.joinValues(",") == "1"

    @test
    def joinValues03(): Bool & Impure =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.toMutMap |> MutMap.joinValues(",") == "1,2,2"

    @test
    def joinValues04(): Bool & Impure =
        Map#{0 => "1", 1 => "2", 2 => "2"} |> Map.toMutMap |> MutMap.joinValues(",") == "1,2,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool & Impure =
        Map.empty(): Map[Int32, Int32] |> Map.toMutMap |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ",") == ""

    @test
    def joinWith02(): Bool & Impure =
        Map#{1 => 1} |> Map.toMutMap |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "1 => 1"

    @test
    def joinWith03(): Bool & Impure =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.toMutMap |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"

    @test
    def joinWith04(): Bool & Impure =
        Map#{0 => "1", 1 => "2", 2 => "2"} |> Map.toMutMap |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"

}
