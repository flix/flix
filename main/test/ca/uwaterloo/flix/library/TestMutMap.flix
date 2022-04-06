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
    def sameElements01(): Bool = region r {
        let m1 = new MutMap(r): MutMap[Int32, Int32, _];
        let m2 = new MutMap(r): MutMap[Int32, Int32, _];
        MutMap.sameElements(m1, m2)
    }

    @test
    def sameElements02(): Bool = region r {
        let m = new MutMap(r);
        MutMap.put!(1, "a", m);

        let m2 = new MutMap(r);
        MutMap.put!(1, "a", m2);

        MutMap.sameElements(m, m2)
    }

    @test
    def sameElements03(): Bool = region r {
        let m = new MutMap(r);
        MutMap.put!(1, "a", m);
        MutMap.put!(1, "b", m);

        let m2 = new MutMap(r);
        MutMap.put!(1, "a", m2);

        not MutMap.sameElements(m, m2)
    }

    @test
    def sameElements04(): Bool = region r {
        let m = new MutMap(r);
        MutMap.put!(1, "a", m);
        MutMap.put!(2, "b", m);

        let m2 = new MutMap(r);
        MutMap.put!(1, "a", m2);

        not MutMap.sameElements(m, m2)
    }

    @test
    def sameElements05(): Bool = region r {
        let m = new MutMap(r);
        MutMap.put!(1, "a", m);

        let m2 = new MutMap(r);
        MutMap.put!(1, "a", m2);
        MutMap.put!(2, "b", m2);

        not MutMap.sameElements(m, m2)
    }

    @test
    def sameElements06(): Bool = region r {
        let m = new MutMap(r);
        MutMap.put!(2, "b", m);
        MutMap.put!(3, "c", m);
        MutMap.put!(1, "a", m);

        let m2 = new MutMap(r);
        MutMap.put!(1, "a", m2);
        MutMap.put!(2, "b", m2);
        MutMap.put!(3, "c", m2);

        MutMap.sameElements(m, m2)
    }

    @test
    def sameElements07(): Bool & Impure =
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
    // mergeWithKey                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def merge01(): Bool = region r {
        let m1 = new MutMap(r): MutMap[Int32, Int32, _];
        let m2 = new MutMap(r): MutMap[Int32, Int32, _];
        MutMap.mergeWithKey!((_, v, _) -> v, m1, m2);
        MutMap.isEmpty(m2)
    }

    @test
    def merge02(): Bool = region r {
        let m1 = new MutMap(r);
        let m2 = new MutMap(r);
        let _ = MutMap.put!(1, 0, m1);
        let _ = MutMap.put!(2, 0, m2);
        MutMap.mergeWithKey!((_, v, _) -> v, m1, m2);
        MutMap.memberOf(1, m2) and MutMap.memberOf(2, m2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool = region r {
        new MutMap(r) |> MutMap.sumValues == 0
    }

    @test
    def sumValues02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |>
            MutMap.sumValues == 1
    }

    @test
    def sumValues03(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.sumValues == 6
    }

    @test
    def sumValues04(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.sumValues == 3
    }

    @test
    def sumValues05(): Bool = region r {
        MutMap(ref Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap.sumValues == -10
    }

    @test
    def sumValues06(): Bool = region r {
        MutMap(ref Map#{10 => 10, -10 => -10}) |>
            MutMap.sumValues == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool = region r {
        new MutMap(r) |> MutMap.sumKeys == 0
    }

    @test
    def sumKeys02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |>
            MutMap.sumKeys == 1
    }

    @test
    def sumKeys03(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.sumKeys == 6
    }

    @test
    def sumKeys04(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.sumKeys == 3
    }

    @test
    def sumKeys05(): Bool = region r {
        MutMap(ref Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4}) |>
            MutMap.sumKeys == -12
    }

    @test
    def sumKeys06(): Bool = region r {
        MutMap(ref Map#{10 => 10, -10 => -10}) |>
            MutMap.sumKeys == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        new MutMap(r) |> MutMap.sumWith((k, v) -> k + v) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |>
            MutMap.sumWith((k, v) -> k + v) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.sumWith((k, v) -> k + v) == 12
    }

    @test
    def sumWith04(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.sumWith((k, v) -> k + v) == 6
    }

    @test
    def sumWith05(): Bool = region r {
        MutMap(ref Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap.sumWith((k, v) -> k + v) == -20
    }

    @test
    def sumWith06(): Bool = region r {
        MutMap(ref Map#{10 => 10, -10 => -10}) |>
            MutMap.sumWith((k, v) -> k + v) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // productValues                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productValues01(): Bool = region r {
        new MutMap(r) |> MutMap.productValues == 0
    }

    @test
    def productValues02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |>
            MutMap.productValues == 1
    }

    @test
    def productValues03(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.productValues == 6
    }

    @test
    def productValues04(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.productValues == -18
    }

    @test
    def productValues05(): Bool = region r {
        MutMap(ref Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4}) |>
            MutMap.productValues == 24
    }

    @test
    def productValues06(): Bool = region r {
        MutMap(ref Map#{10 => 10, -10 => -10}) |>
            MutMap.productValues == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productKeys                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productKeys01(): Bool = region r {
        new MutMap(r) |> MutMap.productValues == 0
    }

    @test
    def productKeys02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |>
            MutMap.productKeys == 1
    }

    @test
    def productKeys03(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.productKeys == 6
    }

    @test
    def productKeys04(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.productKeys == -18
    }

    @test
    def productKeys05(): Bool = region r {
        MutMap(ref Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4}) |>
            MutMap.productKeys == 40
    }

    @test
    def productKeys06(): Bool = region r {
        MutMap(ref Map#{10 => 10, -10 => -10}) |>
            MutMap.productKeys == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        new MutMap(r) |> MutMap.productWith((k, v) -> k + v) == 0
    }

    @test
    def productWith02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |>
            MutMap.productWith((k, v) -> k + v) == 2
    }

    @test
    def productWith03(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.productWith((k, v) -> k + v) == 48
    }

    @test
    def productWith04(): Bool = region r {
        MutMap(ref Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.productWith((k, v) -> k + v) == -288
    }

    @test
    def productWith05(): Bool = region r {
        MutMap(ref Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap.productWith((k, v) -> k + v) == 384
    }

    @test
    def productWith06(): Bool = region r {
        MutMap(ref Map#{10 => 10, -10 => -10}) |>
            MutMap.productWith((k, v) -> k + v) == -400
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinKeys01(): Bool = region r {
        new MutMap(r): MutMap[Int32, Int32, _] |> MutMap.joinKeys(",") == ""
    }

    @test
    def joinKeys02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |> MutMap.joinKeys(",") == "1"
    }

    @test
    def joinKeys03(): Bool = region r {
        MutMap(ref Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap.joinKeys(",") == "0,1,2"
    }

    @test
    def joinKeys04(): Bool = region r {
        MutMap(ref Map#{"0" => 1, "1" => 2, "2" => 2}) |> MutMap.joinKeys(",") == "0,1,2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinValues01(): Bool = region r {
        new MutMap(r): MutMap[Int32, Int32, _] |> MutMap.joinValues(",") == ""
    }

    @test
    def joinValues02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |> MutMap.joinValues(",") == "1"
    }

    @test
    def joinValues03(): Bool = region r {
        MutMap(ref Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap.joinValues(",") == "1,2,2"
    }

    @test
    def joinValues04(): Bool = region r {
        MutMap(ref Map#{0 => "1", 1 => "2", 2 => "2"}) |> MutMap.joinValues(",") == "1,2,2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        new MutMap(r): MutMap[Int32, Int32, _] |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region r {
        MutMap(ref Map#{1 => 1}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "1 => 1"
    }

    @test
    def joinWith03(): Bool = region r {
        MutMap(ref Map#{0 => 1, 1 => 2, 2 => 2}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"
    }

    @test
    def joinWith04(): Bool = region r {
        MutMap(ref Map#{0 => "1", 1 => "2", 2 => "2"}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightLazy                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightLazy01(): Bool = region r {
        MutMap.foldRightLazy((v, s) -> force s + v, 0, new MutMap(r)) == 0
    }

    @test
    def foldRightLazy02(): Bool = region r {
        let m = new MutMap(r);
        MutMap.put!(1, 2, m);
        MutMap.foldRightLazy((v, s) -> force s + v, 0, m) == 2
    }

    @test
    def foldRightLazy03(): Bool = region r {
        let m = MutMap(ref Map#{1 => 2, 3 => 4});
        MutMap.foldRightLazy((v, s) -> force s + v, 0, m) == 6
    }

    @test
    def foldRightLazy04(): Bool = region r {
        let m = MutMap(ref Map#{1 => 2, 3 => 4, 5 => 6});
        MutMap.foldRightLazy((v, s) -> force s + v, 0, m) == 12
    }
}
