/*
 * Copyright 2021 Jakob Schneider Villumsen
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


namespace TestDelayMap {

    /////////////////////////////////////////////////////////////////////////////
    // insertWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWith01(): Bool =
        DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 3, DelayMap.empty()) |>DelayMap.toList == (1, 3) :: Nil

    @test
    def insertWith02(): Bool =
        List.toDelayMap((1, 4) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 3) |> DelayMap.toList == (1, 7) :: Nil

    @test
    def insertWith03(): Bool =
        List.toDelayMap((1, 4) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 2, 3) |> DelayMap.toList == (1, 4) :: (2, 3) :: Nil

    @test
    def insertWith04(): Bool =
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 1) |> DelayMap.toList == (1, 5) :: (5, -2) :: Nil

    @test
    def insertWith05(): Bool =
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |>
        DelayMap.insertWith((v1, v2) -> v1 + v2, 5, 1) |> DelayMap.toList == (1, 4) :: (5, -1) :: Nil

    @test
    def insertWith06(): Bool =
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |>
        DelayMap.insertWith((v1, v2) -> v1 + v2, 4, -2) |>
        DelayMap.toList |> List.sortBy(t -> fst(t)) == (1, 4) :: (4, -2) :: (5, -2):: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWithKey insertWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWithInsertWith01(): Bool =
        (("a", 1) :: Nil)                                    |>
            List.toDelayMap                                  |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith02(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List.toDelayMap                                              |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3)             |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil
    @test
    def insertWithInsertWith03(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List.toDelayMap                                              |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1)             |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith04(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List.toDelayMap                                              |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWith insertWith fusion                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWithFusion01(): Bool & Impure =
        let l = ref Nil;
        List.toDelayMap((1, 1) :: Nil) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1);
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil

    @test
    def insertWithFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = List.toDelayMap((1, 1) :: Nil) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1);
        let _ = DelayMap.toMap(m);
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Map#{}: Map[Unit, Int32] |> Map.toDelayMap |> DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{}

    @test
    def mapPure02(): Bool =
        Map#{1 -> 4} |> Map.toDelayMap |> DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{1 -> 12}

    @test
    def mapPure03(): Bool =
        Map#{2 -> -1, 0 -> 0} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{2 -> -3, 0 -> 0}

    @test
    def mapPure04(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27}

    @test
    def mapPure05(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Map#{}: Map[Unit, Int32] |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v as & Impure) |> DelayMap.toMap == Map#{}

    @test
    def mapImpure02(): Bool & Impure =
        Map#{1 -> 4} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v as & Impure) |> DelayMap.toMap == Map#{1 -> 12}

    @test
    def mapImpure03(): Bool & Impure =
        Map#{2 -> -1, 0 -> 0} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v as & Impure) |> DelayMap.toMap == Map#{2 -> -3, 0 -> 0}

    @test
    def mapImpure04(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v as & Impure) |> DelayMap.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27}

    @test
    def mapImpure05(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v as & Impure) |> DelayMap.toMap == Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}

    @test
    def mapMap02(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> 3 * v as & Impure) |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}

    @test
    def mapMap03(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.map(v -> 3 * v as & Impure) |>
            DelayMap.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}

    @test
    def mapMap04(): Bool & Impure =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> 3 * v as & Impure) |>
            DelayMap.map(v -> 3 * v as & Impure) |>
            DelayMap.toMap == Map#{2 -> -9, 5 -> 135, 11 -> -81}


    /////////////////////////////////////////////////////////////////////////////
    // map (fusion)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List.toDelayMap |>
        DelayMap.map(v -> { l := "a" :: deref l; v }) |>
        DelayMap.map(v -> { l := "b" :: deref l; v });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List.toDelayMap |>
        DelayMap.map(v -> { l := "a" :: deref l; v } as & Pure) |>
        DelayMap.map(v -> { l := "b" :: deref l; v } as & Pure);
        let _ = DelayMap.toMap(m);
        List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool =
        Map#{}: Map[Unit, Unit] |> Map.toDelayMap |>
            DelayMap.mapWithKey((_, v) -> v) |> DelayMap.toMap == Map#{}

    @test
    def mapWithKey02(): Bool =
        Map#{1 -> 4} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{1 -> 5}

    @test
    def mapWithKey03(): Bool =
        Map#{2 -> -1, 0 -> 0} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{2 -> 1, 0 -> 0}

    @test
    def mapWithKey04(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{2 -> 1, 5 -> 20, 11 -> 2}

    @test
    def mapWithKey05(): Bool =
        Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{2 -> 1, 5 -> 20, 11 -> 2, 8 -> 16}


    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def union01(): Bool =
        let m1 = Map.toDelayMap(Map#{}: Map[Unit, Unit]);
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{}

    @test
    def union02(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def union03(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def union04(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union05(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union06(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def union07(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{1 -> 5});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def union08(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union09(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union10(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union11(): Bool =
        let m1 = Map.toDelayMap(Map#{3 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.union(m1, m2) |> DelayMap.toMap== Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def union12(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{2 -> 8});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def union13(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{1 -> 8});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def union14(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 4, 2 -> 3}

    @test
    def union15(): Bool =
        let m1 = Map.toDelayMap(Map#{2 -> 7});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 7}

    @test
    def union16(): Bool =
        let m1 = Map.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWith01(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWith02(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def unionWith03(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def unionWith04(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith05(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith06(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWith07(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{1 -> 5});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> -3}

    @test
    def unionWith08(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith09(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith10(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith11(): Bool =
        let m1 = Map.toDelayMap(Map#{3 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWith12(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{2 -> 8});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> -5}

    @test
    def unionWith13(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{1 -> 8});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> -6, 2 -> 3}

    @test
    def unionWith14(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3}

    @test
    def unionWith15(): Bool =
        let m1 = Map.toDelayMap(Map#{2 -> 7});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 4}

    @test
    def unionWith16(): Bool =
        let m1 = Map.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{2 -> -1, 11 -> 14, 9 -> 8, 15 -> 1, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyPure01(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWithKeyPure02(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyPure03(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyPure04(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyPure05(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyPure06(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyPure07(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{1 -> 5});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> -2}

    @test
    def unionWithKeyPure08(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure09(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure10(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure11(): Bool =
        let m1 = Map.toDelayMap(Map#{3 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyPure12(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{2 -> 8});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> -3}

    @test
    def unionWithKeyPure13(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{1 -> 8});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> -5, 2 -> 3}

    @test
    def unionWithKeyPure14(): Bool =
        let m1 = Map.toDelayMap(Map#{1 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 3, 2 -> 3}

    @test
    def unionWithKeyPure15(): Bool =
        let m1 = Map.toDelayMap(Map#{2 -> 7});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 6}

    @test
    def unionWithKeyPure16(): Bool =
        let m1 = Map.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyImpure01(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{}: Map[Int, Int]

    @test
    def unionWithKeyImpure02(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyImpure03(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2}

    @test
    def unionWithKeyImpure04(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyImpure05(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyImpure06(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 3 -> 4}

    @test
    def unionWithKeyImpure07(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2});
        let m2 = Map.toDelayMap(Map#{1 -> 5});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> -2}

    @test
    def unionWithKeyImpure08(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure09(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3, 3 -> 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure10(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{3 -> 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure11(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{3 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 3, 3 -> 4}

    @test
    def unionWithKeyImpure12(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{2 -> 8});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> -3}

    @test
    def unionWithKeyImpure13(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        let m2 = Map.toDelayMap(Map#{1 -> 8});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> -5, 2 -> 3}

    @test
    def unionWithKeyImpure14(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{1 -> 4});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 3, 2 -> 3}

    @test
    def unionWithKeyImpure15(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{2 -> 7});
        let m2 = Map.toDelayMap(Map#{1 -> 2, 2 -> 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{1 -> 2, 2 -> 6}

    @test
    def unionWithKeyImpure16(): Bool & Impure =
        let m1 = Map.toDelayMap(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22});
        let m2 = Map.toDelayMap(Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, m1, m2) |> DelayMap.toMap == Map#{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey unionWithKey                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyUnionWithKey01(): Bool =
        Map.toDelayMap(Map#{1 -> 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 -> 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 -> 3})) |>
            DelayMap.toMap == Map#{1 -> 6}

    @test
    def unionWithKeyUnionWithKey02(): Bool & Impure =
        Map.toDelayMap(Map#{1 -> 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map.toDelayMap(Map#{1 -> 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 -> 3})) |>
            DelayMap.toMap == Map#{1 -> 6}

    @test
    def unionWithKeyUnionWithKey03(): Bool & Impure =
        Map.toDelayMap(Map#{1 -> 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 -> 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map.toDelayMap(Map#{1 -> 3})) |>
            DelayMap.toMap == Map#{1 -> 6}

    @test
    def unionWithKeyUnionWithKey04(): Bool & Impure =
        Map.toDelayMap(Map#{1 -> 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map.toDelayMap(Map#{1 -> 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2 as & Impure, Map.toDelayMap(Map#{1 -> 3})) |>
            DelayMap.toMap == Map#{1 -> 6}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (fusion)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unionWithKeyFusion01(): Bool & Impure =
        let l = ref Nil;
        Map#{1 -> 1, 2 -> 2, 3 -> 3} |> Map.toDelayMap |>
        DelayMap.unionWithKey((_, _, v) -> { l := "a" :: deref l; v }, Map.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3})) |>
        DelayMap.unionWithKey((_, _, v) -> { l := "b" :: deref l; v }, Map.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3}));
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def unionWithKeyFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = Map#{1 -> 1, 2 -> 2, 3 -> 3} |> Map.toDelayMap |>
        DelayMap.unionWithKey((_, _, v) -> { l := "a" :: deref l; v } as & Pure, Map.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3})) |>
        DelayMap.unionWithKey((_, _, v) -> { l := "b" :: deref l; v } as & Pure, Map.toDelayMap(Map#{1 -> 1, 2 -> 2, 3 -> 3}));
        let _ = DelayMap.toMap(m);
        List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil

}
