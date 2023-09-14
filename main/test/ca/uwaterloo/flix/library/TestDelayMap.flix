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


mod TestDelayMap {

    /////////////////////////////////////////////////////////////////////////////
    // insertWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWith01(): Bool =
        DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 3, DelayMap.empty()) |>DelayMap.toList == (1, 3) :: Nil

    @Test
    def insertWith02(): Bool =
        List.toDelayMap((1, 4) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 3) |> DelayMap.toList == (1, 7) :: Nil

    @Test
    def insertWith03(): Bool =
        List.toDelayMap((1, 4) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 2, 3) |> DelayMap.toList == (1, 4) :: (2, 3) :: Nil

    @Test
    def insertWith04(): Bool =
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 1) |> DelayMap.toList == (1, 5) :: (5, -2) :: Nil

    @Test
    def insertWith05(): Bool =
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |>
        DelayMap.insertWith((v1, v2) -> v1 + v2, 5, 1) |> DelayMap.toList == (1, 4) :: (5, -1) :: Nil

    @Test
    def insertWith06(): Bool =
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |>
        DelayMap.insertWith((v1, v2) -> v1 + v2, 4, -2) |>
        DelayMap.toList |> List.sortBy(t -> fst(t)) == (1, 4) :: (4, -2) :: (5, -2):: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWithKey insertWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWithInsertWith01(): Bool =
        (("a", 1) :: Nil)                                    |>
            List.toDelayMap                                  |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil

    @Test
    def insertWithInsertWith02(): Bool \ IO =
        (("a", 1) :: Nil)                                                        |>
            List.toDelayMap                                                      |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3)                     |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil
    @Test
    def insertWithInsertWith03(): Bool \ IO =
        (("a", 1) :: Nil)                                                        |>
            List.toDelayMap                                                      |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1)                     |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil

    @Test
    def insertWithInsertWith04(): Bool \ IO =
        (("a", 1) :: Nil)                                                        |>
            List.toDelayMap                                                      |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, unchecked_cast("b" as _ \ IO), 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWith insertWith fusion                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertWithFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard List.toDelayMap((1, 1) :: Nil) |>
            DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1) |>
            DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v }, 1, 1);
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil
    }

    //@Test
    //def insertWithFusion02(): Bool = region rc {
    //    let l = ref Nil @ rc;
    //    let m = List.toDelayMap((1, 1) :: Nil) |>
    //    DelayMap.insertWith((v, _) -> unchecked_cast({ l := "a" :: deref l; v } as _ \ {}), 1, 1) |>
    //    DelayMap.insertWith((v, _) -> unchecked_cast({ l := "a" :: deref l; v } as _ \ {}), 1, 1) |>
    //    DelayMap.insertWith((v, _) -> unchecked_cast({ l := "a" :: deref l; v } as _ \ {}), 1, 1) |>
    //    DelayMap.insertWith((v, _) -> unchecked_cast({ l := "b" :: deref l; v } as _ \ {}), 1, 1) |>
    //    DelayMap.insertWith((v, _) -> unchecked_cast({ l := "b" :: deref l; v } as _ \ {}), 1, 1) |>
    //    DelayMap.insertWith((v, _) -> unchecked_cast({ l := "b" :: deref l; v } as _ \ {}), 1, 1);
    //    let _ = DelayMap.toMap(m);
    //    List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil
    //}

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool =
        let m = Map#{}: Map[Unit, Unit] |> Map.toDelayMap;
        DelayMap.count((k, v) -> k == v, m) == 0

    @Test
    def count02(): Bool =
        let m = Map#{1 => 2} |> Map.toDelayMap;
        DelayMap.count((k, v) -> k == v, m) == 0

    @Test
    def count03(): Bool =
        let m = Map#{1 => 1} |> Map.toDelayMap;
        DelayMap.count((k, v) -> k == v, m) == 1

    @Test
    def count04(): Bool =
        let m = Map#{1 => 2, 2 => 3} |> Map.toDelayMap;
        DelayMap.count((k, v) -> k == v, m) == 0

    @Test
    def count05(): Bool =
        let m = Map#{1 => 1, 2 => 3} |> Map.toDelayMap;
        DelayMap.count((k, v) -> k == v, m) == 1

    @Test
    def count06(): Bool =
        let m = Map#{1 => 2, 2 => 2} |> Map.toDelayMap;
        DelayMap.count((k, v) -> k == v, m) == 1

    @Test
    def count07(): Bool =
        let m = Map#{1 => 1, 2 => 2} |> Map.toDelayMap;
        DelayMap.count((k, v) -> k == v, m) == 2

    @Test
    def count08(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap |> Map.toDelayMap;
        let a = MutList.new(rc);
        discard DelayMap.count((k, _) -> { let b = MutList.new(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); true }, m);
        MutList.toList(a) == range
    }


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapPure01(): Bool =
        Map#{}: Map[Unit, Int32] |> Map.toDelayMap |> DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{}

    @Test
    def mapPure02(): Bool =
        Map#{1 => 4} |> Map.toDelayMap |> DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{1 => 12}

    @Test
    def mapPure03(): Bool =
        Map#{2 => -1, 0 => 0} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{2 => -3, 0 => 0}

    @Test
    def mapPure04(): Bool =
        Map#{2 => -1, 5 => 15, 11 => -9} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{2 => -3, 5 => 45, 11 => -27}

    @Test
    def mapPure05(): Bool =
        Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8} |> Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |> DelayMap.toMap == Map#{2 => -3, 5 => 45, 11 => -27, 8 => 24}


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapImpure01(): Bool \ IO =
        Map#{}: Map[Unit, Int32] |> Map.toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap == Map#{}

    @Test
    def mapImpure02(): Bool \ IO =
        Map#{1 => 4} |> Map.toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap == Map#{1 => 12}

    @Test
    def mapImpure03(): Bool \ IO =
        Map#{2 => -1, 0 => 0} |> Map.toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap == Map#{2 => -3, 0 => 0}

    @Test
    def mapImpure04(): Bool \ IO =
        Map#{2 => -1, 5 => 15, 11 => -9} |> Map.toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap == Map#{2 => -3, 5 => 45, 11 => -27}

    @Test
    def mapImpure05(): Bool \ IO =
        Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8} |> Map.toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |> DelayMap.toMap == Map#{2 => -3, 5 => 45, 11 => -27, 8 => 24}


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapMap01(): Bool =
        Map#{2 => -1, 5 => 15, 11 => -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.toMap == Map#{2 => -9, 5 => 135, 11 => -81}

    @Test
    def mapMap02(): Bool \ IO =
        Map#{2 => -1, 5 => 15, 11 => -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.toMap == Map#{2 => -9, 5 => 135, 11 => -81}

    @Test
    def mapMap03(): Bool \ IO =
        Map#{2 => -1, 5 => 15, 11 => -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> 3 * v) |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.toMap == Map#{2 => -9, 5 => 135, 11 => -81}

    @Test
    def mapMap04(): Bool \ IO =
        Map#{2 => -1, 5 => 15, 11 => -9} |>
            Map.toDelayMap |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.map(v -> unchecked_cast(3 * v as _ \ IO)) |>
            DelayMap.toMap == Map#{2 => -9, 5 => 135, 11 => -81}


    /////////////////////////////////////////////////////////////////////////////
    // map (fusion)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List.toDelayMap |>
            DelayMap.map(v -> { l := "a" :: deref l; v }) |>
            DelayMap.map(v -> { l := "b" :: deref l; v });
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil
    }

    //@Test
    //def mapFusion02(): Bool = region rc {
    //    let l = ref Nil @ rc;
    //    let m = ((1, 1) :: (2, 2) :: (3, 3) :: Nil) |> List.toDelayMap |>
    //    DelayMap.map(v -> unchecked_cast({ l := "a" :: deref l; v } as _ \ {})) |>
    //    DelayMap.map(v -> unchecked_cast({ l := "b" :: deref l; v } as _ \ {}));
    //    let _ = DelayMap.toMap(m);
    //    List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil
    //}

    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Bool =
        Map#{}: Map[Unit, Unit] |> Map.toDelayMap |>
            DelayMap.mapWithKey((_, v) -> v) |> DelayMap.toMap == Map#{}

    @Test
    def mapWithKey02(): Bool =
        Map#{1 => 4} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{1 => 5}

    @Test
    def mapWithKey03(): Bool =
        Map#{2 => -1, 0 => 0} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{2 => 1, 0 => 0}

    @Test
    def mapWithKey04(): Bool =
        Map#{2 => -1, 5 => 15, 11 => -9} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{2 => 1, 5 => 20, 11 => 2}

    @Test
    def mapWithKey05(): Bool =
        Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8} |> Map.toDelayMap |>
            DelayMap.mapWithKey((k, v) -> k + v) |> DelayMap.toMap == Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}


    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def union01(): Bool =
        let m1 = Map.toDelayMap(Map#{}: Map[Unit, Unit]);
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{}

    @Test
    def union02(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def union03(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def union04(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def union05(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def union06(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def union07(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{1 => 5});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def union08(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def union09(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def union10(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def union11(): Bool =
        let m1 = Map.toDelayMap(Map#{3 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.union(m1, m2) |> DelayMap.toMap== Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def union12(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{2 => 8});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3}

    @Test
    def union13(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{1 => 8});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3}

    @Test
    def union14(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 4, 2 => 3}

    @Test
    def union15(): Bool =
        let m1 = Map.toDelayMap(Map#{2 => 7});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 7}

    @Test
    def union16(): Bool =
        let m1 = Map.toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = Map.toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        DelayMap.union(m1, m2) |> DelayMap.toMap == Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22, 1 => 2, 44 => 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWith01(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{}: Map[Int32, Int32]

    @Test
    def unionWith02(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def unionWith03(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def unionWith04(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWith05(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWith06(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWith07(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{1 => 5});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => -3}

    @Test
    def unionWith08(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWith09(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWith10(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWith11(): Bool =
        let m1 = Map.toDelayMap(Map#{3 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWith12(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{2 => 8});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => -5}

    @Test
    def unionWith13(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{1 => 8});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => -6, 2 => 3}

    @Test
    def unionWith14(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3}

    @Test
    def unionWith15(): Bool =
        let m1 = Map.toDelayMap(Map#{2 => 7});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 4}

    @Test
    def unionWith16(): Bool =
        let m1 = Map.toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = Map.toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        DelayMap.unionWith((v1, v2) -> v1 - v2, m1, m2) |> DelayMap.toMap == Map#{2 => -1, 11 => 14, 9 => 8, 15 => 1, 1 => 2, 44 => 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyPure01(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{}: Map[Int32, Int32]

    @Test
    def unionWithKeyPure02(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def unionWithKeyPure03(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def unionWithKeyPure04(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKeyPure05(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKeyPure06(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKeyPure07(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{1 => 5});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => -2}

    @Test
    def unionWithKeyPure08(): Bool =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyPure09(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyPure10(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyPure11(): Bool =
        let m1 = Map.toDelayMap(Map#{3 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyPure12(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{2 => 8});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => -3}

    @Test
    def unionWithKeyPure13(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{1 => 8});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => -5, 2 => 3}

    @Test
    def unionWithKeyPure14(): Bool =
        let m1 = Map.toDelayMap(Map#{1 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 3, 2 => 3}

    @Test
    def unionWithKeyPure15(): Bool =
        let m1 = Map.toDelayMap(Map#{2 => 7});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 6}

    @Test
    def unionWithKeyPure16(): Bool =
        let m1 = Map.toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = Map.toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, m1, m2) |> DelayMap.toMap == Map#{2 => 1, 11 => 14, 9 => 8, 15 => 16, 1 => 2, 44 => 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyImpure01(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{}: Map[Int32, Int32]

    @Test
    def unionWithKeyImpure02(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def unionWithKeyImpure03(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def unionWithKeyImpure04(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKeyImpure05(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKeyImpure06(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 3 => 4}

    @Test
    def unionWithKeyImpure07(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2});
        let m2 = Map.toDelayMap(Map#{1 => 5});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => -2}

    @Test
    def unionWithKeyImpure08(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyImpure09(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3, 3 => 4});
        let m2 = Map.toDelayMap(Map#{});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyImpure10(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{3 => 4});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyImpure11(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{3 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 3, 3 => 4}

    @Test
    def unionWithKeyImpure12(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{2 => 8});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => -3}

    @Test
    def unionWithKeyImpure13(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        let m2 = Map.toDelayMap(Map#{1 => 8});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => -5, 2 => 3}

    @Test
    def unionWithKeyImpure14(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{1 => 4});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 3, 2 => 3}

    @Test
    def unionWithKeyImpure15(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{2 => 7});
        let m2 = Map.toDelayMap(Map#{1 => 2, 2 => 3});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{1 => 2, 2 => 6}

    @Test
    def unionWithKeyImpure16(): Bool \ IO =
        let m1 = Map.toDelayMap(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22});
        let m2 = Map.toDelayMap(Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33});
        DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), m1, m2) |> DelayMap.toMap == Map#{2 => 1, 11 => 14, 9 => 8, 15 => 16, 1 => 2, 44 => 33}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey unionWithKey                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyUnionWithKey01(): Bool =
        Map.toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap == Map#{1 => 6}

    @Test
    def unionWithKeyUnionWithKey02(): Bool \ IO =
        Map.toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), Map.toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap == Map#{1 => 6}

    @Test
    def unionWithKeyUnionWithKey03(): Bool \ IO =
        Map.toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> k + v1 - v2, Map.toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), Map.toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap == Map#{1 => 6}

    @Test
    def unionWithKeyUnionWithKey04(): Bool \ IO =
        Map.toDelayMap(Map#{1 => 5}) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), Map.toDelayMap(Map#{1 => 2})) |>
            DelayMap.unionWithKey((k, v1, v2) -> unchecked_cast(k + v1 - v2 as _ \ IO), Map.toDelayMap(Map#{1 => 3})) |>
            DelayMap.toMap == Map#{1 => 6}


    /////////////////////////////////////////////////////////////////////////////
    // unionWithKey (fusion)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unionWithKeyFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toDelayMap |>
            DelayMap.unionWithKey((_, _, v) -> { l := "a" :: deref l; v }, Map.toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3})) |>
            DelayMap.unionWithKey((_, _, v) -> { l := "b" :: deref l; v }, Map.toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3}));
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    //@Test
    //def unionWithKeyFusion02(): Bool = region rc {
    //    let l = ref Nil @ rc;
    //    let m = Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toDelayMap |>
    //    DelayMap.unionWithKey((_, _, v) -> unchecked_cast({ l := "a" :: deref l; v } as _ \ {}), Map.toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3})) |>
    //    DelayMap.unionWithKey((_, _, v) -> unchecked_cast({ l := "b" :: deref l; v } as _ \ {}), Map.toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3}));
    //    let _ = DelayMap.toMap(m);
    //    List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil
    //}

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjust01(): Bool =
        let m = Map.toDelayMap(Map#{});
        DelayMap.adjust(v -> 2 * v, 1, m) |> DelayMap.toMap == Map#{}

    @Test
    def adjust02(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.adjust(v -> 2 * v, 1, m) |> DelayMap.toMap == Map#{1 => 8}

    @Test
    def adjust03(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.adjust(v -> 2 * v, 2, m) |> DelayMap.toMap == Map#{1 => 4}

    @Test
    def adjust04(): Bool =
        let m = Map.toDelayMap(Map#{1 => -14, 5 => -2});
        DelayMap.adjust(v -> 2 * v, 1, m) |> DelayMap.toMap == Map#{1 => -28, 5 => -2}

    @Test
    def adjust05(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.adjust(v -> 2 * v, 5, m) |> DelayMap.toMap == Map#{1 => 4, 5 => -4}

    @Test
    def adjust06(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.adjust(v -> 2 * v, 4, m) |> DelayMap.toMap == Map#{1 => 4, 5 => -2}


    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjustWithKey01(): Bool =
        let m = Map.toDelayMap(Map#{});
        DelayMap.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap.toMap == Map#{}

    @Test
    def adjustWithKey02(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap.toMap == Map#{1 => 5}

    @Test
    def adjustWithKey03(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.adjustWithKey((k, v) -> k + v, 2, m) |> DelayMap.toMap == Map#{1 => 4}

    @Test
    def adjustWithKey04(): Bool =
        let m = Map.toDelayMap(Map#{1 => -14, 5 => -2});
        DelayMap.adjustWithKey((k, v) -> k + v, 1, m) |> DelayMap.toMap == Map#{1 => -13, 5 => -2}

    @Test
    def adjustWithKey05(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.adjustWithKey((k, v) -> k + v, 5, m) |> DelayMap.toMap == Map#{1 => 4, 5 => 3}

    @Test
    def adjustWithKey06(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.adjustWithKey((k, v) -> k + v, 4, m) |> DelayMap.toMap == Map#{1 => 4, 5 => -2}


    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Bool =
        let m = Map.toDelayMap(Map#{});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap == Map#{}

    @Test
    def update02(): Bool =
        let m = Map.toDelayMap(Map#{1 => 3});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap == Map#{1 => 6}

    @Test
    def update03(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap == Map#{1 => 4}

    @Test
    def update04(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 2, m) |> DelayMap.toMap == Map#{1 => 4}

    @Test
    def update05(): Bool =
        let m = Map.toDelayMap(Map#{1 => -14, 5 => -2});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap == Map#{1 => -14, 5 => -2}

    @Test
    def update06(): Bool =
        let m = Map.toDelayMap(Map#{1 => -13, 5 => -2});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 1, m) |> DelayMap.toMap == Map#{1 => -26, 5 => -2}

    @Test
    def update07(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 5, m) |> DelayMap.toMap == Map#{1 => 4, 5 => -2}

    @Test
    def update08(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -1});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 5, m) |> DelayMap.toMap == Map#{1 => 4, 5 => -2}

    @Test
    def update09(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.update(v -> if (v `Int32.rem` 2 != 0) Some(2 * v) else None, 4, m) |> DelayMap.toMap == Map#{1 => 4, 5 => -2}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (pure)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyPure01(): Bool =
        let m = Map.toDelayMap(Map#{});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap == Map#{}

    @Test
    def updateWithKeyPure02(): Bool =
        let m = Map.toDelayMap(Map#{1 => 3});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap == Map#{1 => 7}

    @Test
    def updateWithKeyPure03(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap == Map#{1 => 4}

    @Test
    def updateWithKeyPure04(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 2, m) |>
            DelayMap.toMap == Map#{1 => 4}

    @Test
    def updateWithKeyPure05(): Bool =
        let m = Map.toDelayMap(Map#{1 => -14, 5 => -2});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap == Map#{1 => -14, 5 => -2}

    @Test
    def updateWithKeyPure06(): Bool =
        let m = Map.toDelayMap(Map#{1 => -13, 5 => -2});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 1, m) |>
            DelayMap.toMap == Map#{1 => -25, 5 => -2}

    @Test
    def updateWithKeyPure07(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 5, m) |>
            DelayMap.toMap == Map#{1 => 4, 5 => -2}

    @Test
    def updateWithKeyPure08(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -1});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 5, m) |>
            DelayMap.toMap == Map#{1 => 4, 5 => 3}

    @Test
    def updateWithKeyPure09(): Bool =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None, 4, m) |>
            DelayMap.toMap == Map#{1 => 4, 5 => -2}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (pure)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyImpure01(): Bool \ IO =
        let m = Map.toDelayMap(Map#{});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap == Map#{}

    @Test
    def updateWithKeyImpure02(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => 3});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap == Map#{1 => 7}

    @Test
    def updateWithKeyImpure03(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap == Map#{1 => 4}

    @Test
    def updateWithKeyImpure04(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => 4});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 2, m) |>
            DelayMap.toMap == Map#{1 => 4}

    @Test
    def updateWithKeyImpure05(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => -14, 5 => -2});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap == Map#{1 => -14, 5 => -2}

    @Test
    def updateWithKeyImpure06(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => -13, 5 => -2});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 1, m) |>
            DelayMap.toMap == Map#{1 => -25, 5 => -2}

    @Test
    def updateWithKeyImpure07(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 5, m) |>
            DelayMap.toMap == Map#{1 => 4, 5 => -2}

    @Test
    def updateWithKeyImpure08(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -1});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 5, m) |>
            DelayMap.toMap == Map#{1 => 4, 5 => 3}

    @Test
    def updateWithKeyImpure09(): Bool \ IO =
        let m = Map.toDelayMap(Map#{1 => 4, 5 => -2});
        DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v `Int32.rem` 2 != 0) Some(k + 2 * v) else None as _ \ IO), 4, m) |>
            DelayMap.toMap == Map#{1 => 4, 5 => -2}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey updateWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyUpdateWithKey01(): Bool =
        Map.toDelayMap(Map#{1 => 4, 5 => -2})                                          |>
            DelayMap.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None, 1) |>
            DelayMap.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None, 5) |>
            DelayMap.toMap == Map#{1 => 9, 5 => 1}

    @Test
    def updateWithKeyUpdateWithKey02(): Bool \ IO =
        Map.toDelayMap(Map#{1 => 4, 5 => -2})                                                              |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v == 4) Some(k + 2 * v) else None as _ \ IO), 1) |>
            DelayMap.updateWithKey((k, v) -> if (k == 5) Some(k + 2 * v) else None, 5)                     |>
            DelayMap.toMap == Map#{1 => 9, 5 => 1}

    @Test
    def updateWithKeyUpdateWithKey03(): Bool \ IO =
        Map.toDelayMap(Map#{1 => 4, 5 => -2})                                                              |>
            DelayMap.updateWithKey((k, v) -> if (v == 4) Some(k + 2 * v) else None, 1)                     |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (k == 5) Some(k + 2 * v) else None as _ \ IO), 5) |>
            DelayMap.toMap == Map#{1 => 9, 5 => 1}

    @Test
    def updateWithKeyUpdateWithKey04(): Bool \ IO =
        Map.toDelayMap(Map#{1 => 4, 5 => -2})                                                              |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (v == 4) Some(k + 2 * v) else None as _ \ IO), 1) |>
            DelayMap.updateWithKey((k, v) -> unchecked_cast(if (k == 5) Some(k + 2 * v) else None as _ \ IO), 5) |>
            DelayMap.toMap == Map#{1 => 9, 5 => 1}


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey (fusion)                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKeyFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard Map.toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3}) |>
            DelayMap.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) }, 1) |>
            DelayMap.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) }, 2) |>
            DelayMap.updateWithKey((_, v) -> { l := "a" :: deref l; Some(v) }, 3) |>
            DelayMap.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) }, 1) |>
            DelayMap.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) }, 2) |>
            DelayMap.updateWithKey((_, v) -> { l := "b" :: deref l; Some(v) }, 3);
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil
    }

    //@Test
    //def updateWithKeyFusion02(): Bool = region rc {
    //    let l = ref Nil @ rc;
    //    let _ = Map.toDelayMap(Map#{1 => 1, 2 => 2, 3 => 3}) |>
    //        DelayMap.updateWithKey((_, v) -> unchecked_cast({ l := "a" :: deref l; Some(v) } as _ \ {}), 1) |>
    //        DelayMap.updateWithKey((_, v) -> unchecked_cast({ l := "a" :: deref l; Some(v) } as _ \ {}), 2) |>
    //        DelayMap.updateWithKey((_, v) -> unchecked_cast({ l := "a" :: deref l; Some(v) } as _ \ {}), 3) |>
    //        DelayMap.updateWithKey((_, v) -> unchecked_cast({ l := "b" :: deref l; Some(v) } as _ \ {}), 1) |>
    //        DelayMap.updateWithKey((_, v) -> unchecked_cast({ l := "b" :: deref l; Some(v) } as _ \ {}), 2) |>
    //        DelayMap.updateWithKey((_, v) -> unchecked_cast({ l := "b" :: deref l; Some(v) } as _ \ {}), 3) |>
    //        DelayMap.toMap;
    //    List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil
    //}

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Bool =
        Map#{} |> Map.toDelayMap |> DelayMap.size == 0

    @Test
    def size02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |> DelayMap.size == 1

    @Test
    def size03(): Bool =
        Map#{1 => 2, 2 => 4} |> Map.toDelayMap |> DelayMap.size == 2

    @Test
    def size04(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6} |> Map.toDelayMap |> DelayMap.size == 3

    @Test
    def size05(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> Map.toDelayMap |> DelayMap.size == 4

    @Test
    def size06(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10} |> Map.toDelayMap |> DelayMap.size == 5


    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Bool =
        DelayMap.singleton(1, 2) |> DelayMap.toMap == Map#{1 => 2}

    @Test
    def singleton02(): Bool =
        DelayMap.singleton(3, -1) |> DelayMap.toMap == Map#{3 => -1}

    @Test
    def singleton03(): Bool =
        DelayMap.singleton(-99, -11) |> DelayMap.toMap == Map#{-99 => -11}


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool =
        Map#{}: Map[Unit, Unit] |> Map.toDelayMap |> DelayMap.isEmpty

    @Test
    def isEmpty02(): Bool =
        not (Map#{1 => 2} |> Map.toDelayMap |> DelayMap.isEmpty)

    @Test
    def isEmpty03(): Bool =
        not (Map#{1 => 2, 2 => 4} |> Map.toDelayMap |> DelayMap.isEmpty)

    @Test
    def isEmpty04(): Bool =
        not (Map#{1 => 2, 2 => 4, 3 => 6} |> Map.toDelayMap |> DelayMap.isEmpty)

    @Test
    def isEmpty05(): Bool =
        not (Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> Map.toDelayMap |> DelayMap.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool =
        Map#{} |> Map.toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0) == 0

    @Test
    def foldLeft02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0) == 2

    @Test
    def foldLeft03(): Bool =
        Map#{1 => 2, 3 => 4} |> Map.toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0) == 6

    @Test
    def foldLeft04(): Bool =
        Map#{1 => 2, 3 => 4, 5 => 6} |> Map.toDelayMap |>
            DelayMap.foldLeft((s, v) -> s + v, 0) == 12


    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeftWithKey01(): Bool =
        Map#{} |> Map.toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0) == 0

    @Test
    def foldLeftWithKey02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0) == 3

    @Test
    def foldLeftWithKey03(): Bool =
        Map#{1 => 2, 3 => 4} |> Map.toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0) == 10

    @Test
    def foldLeftWithKey04(): Bool =
        Map#{1 => 2, 3 => 4, 5 => 6} |> Map.toDelayMap |>
            DelayMap.foldLeftWithKey((s, k, v) -> k + s + v, 0) == 21


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool =
        Map#{} |> Map.toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0) == 0

    @Test
    def foldRight02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0) == 2

    @Test
    def foldRight03(): Bool =
        Map#{1 => 2, 3 => 4} |> Map.toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0) == 6

    @Test
    def foldRight04(): Bool =
        Map#{1 => 2, 3 => 4, 5 => 6} |> Map.toDelayMap |>
            DelayMap.foldRight((v, acc) -> acc + v, 0) == 12


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKey01(): Bool =
        Map#{} |> Map.toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0) == 0

    @Test
    def foldRightWithKey02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0) == 3

    @Test
    def foldRightWithKey03(): Bool =
        Map#{1 => 2, 3 => 4} |> Map.toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0) == 10

    @Test
    def foldRightWithKey04(): Bool =
        Map#{1 => 2, 3 => 4, 5 => 6} |> Map.toDelayMap |>
            DelayMap.foldRightWithKey((k, v, acc) -> k + acc + v, 0) == 21

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool =
        Map#{} |> Map.toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0) == 0

    @Test
    def foldRightWithCont02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0) == 2

    @Test
    def foldRightWithCont03(): Bool =
        Map#{1 => 2, 3 => 4} |> Map.toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0) == 6

    @Test
    def foldRightWithCont04(): Bool =
        Map#{1 => 2, 3 => 4, 5 => 6} |> Map.toDelayMap |>
            DelayMap.foldRightWithCont((v, c) -> c() + v, 0) == 12


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKeyCont                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKeyCont01(): Bool =
        Map#{} |> Map.toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0) == 0

    @Test
    def foldRightWithKeyCont02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0) == 3

    @Test
    def foldRightWithKeyCont03(): Bool =
        Map#{1 => 2, 3 => 4} |> Map.toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0) == 10

    @Test
    def foldRightWithKeyCont04(): Bool =
        Map#{1 => 2, 3 => 4, 5 => 6} |> Map.toDelayMap |>
            DelayMap.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0) == 21


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2) == None

    @Test
    def reduceLeft02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2) == Some(2)

    @Test
    def reduceLeft03(): Bool =
        Map#{1 => 2, 2 => 3} |> Map.toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2) == Some(-1)

    @Test
    def reduceLeft04(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.toDelayMap |>
            DelayMap.reduceLeft((v1, v2) -> v1 - v2) == Some(-5)


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeftWithKey01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == None

    @Test
    def reduceLeftWithKey02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == Some((1, 2))

    @Test
    def reduceLeftWithKey03(): Bool =
        Map#{1 => 2, 2 => 3} |> Map.toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == Some((-1, -1))

    @Test
    def reduceLeftWithKey04(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.toDelayMap |>
            DelayMap.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == Some((-4, -5))


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2) == None

    @Test
    def reduceRight02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2) == Some(2)

    @Test
    def reduceRight03(): Bool =
        Map#{1 => 2, 2 => 3} |> Map.toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2) == Some(-1)

    @Test
    def reduceRight04(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.toDelayMap |>
            DelayMap.reduceRight((v1, v2) -> v1 - v2) == Some(3)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRightWithKey01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == None

    @Test
    def reduceRightWithKey02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == Some((1, 2))

    @Test
    def reduceRightWithKey03(): Bool =
        Map#{1 => 2, 2 => 3} |> Map.toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == Some((-1, -1))

    @Test
    def reduceRightWithKey04(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.toDelayMap |>
            DelayMap.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2)) == Some((2 , 3))


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let ri = ref 21 @ rc;
        Map#{} |> Map.toDelayMap |>
            DelayMap.forEach((k, _) -> ri := k);
        21 == deref ri
    }

    @Test
    def forEach02(): Bool = region rc {
        let ri = ref 21 @ rc;
        Map#{1 => "Hello World!"} |> Map.toDelayMap |>
            DelayMap.forEach((k, _) -> ri := k);
        1 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let ri = ref 21 @ rc;
        Map#{} |> Map.toDelayMap |>
            DelayMap.forEachWithIndex((i, _, _) -> ri := i);
        21 == deref ri
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let ri = ref 21 @ rc;
        Map#{1 => "Hello World!"} |> Map.toDelayMap |>
            DelayMap.forEachWithIndex((i, _, _) -> ri := i);
        0 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool =
        not (Map#{}: Map[_, Unit] |> Map.toDelayMap |>
            DelayMap.memberOf(2))

    @Test
    def memberOf02(): Bool =
        not (Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.memberOf(2))

    @Test
    def memberOf03(): Bool =
        Map#{2 => 1} |> Map.toDelayMap |>
            DelayMap.memberOf(2)

    @Test
    def memberOf04(): Bool =
        not (Map#{2 => 1, 3 => 17} |> Map.toDelayMap |>
            DelayMap.memberOf(5))

    @Test
    def memberOf05(): Bool =
        Map#{2 => 1, 5 => 17} |> Map.toDelayMap |>
            DelayMap.memberOf(5)

    @Test
    def memberOf06(): Bool =
        Map#{5 => 1, 3 => 17} |> Map.toDelayMap |>
            DelayMap.memberOf(5)

    @Test
    def memberOf07(): Bool =
        not (Map#{2 => 1, 3 => 17, -1 => -2} |> Map.toDelayMap |>
            DelayMap.memberOf(-2))

    @Test
    def memberOf08(): Bool =
        Map#{-2 => 1, 3 => 17, -1 => -2} |> Map.toDelayMap |>
            DelayMap.memberOf(-2)

    @Test
    def memberOf09(): Bool =
        Map#{2 => 1, -2 => 17, -1 => -2} |> Map.toDelayMap |>
            DelayMap.memberOf(-2)

    @Test
    def memberOf10(): Bool =
        Map#{2 => 1, 3 => 17, -2 => -2} |> Map.toDelayMap |>
            DelayMap.memberOf(-2)


    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def keysOf01(): Bool =
        Map#{}: Map[Unit, Unit] |> Map.toDelayMap |>
            DelayMap.keysOf == Set#{}

    @Test
    def keysOf02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.keysOf == Set#{1}

    @Test
    def keysOf03(): Bool =
        Map#{1 => 2, 2 => 4} |> Map.toDelayMap |>
            DelayMap.keysOf == Set#{1, 2}

    @Test
    def keysOf04(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6} |> Map.toDelayMap |>
            DelayMap.keysOf == Set#{1, 2, 3}

    @Test
    def keysOf05(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> Map.toDelayMap |>
            DelayMap.keysOf == Set#{1, 2, 3, 4}

    @Test
    def keysOf06(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10} |> Map.toDelayMap |>
            DelayMap.keysOf == Set#{1, 2, 3, 4, 5}


    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valuesOf01(): Bool =
        Map#{}: Map[Unit, Unit] |> Map.toDelayMap |>
            DelayMap.valuesOf == Nil

    @Test
    def valuesOf02(): Bool =
        Map#{1 => 2} |> Map.toDelayMap |>
            DelayMap.valuesOf == 2 :: Nil

    @Test
    def valuesOf03(): Bool =
        Map#{1 => 2, 2 => 4} |> Map.toDelayMap |>
            DelayMap.valuesOf == 2 :: 4 :: Nil

    @Test
    def valuesOf04(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6} |> Map.toDelayMap |>
            DelayMap.valuesOf == 2 :: 4 :: 6 :: Nil

    @Test
    def valuesOf05(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8} |> Map.toDelayMap |>
            DelayMap.valuesOf == 2 :: 4 :: 6 :: 8 :: Nil

    @Test
    def valuesOf06(): Bool =
        Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10} |> Map.toDelayMap |>
            DelayMap.valuesOf == 2 :: 4 :: 6 :: 8 :: 10 :: Nil

    @Test
    def valuesOf07(): Bool =
        Map#{1 => -11, 2 => 4, 3 => -5, 4 => 7, 5 => -5} |> Map.toDelayMap |>
            DelayMap.valuesOf == -11 :: 4 :: -5 :: 7 :: -5 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // minimumKey                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKey01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.minimumKey == None

    @Test
    def minimumKey02(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> Map.toDelayMap |>
            DelayMap.minimumKey == Some((1, 2))

    @Test
    def minimumKey03(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.minimumKey == Some((0, 2))

    @Test
    def minimumKey04(): Bool =
        Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.minimumKey == Some((-44, 4))


    /////////////////////////////////////////////////////////////////////////////
    // minimumKeyBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKeyBy01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y) == None

    @Test
    def minimumKeyBy02(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> Map.toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y) == Some((1, 2))

    @Test
    def minimumKeyBy03(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y) == Some((0, 2))

    @Test
    def minimumKeyBy04(): Bool =
        Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.minimumKeyBy((x, y) -> x <=> y) == Some((-44, 4))


    /////////////////////////////////////////////////////////////////////////////
    // maximumKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKey01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.maximumKey == None

    @Test
    def maximumKey02(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> Map.toDelayMap |>
            DelayMap.maximumKey == Some((5, 5))

    @Test
    def maximumKey03(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.maximumKey == Some((5, 5))

    @Test
    def maximumKey04(): Bool =
        Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.maximumKey == Some((107, -107))


    /////////////////////////////////////////////////////////////////////////////
    // maximumKeyBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKeyBy01(): Bool =
        Map#{}: Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y) == None

    @Test
    def maximumKeyBy02(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5} |> Map.toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y) == Some((5, 5))

    @Test
    def maximumKeyBy03(): Bool =
        Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y) == Some((5, 5))

    @Test
    def maximumKeyBy04(): Bool =
        Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2} |> Map.toDelayMap |>
            DelayMap.maximumKeyBy((x, y) -> x <=> y) == Some((107, -107))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        Map.empty(): Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.iterator(rc) |> Iterator.toMap == Map.empty()
    }

    @Test
    def iterator02(): Bool = region rc {
        Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |> Map.toDelayMap |>
            DelayMap.iterator(rc) |> Iterator.toMap == Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}
    }

    @Test
    def iterator03(): Bool = region rc {
        let l = List.range(0, 100);
        List.zip(l, List.reverse(l)) |> List.toMap |> Map.toDelayMap |>
            DelayMap.iterator(rc) |> Iterator.toMap |> Map.toList == List.zip(List.range(0, 100), List.reverse(List.range(0, 100)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let m: DelayMap[Int32, Int32] = Map#{} |> Map.toDelayMap;
        let d1 = DelayMap.toMutDeque(rc, m);

        let d2 = MutDeque.new(rc);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let m = Map#{1 => 2} |> Map.toDelayMap;
        let d1 = DelayMap.toMutDeque(rc, m);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((1, 2), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let m = Map#{1 => 2, 3 => 4, 5 => 6} |> Map.toDelayMap;
        let d1 = DelayMap.toMutDeque(rc, m);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((3, 4), d2);
        MutDeque.pushBack((5, 6), d2);
        MutDeque.pushFront((1, 2), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let m = Map#{1 => 'a', 2 => 'b', 3 => 'c'} |> Map.toDelayMap;
        let d1 = DelayMap.toMutDeque(rc, m);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront((3, 'c'), d2);
        MutDeque.pushFront((2, 'b'), d2);
        MutDeque.pushFront((1, 'a'), d2);

        d1 `MutDeque.sameElements` d2
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumValues01(): Bool =
        DelayMap.empty() |> DelayMap.sumValues == 0

    @Test
    def sumValues02(): Bool =
        Map#{1 => 1} |> Map.toDelayMap |>
            DelayMap.sumValues == 1

    @Test
    def sumValues03(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toDelayMap |>
            DelayMap.sumValues == 6

    @Test
    def sumValues04(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toDelayMap |>
            DelayMap.sumValues == 3

    @Test
    def sumValues05(): Bool =
        Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> Map.toDelayMap |>
            DelayMap.sumValues == -10

    @Test
    def sumValues06(): Bool =
        Map#{10 => 10, -10 => -10} |> Map.toDelayMap |>
            DelayMap.sumValues == 0

    @Test
    def sumValues07(): Bool =
        List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toDelayMap |>
            DelayMap.sumValues == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumKeys01(): Bool =
        DelayMap.empty() |> DelayMap.sumKeys == 0

    @Test
    def sumKeys02(): Bool =
        Map#{1 => 1} |> Map.toDelayMap |>
            DelayMap.sumKeys == 1

    @Test
    def sumKeys03(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toDelayMap |>
            DelayMap.sumKeys == 6

    @Test
    def sumKeys04(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toDelayMap |>
            DelayMap.sumKeys == 3

    @Test
    def sumKeys05(): Bool =
        Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4} |> Map.toDelayMap |>
            DelayMap.sumKeys == -12

    @Test
    def sumKeys06(): Bool =
        Map#{10 => 10, -10 => -10} |> Map.toDelayMap |>
            DelayMap.sumKeys == 0

    @Test
    def sumKeys07(): Bool =
        List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toDelayMap |>
            DelayMap.sumKeys == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        DelayMap.empty() |> DelayMap.sumWith((k, v) -> k + v) == 0

    @Test
    def sumWith02(): Bool =
        Map#{1 => 1} |> Map.toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v) == 2

    @Test
    def sumWith03(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3} |> Map.toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v) == 12

    @Test
    def sumWith04(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |> Map.toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v) == 6

    @Test
    def sumWith05(): Bool =
        Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |> Map.toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v) == -20

    @Test
    def sumWith06(): Bool =
        Map#{10 => 10, -10 => -10} |> Map.toDelayMap |>
            DelayMap.sumWith((k, v) -> k + v) == 0

    @Test
    def sumWith07(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap |> Map.toDelayMap;
        let a = MutList.new(rc);
        discard DelayMap.sumWith((k, _) -> { let b = MutList.new(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); 0 }, m);
        MutList.toList(a) == range
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Bool =
        DelayMap.empty(): DelayMap[Int32, Int32] |> DelayMap.joinKeys(",") == ""

    @Test
    def joinKeys02(): Bool =
        Map#{1 => 1} |> Map.toDelayMap |> DelayMap.joinKeys(",") == "1"

    @Test
    def joinKeys03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.toDelayMap |> DelayMap.joinKeys(",") == "0,1,2"

    @Test
    def joinKeys04(): Bool =
        Map#{"0" => 1, "1" => 2, "2" => 2} |> Map.toDelayMap |> DelayMap.joinKeys(",") == "0,1,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Bool =
        DelayMap.empty(): DelayMap[Int32, Int32] |> DelayMap.joinValues(",") == ""

    @Test
    def joinValues02(): Bool =
        Map#{1 => 1} |> Map.toDelayMap |> DelayMap.joinValues(",") == "1"

    @Test
    def joinValues03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.toDelayMap |> DelayMap.joinValues(",") == "1,2,2"

    @Test
    def joinValues04(): Bool =
        Map#{0 => "1", 1 => "2", 2 => "2"} |> Map.toDelayMap |> DelayMap.joinValues(",") == "1,2,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        Map.empty(): Map[Int32, Int32] |> Map.toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ",") == ""

    @Test
    def joinWith02(): Bool =
        Map#{1 => 1} |> Map.toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "1 => 1"

    @Test
    def joinWith03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"

    @Test
    def joinWith04(): Bool =
        Map#{0 => "1", 1 => "2", 2 => "2"} |> Map.toDelayMap |>
            DelayMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool =
        ToString.toString(Map#{1 => 2} |> Map.toDelayMap) == "DelayMap#{1 => 2}"

    @Test
    def toString02(): Bool =
        ToString.toString(Map#{1 => 0, 2 => 1, 3 => 2} |> Map.toDelayMap) == "DelayMap#{1 => 0, 2 => 1, 3 => 2}"

    @Test
    def toString03(): Bool =
        ToString.toString(Map#{1 => "b", 2 => "a"} |> Map.toDelayMap) == "DelayMap#{1 => b, 2 => a}"

    @Test
    def toString04(): Bool =
        ToString.toString(Map#{97 => false, 2 => true, 3 => false, 4 => true, 0 => true} |> Map.toDelayMap) == "DelayMap#{0 => true, 2 => true, 3 => false, 4 => true, 97 => false}"

    @Test
    def toString05(): Bool =
        ToString.toString(Map#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}} |> Map.toDelayMap) == "DelayMap#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}}"


}
