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
        DelayMap.insertWith((v1, v2) -> v1 + v2, 1, 3, DelayMap.empty()) |> DelayMap.toList == (1, 3) :: Nil

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
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 5, 1) |> DelayMap.toList == (1, 4) :: (5, -1) :: Nil

    @test
    def insertWith06(): Bool =
        List.toDelayMap((1, 4) :: (5, -2) :: Nil) |> DelayMap.insertWith((v1, v2) -> v1 + v2, 4, -2) |> DelayMap.toList |> List.sortBy(t -> fst(t)) == (1, 4) :: (4, -2) :: (5, -2):: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWithKey insertWithKey                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertWithInsertWith01(): Bool =
        (("a", 1) :: Nil)                                    |>
            List.toDelayMap                                 |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith02(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List.toDelayMap                                             |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 3)             |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil
    @test
    def insertWithInsertWith03(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List.toDelayMap                                             |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b", 1)             |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil

    @test
    def insertWithInsertWith04(): Bool & Impure =
        (("a", 1) :: Nil)                                                |>
            List.toDelayMap                                             |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 1) |>
            DelayMap.insertWith((v1, v2) -> v1 + v2, "b" as & Impure, 3) |>
            DelayMap.toList == ("a", 1) :: ("b", 4) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // insertWith insertWith fusion                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
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
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        let m = List.toDelayMap((1, 1) :: Nil) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "a" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1) |>
        DelayMap.insertWith((v, _) -> { l := "b" :: deref l; v } as & Pure, 1, 1);
        let listBeforeEvaluation = deref l;
        let _ = DelayMap.toList(m);
        listBeforeEvaluation == Nil and List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil

}
