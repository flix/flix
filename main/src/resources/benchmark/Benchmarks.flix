/*
 * Copyright 2024 Magnus Madsen, Jakob Schneider Villumsen
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

///
/// Returns the list of benchmarks.
///
/// A benchmark has a name and a function to evaluate.
///
def benchmarks(): Map[String, Unit -> Unit \ IO] = Map#{
    "Iterator.filter"    => () -> region rc { Iterator.range(rc, 0, 10_000) |> Iterator.filter(x -> Int32.modulo(x, 2) == 0) |> Iterator.toVector |> blackhole },
    "Iterator.filterMap" => () -> region rc { Iterator.range(rc, 0, 10_000) |> Iterator.filterMap(x -> if (Int32.remainder(x, 2) == 0) Some(x) else None) |> Iterator.toVector |> blackhole },
    "Iterator.foldLeft"  => () -> region rc { Iterator.range(rc, 0, 10_000) |> Iterator.foldLeft(Add.add, 0) |> blackhole },
    "Iterator.foldRight" => () -> region rc { Iterator.range(rc, 0, 10_000) |> Iterator.foldRight(Add.add, 0) |> blackhole },
    "Iterator.map"       => () -> region rc { Iterator.range(rc, 0, 10_000) |> Iterator.map(x -> x + 1) |> Iterator.toVector |> blackhole },
    "List.filter"        => () -> List.range(0, 10_000) |> List.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole,
    "List.foldLeft"      => () -> List.range(0, 10_000) |> List.foldLeft(Add.add, 0) |> blackhole,
    "List.foldRight"     => () -> List.range(0, 10_000) |> List.foldRight(Add.add, 0) |> blackhole,
    "List.map"           => () -> List.range(0, 10_000) |> List.map(x -> x + 1) |> blackhole,
    "List.length"        => () -> List.range(0, 10_000) |> List.length |> blackhole,
    "List.reverse"       => () -> List.range(0, 10_000) |> List.reverse |> blackhole,
    "List.filterMap"     => () -> List.range(0, 10_000) |> List.filterMap(x -> if (Int32.remainder(x, 2) == 0) Some(x) else None) |> blackhole,
    "Map.filter"         => () -> List.range(0, 100) |> List.toMapWith(k -> k) |> Map.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole,
    "Map.foldLeft"       => () -> List.range(0, 100) |> List.toMapWith(k -> k) |> Map.foldLeft(Add.add, 0) |> blackhole,
    "Map.foldRight"      => () -> List.range(0, 100) |> List.toMapWith(k -> k) |> Map.foldRight(Add.add, 0) |> blackhole,
    "Set.filter"         => () -> Set.range(0, 100) |> Set.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole,
    "Set.foldLeft"       => () -> Set.range(0, 100) |> Set.foldLeft(Add.add, 0) |> blackhole,
    "Set.foldRight"      => () -> Set.range(0, 100) |> Set.foldRight(Add.add, 0) |> blackhole
}
