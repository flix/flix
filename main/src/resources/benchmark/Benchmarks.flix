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
    "Iterator.filter"    => () -> region rc { Vector.range(0, size()) |> Vector.iterator(rc) |> Iterator.filter(x -> Int32.modulo(x, 2) == 0) |> Iterator.toVector |> blackhole },
    "Iterator.foldLeft"  => () -> region rc { Vector.range(0, size()) |> Vector.iterator(rc) |> Iterator.foldLeft(Add.add, 0) |> blackhole },
    "Iterator.foldRight" => () -> region rc { Vector.range(0, size()) |> Vector.iterator(rc) |> Iterator.foldRight(Add.add, 0) |> blackhole },
    "Iterator.map"       => () -> region rc { Vector.range(0, size()) |> Vector.iterator(rc) |> Iterator.map(x -> x + 1) |> Iterator.toVector |> blackhole },
    "List.filter"        => () -> List.range(0, size()) |> List.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole,
    "List.foldLeft"      => () -> List.range(0, size()) |> List.foldLeft(Add.add, 0) |> blackhole,
    "List.foldRight"     => () -> List.range(0, size()) |> List.foldRight(Add.add, 0) |> blackhole,
    "List.map"           => () -> List.range(0, size()) |> List.map(x -> x + 1) |> blackhole,
    "Vector.filter"      => () -> Vector.range(0, size()) |> Vector.filter(x -> Int32.modulo(x, 2) == 0) |> blackhole,
    "Vector.foldLeft"    => () -> Vector.range(0, size()) |> Vector.foldLeft(Add.add, 0) |> blackhole,
    "Vector.foldRight"   => () -> Vector.range(0, size()) |> Vector.foldRight(Add.add, 0) |> blackhole,
    "Vector.map"         => () -> Vector.range(0, size()) |> Vector.map(x -> x + 1) |> blackhole
}

///
/// Returns the size of the collections to use.
///
def size(): Int32 = 1_000
