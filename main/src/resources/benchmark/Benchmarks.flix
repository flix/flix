/*
 * Copyright 2024 Magnus Madsen, Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
