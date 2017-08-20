/*
 *  Copyright 2017 Ramin Zarifi
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.runtime

object ScalaBenchmark extends Benchmarker{

  val WarmupRounds: Int = 100

  val ActualRounds: Int = 50

  val ListBenchmarks: List[() => Any] = List(
    benchmark01,
    benchmark02,
    benchmark03,
    benchmark04,
    benchmark05,
    benchmark06,
    benchmark07,
    benchmark08,
    benchmark09,
    benchmark10,
    benchmark11,
    benchmark12,
    benchmark13,
    benchmark14,
    benchmark15,
    benchmark16,
    benchmark17,
    benchmark18
  )

  def main(args: Array[String]): Unit = {
    Console.println(s"-- Benchmarks for List --")
    Console.println(s"    Warmup Rounds: $WarmupRounds")
    for ((defn, ind) <- ListBenchmarks.zipWithIndex) {
      val index = ind + 1
      val suffix = if(index.toString.length == 1) "0".concat(index.toString) else index.toString
      Console.print("      ")
      Console.print(s"benchmark$suffix")
      Console.print(": ")
      for (i <- 0 until WarmupRounds) {
        Console.print(".")
        defn()
      }
      Console.println()
    }
    Console.println()

    /*
     * Actual Rounds.
     */
    Console.println(s"    Actual Rounds: $ActualRounds")
    Console.println(s"      Name:                Median (ms)")
    for ((defn, ind) <- ListBenchmarks.zipWithIndex) {
      val index = ind + 1
      val suffix = if(index.toString.length == 1) "0".concat(index.toString) else index.toString
      val timings = run(defn, ActualRounds)
      val medianInMiliSeconds = median(timings).toDouble / (1000.0 * 1000.0)
      Console.println(f"      benchmark$suffix $medianInMiliSeconds%20.1f")
      sleepAndGC()
    }

    Console.println()
  }
  ///
  /// create a list of integers and compute the length of the result.
  ///
  def benchmark01(): Int = List.range(0, 100 * 1000).length

  ///
  /// create a list of integers and return the first half the elements.
  ///
  def benchmark02(): Int = List.range(0, 100 * 1000).take(50 * 1000).length
  ///
  /// create a list of integers and return the last half the elements.
  ///
  def benchmark03(): Int = List.range(0, 100 * 1000).drop(50 * 1000).length

  ///
  /// create a list of integers, reverse it, and compute the length of the result.
  ///
  def benchmark04(): Int = List.range(0, 100 * 1000).reverse.length

  ///
  /// create a list of integers, filter all its even numbers, and compute the length of the result.
  ///
  def benchmark05(): Int = List.range(0, 100 * 1000).filter(x => x % 2 == 0).length

  ///
  /// create a list of integers, append it to a list of 1 million integers, and compute the length of the result.
  ///
  def benchmark06(): Int = (List.range(0, 100 * 1000) ::: List.range(0, 100 * 1000)).length

  ///
  /// create a list of integers, increment each integer by one, and compute the length of the result.
  ///
  def benchmark07(): Int = List.range(0, 100 * 1000).map(x => x + 1).length

  ///
  /// create a list of integers, flatMap it over lists of integers, and compute the length of the result.
  ///
  def benchmark08(): Int = List.range(0, 1000).flatMap(x => List.fill(100)(x)).length

  ///
  /// create a list of integers and compute its sum via foldLeft.
  ///
  def benchmark09(): Int = List.range(0, 100 * 1000).foldLeft(0)((x, y) => x + y)

  ///
  /// create a list of integers and compute its sum via foldRight.
  ///
  def benchmark10(): Int = List.range(0, 100 * 1000).foldRight(0)((x, y) => x + y)

  ///
  /// create two lists of integers, zip them, and compute the length of the result.
  ///
  def benchmark11(): Int = List.range(0, 100 * 1000).zip(List.range(0, 100 * 1000)).length

  ///
  /// create a list of pairs, unzip it, and compute the length of the result.
  ///
  def benchmark12(): Int = {
    val (xs, xy) = List.range(0, 100 * 1000).map(x => (x, 2 * x)).unzip
    xs.length + xy.length
  }

  ///
  /// create two lists of integers, check that each integer of the first list exists in the latter list.
  ///
  def benchmark13(): Int = {
    val xs = List.range(0, 1000)
    val ys = List.range(0, 1000)
    val zs = ys.map(y => xs.contains(y))
    zs.length
  }

  ///
  /// create two lists of integers, check if the cube of each integer in the first list exists in the latter list.
  ///
  def benchmark14(): Int = {
    val xs = List.range(0, 1000)
    val ys = List.range(0, 1000)
    val zs = ys.map(y => xs.exists(x => x * x == y))
    zs.length
  }

  ///
  /// intersperses an integer into a list of integers.
  ///
  def benchmark15(): Int = {
    def intersperse[a](a: a, xs: List[a]): List[a] = xs match {
      case x1 :: x2 :: rs => x1 :: a :: intersperse(a, x2 :: rs)
      case _ => xs
    }

    intersperse[Int](42, List.range[Int](0, 100 * 1000)).length
  }

  ///
  /// create two lists of integers, zip them, unzip them, and compute the length of the result.
  ///
  def benchmark16(): Int = {
    val (xs, ys) = List.range(0, 100 * 1000).zip(List.range(0, 100 * 1000)).unzip
    xs.length + ys.length
  }

  ///
  /// creates a list and searches for an item from the left.
  ///
  def benchmark17(): Option[Int] = List.range(0, 100 * 1000).find(x => x == 50 * 1000)

  ///
  /// creates a list and searches for an item from the right.
  ///
  def benchmark18(): Option[Int] = {
    def findRight[a](f: a => Boolean, xs: List[a]): Option[a] = xs match {
      case Nil => None
      case x :: rs => findRight(f, rs).orElse{
        if(f(x)) Some(x) else None
      }
    }

    findRight[Int](x => x == 50 * 1000, List.range[Int](0, 100 * 1000))
  }
}
