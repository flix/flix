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

package ca.uwaterloo.flix.runtime.Benchmark

object ScalaAppleToAppleListBenchmarkTests {

  val tests: List[() => Any] = List(
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

import ca.uwaterloo.flix.runtime.Benchmark.FlixListInScala._

  ///
  /// create a list of integers and compute the length of the result.
  ///
  private def benchmark01(): Int = length(range(0, 100 * 1000))


  ///
  /// create a list of integers and return the first half the elements.
  ///
  private def benchmark02(): Int = length(take(50 * 1000, range(0, 100 * 1000)))
  ///
  /// create a list of integers and return the last half the elements.
  ///
  private def benchmark03(): Int = length(drop(50 * 1000, range(0, 100 * 1000)))

  ///
  /// create a list of integers, reverse it, and compute the length of the result.
  ///
  private def benchmark04(): Int = length(reverse(range(0, 100 * 1000)))

  ///
  /// create a list of integers, filter all its even numbers, and compute the length of the result.
  ///
  private def benchmark05(): Int = length(filter[Int](x => x % 2 == 0, range(0, 100 * 1000)))

  ///
  /// create a list of integers, append it to a list of 1 million integers, and compute the length of the result.
  ///
  private def benchmark06(): Int = length(append(range(0, 100 * 1000), range(0, 100 * 1000)))

  ///
  /// create a list of integers, increment each integer by one, and compute the length of the result.
  ///
  private def benchmark07(): Int = length(map[Int, Int](x => x + 1, range(0, 100 * 1000)))

  ///
  /// create a list of integers, flatMap it over lists of integers, and compute the length of the result.
  ///
  private def benchmark08(): Int = length(flatMap[Int, Int](x => repeat(100, x), range(0, 1000)))

  ///
  /// create a list of integers and compute its sum via foldLeft.
  ///
  private def benchmark09(): Int = foldLeft[Int, Int]((x, y) => x + y, 0, range(0, 100 * 1000))

  ///
  /// create a list of integers and compute its sum via foldRight.
  ///
  private def benchmark10(): Int = foldRight[Int, Int]((x, y) => x + y, 0, range(0, 100 * 1000))

  ///
  /// create two lists of integers, zip them, and compute the length of the result.
  ///
  private def benchmark11(): Int = length(zip(range(0, 100 * 1000), range(0, 100 * 1000)))

  ///
  /// create a list of pairs, unzip it, and compute the length of the result.
  ///
  private def benchmark12(): Int = {
    val (xs, xy) = unzip[Int,Int](map[Int, (Int, Int)](x => (x, 2 * x), range(0, 100 * 1000)))
    length(xs) + length(xy)
  }

  ///
  /// create two lists of integers, check that each integer of the first list exists in the latter list.
  ///
  private def benchmark13(): Int = {
    val xs = range(0, 1000)
    val ys = range(0, 1000)
    val zs = map[Int, Boolean](y => memberOf(y, xs), ys)
    length(zs)
  }

  ///
  /// create two lists of integers, check if the cube of each integer in the first list exists in the latter list.
  ///
  private def benchmark14(): Int = {
    val xs = range(0, 1000)
    val ys = range(0, 1000)
    val zs = map[Int, Boolean](y => exists[Int](x => x * x == y, xs), ys)
    length(zs)
  }

  ///
  /// intersperses an integer into a list of integers.
  ///
  private def benchmark15(): Int = length(intersperse[Int](42, range(0, 100 * 1000)))


  ///
  /// create two lists of integers, zip them, unzip them, and compute the length of the result.
  ///
  private def benchmark16(): Int = {
    val (xs, ys) = unzip[Int, Int](zip[Int, Int](range(0, 100 * 1000), range(0, 100 * 1000)))
    length(xs) + length(ys)
  }

  ///
  /// creates a list and searches for an item from the left.
  ///
  private def benchmark17(): Option[Int] = findLeft[Int](x => x == 50 * 1000, range(0, 100 * 1000))

  ///
  /// creates a list and searches for an item from the right.
  ///
  private def benchmark18(): Option[Int] = findRight[Int](x => x == 50 * 1000,range(0, 100 * 1000))
}