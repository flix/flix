/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.util.benchmark

object ApplesToApples {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class ::[A](x: A, xs: List[A]) extends List[A]

  def append[a](xs: List[a], ys: List[a]): List[a] = xs match {
    case Nil => ys
    case x :: rs => ::(x, append(rs, ys))
  }

  def drop[a](n: Int, xs: List[a]): List[a] = if (n < 0) xs else (n, xs) match {
    case (_, Nil) => Nil
    case (0, _) => xs
    case (i, x :: rs) => drop(i - 1, rs)
  }

  def filter[a](f: a => Boolean, xs: List[a]): List[a] = xs match {
    case Nil => Nil
    case x :: rs =>
      val r = filter(f, rs);
      if (f(x)) ::(x, r) else r
  }

  def foldLeft[a, b](f: (b, a) => b, s: b, xs: List[a]): b = xs match {
    case Nil => s
    case x :: rs => foldLeft(f, f(s, x), rs)
  }

  def foldRight[a, b](f: (a, b) => b, s: b, xs: List[a]): b = xs match {
    case Nil => s
    case x :: rs => f(x, foldRight(f, s, rs))
  }

  def length[a](xs: List[a]): Int = xs match {
    case Nil => 0
    case x :: rs => 1 + length(rs)
  }

  def map[a, b](f: a => b, xs: List[a]): List[b] = xs match {
    case Nil => Nil
    case x :: rs => ::(f(x), map(f, rs))
  }

  def flatMap[a, b](f: a => List[b], xs: List[a]): List[b] = xs match {
    case Nil => Nil
    case x :: rs => append(f(x), flatMap(f, rs))
  }

  def range(b: Int, e: Int): List[Int] = if (b >= e) Nil else ::(b, range(b + 1, e))

  def repeat[a](a: a, n: Int): List[a] = if (n <= 0) Nil else ::(a, repeat(a, n - 1))

  def reverse[a](xs: List[a]): List[a] = reverseHelper(xs, Nil)

  def reverseHelper[a](xs: List[a], acc: List[a]): List[a] = xs match {
    case Nil => acc
    case x :: rs => reverseHelper(rs, ::(x, acc))
  }

  def take[a](n: Int, xs: List[a]): List[a] = if (n < 0) Nil else (n, xs) match {
    case (_, Nil) => Nil
    case (0, _) => Nil
    case (i, x :: rs) => ::(x, take(i - 1, rs))
  }

  def benchmark01(): Boolean = length(range(0, 100 * 1000)) == 100000

  def benchmark02(): Boolean = length(take(50 * 1000, range(0, 100 * 1000))) == 50000

  def benchmark03(): Boolean = length(drop(50 * 1000, range(0, 100 * 1000))) == 50000

  def benchmark04(): Boolean = length(reverse(range(0, 100 * 1000))) == 100000

  def benchmark05(): Boolean = length(filter[Int](x => x % 2 == 0, range(0, 100 * 1000))) == 50000

  def benchmark06(): Boolean = length(append[Int](range(0, 100 * 1000), range(0, 100 * 1000))) == 200000

  def benchmark07(): Boolean = length(map[Int, Int](x => x + 1, range(0, 100 * 1000))) == 100000

  def benchmark08(): Boolean = length(flatMap[Int, Int](x => repeat(x, 100), range(0, 1000))) == 100000

  def benchmark09(): Boolean = foldLeft[Int, Int]({ case (x, y) => x + y }, 0, range(0, 100 * 1000)) == 704982704

  def benchmark10(): Boolean = foldRight[Int, Int]({ case (x, y) => x + y }, 0, range(0, 100 * 1000)) == 704982704

  //    ///
  //    /// create two lists of integers, zip them, and compute the length of the result.
  //    ///
  //    @benchmark
  //    def benchmark11(): Bool = List.length(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000))) `assertEq!` 100000
  //
  //    ///
  //    /// create a list of pairs, unzip it, and compute the length of the result.
  //    ///
  //    @benchmark
  //    def benchmark12(): Bool =
  //      let (xs, ys) = List.unzip(List.map(x -> (x, 2 * x), List.range(0, 100 * 1000)));
  //    (List.length(xs) + List.length(ys)) `assertEq!` 200000
  //
  //    ///
  //    /// create two lists of integers, check that each integer of the first list exists in the latter list.
  //    ///
  //    @benchmark
  //    def benchmark13(): Bool =
  //      let xs = List.range(0, 1000);
  //    let ys = List.range(0, 1000);
  //    let zs = List.map(y -> List.memberOf(y, xs), ys);
  //    List.length(zs) `assertEq!` 1000
  //
  //    ///
  //    /// create two lists of integers, check if the cube of each integer in the first list exists in the latter list.
  //    ///
  //    @benchmark
  //    def benchmark14(): Bool =
  //      let xs = List.range(0, 1000);
  //    let ys = List.range(0, 1000);
  //    let zs = List.map(y -> List.exists(x -> x * x == y, xs), ys);
  //    List.length(zs) `assertEq!` 1000
  //
  //    ///
  //    /// intersperses an integer into a list of integers.
  //    ///
  //    @benchmark
  //    def benchmark15(): Bool = List.length(List.intersperse(42, List.range(0, 100 * 1000))) `assertEq!` 199999
  //
  //    ///
  //    /// create two lists of integers, zip them, unzip them, and compute the length of the result.
  //    ///
  //    @benchmark
  //    def benchmark16(): Bool =
  //      let (xs, ys) = List.unzip(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000)));
  //    (List.length(xs) + List.length(ys)) `assertEq!` 200000
  //
  //    ///
  //    /// creates a list and searches for an item from the left.
  //    ///
  //    @benchmark
  //    def benchmark17(): Bool = List.findLeft(x -> x == 50 * 1000, List.range(0, 100 * 1000)) `assertEq!` Some(50000)
  //
  //    ///
  //    /// creates a list and searches for an item from the right.
  //    ///
  //    @benchmark
  //    def benchmark18(): Bool = List.findRight(x -> x == 50 * 1000, List.range(0, 100 * 1000)) `assertEq!` Some(50000)
  //
  //    ///
  //    /// creates a list of integers (in celsius) and sums them (as integers).
  //    ///
  //    @benchmark
  //    def benchmark19(): Bool =
  //      let f = (x, y) -> match y with {
  //      case Celsius(z) => x + z
  //    };
  //    List.foldLeft(f, 0, List.map(Celsius, List.range(0, 100 * 1000))) `assertEq!` 704982704
  //
  //    ///
  //    /// creates a list of integers (in celsius) and sums them (as celsius).
  //    ///
  //    @benchmark
  //    def benchmark20(): Bool =
  //      let f = (x, y) ->
  //        let Celsius(a) = x;
  //    let Celsius(b) = y;
  //    Celsius(a + b);
  //    List.foldLeft(f, Celsius(0), List.map(Celsius, List.range(0, 100 * 1000)))  `assertEq!` Celsius(704982704)
  //
  //    // A type safe wrapper of integers.
  //    type Celsius = Celsius(Int)
  //
  //  }


}
