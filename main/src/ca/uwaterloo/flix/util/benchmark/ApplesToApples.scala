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

  def exists[a](f: a => Boolean, xs: List[a]): Boolean = xs match {
    case Nil => false
    case x :: rs => if (f(x)) true else exists(f, rs)
  }

  def filter[a](f: a => Boolean, xs: List[a]): List[a] = xs match {
    case Nil => Nil
    case x :: rs =>
      val r = filter(f, rs);
      if (f(x)) ::(x, r) else r
  }

  def findLeft[a](f: a => Boolean, xs: List[a]): Option[a] = xs match {
    case Nil => None
    case x :: rs => if (f(x)) Some(x) else findLeft(f, rs)
  }

  def findRight[a](f: a => Boolean, xs: List[a]): Option[a] = xs match {
    case Nil => None
    case x :: rs => findRight(f, rs).orElse(if (f(x)) Some(x) else None)
  }

  def foldLeft[a, b](f: (b, a) => b, s: b, xs: List[a]): b = xs match {
    case Nil => s
    case x :: rs => foldLeft(f, f(s, x), rs)
  }

  def foldRight[a, b](f: (a, b) => b, s: b, xs: List[a]): b = xs match {
    case Nil => s
    case x :: rs => f(x, foldRight(f, s, rs))
  }

  def intersperse[a](a: a, xs: List[a]): List[a] = xs match {
    case x1 :: x2 :: rs => ::(x1, ::(a, intersperse(a, ::(x2, rs))))
    case _ => xs
  }

  def length[a](xs: List[a]): Int = xs match {
    case Nil => 0
    case x :: rs => 1 + length(rs)
  }

  def map[a, b](f: a => b, xs: List[a]): List[b] = xs match {
    case Nil => Nil
    case x :: rs => ::(f(x), map(f, rs))
  }

  def memberOf[a](a: a, xs: List[a]): Boolean = xs match {
    case Nil => false
    case x :: rs => if (x == a) true else memberOf(a, rs)
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

  def zip[a, b](xs: List[a], ys: List[b]): List[(a, b)] = (xs, ys) match {
    case (x :: rs, y :: qs) => ::((x, y), zip(rs, qs))
    case _ => Nil
  }

  def unzip[a, b](xs: List[(a, b)]): (List[a], List[b]) = xs match {
    case Nil => (Nil, Nil)
    case (x1, x2) :: rs =>
      val (r1, r2) = unzip(rs);
      (::(x1, r1), ::(x2, r2))
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

  def benchmark11(): Boolean = length(zip(range(0, 100 * 1000), range(0, 100 * 1000))) == 100000

  def benchmark12(): Boolean = {
    val (xs, ys) = unzip(map[Int, (Int, Int)](x => (x, 2 * x), range(0, 100 * 1000)))
    (length(xs) + length(ys)) == 200000
  }

  def benchmark13(): Boolean = {
    val xs = range(0, 1000)
    val ys = range(0, 1000)
    val zs = map[Int, Boolean](y => memberOf(y, xs), ys)
    length(zs) == 1000
  }

  def benchmark14(): Boolean = {
    val xs = range(0, 1000);
    val ys = range(0, 1000);
    val zs = map[Int, Boolean](y => exists[Int](x => x * x == y, xs), ys);
    length(zs) == 1000
  }

  def benchmark15(): Boolean = length(intersperse(42, range(0, 100 * 1000))) == 199999

  def benchmark16(): Boolean = {
    val (xs, ys) = unzip(zip(range(0, 100 * 1000), range(0, 100 * 1000)));
    (length(xs) + length(ys)) == 200000
  }

  def benchmark17(): Boolean = findLeft[Int](x => x == 50 * 1000, range(0, 100 * 1000)) == Some(50000)

  def benchmark18(): Boolean = findRight[Int](x => x == 50 * 1000, range(0, 100 * 1000)) == Some(50000)

  def benchmark19(): Boolean = {
    val f: (Int, Celsius) => Int = (x, y) => y match {
      case Celsius(z) => x + z
    }
    foldLeft(f, 0, map(Celsius, range(0, 100 * 1000))) == 704982704
  }

  def benchmark20(): Boolean = {
    val f: (Celsius, Celsius) => Celsius = (x, y) => {
      val Celsius(a) = x
      val Celsius(b) = y
      Celsius(a + b)
    }
    foldLeft(f, Celsius(0), map(Celsius, range(0, 100 * 1000))) == Celsius(704982704)
  }

  final case class Celsius(t: Int) extends AnyVal

}
