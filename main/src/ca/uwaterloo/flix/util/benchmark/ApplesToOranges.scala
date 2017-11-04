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

object ApplesToOranges {

  //
  //  def benchmark01(): Bool = List.length(List.range(0, 100 * 1000)) `assertEq!` 100000
  //
  def benchmark01: Boolean = (0 until 100 * 1000).toList.length == 100000

  //
  //  def benchmark02(): Bool = List.length(List.take(50 * 1000, List.range(0, 100 * 1000))) `assertEq!` 50000
  //
  def benchmark02: Boolean = (0 until 100 * 1000).toList.take(50 * 1000).length == 50000

  //
  //  def benchmark03(): Bool = List.length(List.drop(50 * 1000, List.range(0, 100 * 1000))) `assertEq!` 50000
  //
  def benchmark03: Boolean = (0 until 100 * 1000).toList.drop(50 * 1000).length == 50000

  //
  //  def benchmark04(): Bool = List.length(List.reverse(List.range(0, 100 * 1000))) `assertEq!` 100000
  //
  def benchmark04: Boolean = (0 until 100 * 1000).toList.reverse.length == 100000

  //
  //  def benchmark05(): Bool = List.length(List.filter(x -> x % 2 == 0, List.range(0, 100 * 1000))) `assertEq!` 50000
  //
  def benchmark05: Boolean = (0 until 100 * 1000).toList.filter(x => x % 2 == 0).length == 50000

  //
  //  def benchmark06(): Bool = List.length(List.range(0, 100 * 1000) ::: List.range(0, 100 * 1000)) `assertEq!` 200000
  //
  def benchmark06: Boolean = ((0 until 100 * 1000).toList ::: (0 until 100 * 1000).toList).length == 200000

  //
  //  def benchmark07(): Bool = List.length(List.map(x -> x + 1, List.range(0, 100 * 1000))) `assertEq!` 100000
  //
  def benchmark07: Boolean = (0 until 100 * 1000).toList.map(x => x + 1).length == 100000

  //
  //  def benchmark08(): Bool = List.length(List.flatMap(x -> List.repeat(x, 100), List.range(0, 1000))) `assertEq!` 100000
  //
  def benchmark08: Boolean = (0 until 1000).toList.flatMap(x => List.fill(100)(x)).length == 100000

  //
  //  def benchmark09(): Bool = List.foldLeft((x, y) -> x + y, 0, List.range(0, 100 * 1000)) `assertEq!` 704982704
  //
  def benchmark09: Boolean = (0 until 100 * 1000).toList.foldLeft(0) { case (x, y) => x + y } == 704982704

  //
  //  def benchmark10(): Bool = List.foldRight((x, y) -> x + y, 0, List.range(0, 100 * 1000)) `assertEq!` 704982704
  //
  def benchmark10: Boolean = (0 until 100 * 1000).toList.foldRight(0) { case (x, y) => x + y } == 704982704

  //
  //  def benchmark11(): Bool = List.length(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000))) `assertEq!` 100000
  //
  def benchmark11: Boolean = (0 until 100 * 1000).toList.zip((0 until 100 * 1000).toList).length == 100000

  //
  //  def benchmark12(): Bool = let (xs, ys) = List.unzip(List.map(x -> (x, 2 * x), List.range(0, 100 * 1000))); (List.length(xs) + List.length(ys)) `assertEq!` 200000
  //
  def benchmark12: Boolean = {
    val (xs, ys) = (0 until 100 * 1000).toList.map(x => (x, 2 * x)).unzip
    xs.length + ys.length == 200000
  }

  //
  //  def benchmark13(): Bool = let xs = List.range(0, 1000); let ys = List.range(0, 1000); let zs = List.map(y -> List.memberOf(y, xs), ys); List.length(zs) `assertEq!` 1000
  //
  def benchmark13: Boolean = {
    val xs = (0 until 1000).toList
    val ys = (0 until 1000).toList
    val zs = xs.map(y => ys.contains(y))
    zs.length == 1000
  }

  //
  //  def benchmark14(): Bool = let xs = List.range(0, 1000); let ys = List.range(0, 1000); let zs = List.map(y -> List.exists(x -> x * x == y, xs), ys); List.length(zs) `assertEq!` 1000
  //
  def benchmark14: Boolean = {
    val xs = (0 until 1000).toList
    val ys = (0 until 1000).toList
    val zs = ys.map(y => xs.exists(x => x * x == y))
    zs.length == 1000
  }

  //
  //  def benchmark15(): Bool = List.length(List.intersperse(42, List.range(0, 100 * 1000))) `assertEq!` 199999
  //
  // TODO: Omitted. No intersperse.
  def benchmark15: Boolean = true

  //
  //  def benchmark16(): Bool = let (xs, ys) = List.unzip(List.zip(List.range(0, 100 * 1000), List.range(0, 100 * 1000))); (List.length(xs) + List.length(ys)) `assertEq!` 200000
  //
  def benchmark16: Boolean = {
    val (xs, ys) = (0 until 100 * 1000).toList.zip((0 until 100 * 1000).toList).unzip
    xs.length + ys.length == 200000
  }

  //
  //  def benchmark17(): Bool = List.findLeft(x -> x == 50 * 1000, List.range(0, 100 * 1000)) `assertEq!` Some(50000)
  //
  def benchmark17: Boolean = (0 until 100 * 1000).toList.find(x => x == 50 * 1000) == Some(50000)

  //
  //  def benchmark18(): Bool = List.findRight(x -> x == 50 * 1000, List.range(0, 100 * 1000)) `assertEq!` Some(50000)
  //
  // TODO: Omitted. No findRight.
  def benchmark18: Boolean = true

  //
  //  def benchmark19(): Bool =
  //    let f = (x, y) -> match y with {
  //    case Celsius(z) => x + z
  //  };
  //  List.foldLeft(f, 0, List.map(Celsius, List.range(0, 100 * 1000))) `assertEq!` 704982704
  //
  def benchmark19: Boolean = {
    val f: (Int, Celsius) => Int = {
      case (x: Int, y: Celsius) => y match {
        case Celsius(z) => x + z
      }
    }
    (0 until 100 * 1000).toList.map(Celsius).foldLeft(0)(f) == 704982704
  }

  //
  //  def benchmark20(): Bool =
  //    let f = (x, y) ->
  //      let Celsius(a) = x;
  //      let Celsius(b) = y;
  //        Celsius(a + b);
  //  List.foldLeft(f, Celsius(0), List.map(Celsius, List.range(0, 100 * 1000)))  `assertEq!` Celsius(704982704)
  //
  def benchmark20: Boolean = {
    val f: (Celsius, Celsius) => Celsius = {
      case (x: Celsius, y: Celsius) => {
        val Celsius(a) = x
        val Celsius(b) = y
        Celsius(a + b)
      }
    }
    (0 until 100 * 1000).toList.map(Celsius).foldLeft(Celsius(0))(f) == Celsius(704982704)
  }

  final case class Celsius(t: Int) extends AnyVal

}
