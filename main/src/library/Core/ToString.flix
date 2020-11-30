/*
 *  Copyright 2020 Matthew Lutze
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

namespace ToString {

    ///
    /// A type class for types that can be converted to strings.
    ///
    class ToString[a] {
        ///
        /// Returns a string representation of the given x.
        ///
        pub def toString(x: a): String
    }

    instance ToString[Unit] {
        def toString(x: Unit): String = "()"
    }

    instance ToString[Bool] {
        def toString(x: Bool): String = match x {
            case true  => "true"
            case false => "false"
        }
    }

    instance ToString[Char] {
        def toString(x: Char): String = Char.toString(x)
    }

    instance ToString[Float32] {
        def toString(x: Float32): String = Float32.toString(x)
    }

    instance ToString[Float64] {
        def toString(x: Float64): String = Float64.toString(x)
    }

    instance ToString[Int8] {
        def toString(x: Int8): String = Int8.toString(x)
    }

    instance ToString[Int16] {
        def toString(x: Int16): String = Int16.toString(x)
    }

    instance ToString[Int32] {
        def toString(x: Int32): String = Int32.toString(x)
    }

    instance ToString[Int64] {
        def toString(x: Int64): String = Int64.toString(x)
    }

    instance ToString[String] {
        def toString(x: String): String = x
    }

    instance ToString[BigInt] {
        def toString(x: BigInt): String = BigInt.toString(x)
    }

    instance ToString[Option[a]] with [a : ToString] {
        def toString(o: Option[a]): String = match o {
            case Some(y) => "Some(${toString(y)})"
            case None => "None"
        }
    }

    instance ToString[List[a]] with [a : ToString] {
        def toString(l: List[a]): String = {
            let s = l |> List.map(toString) |> List.intersperse(" :: ") |> String.flatten;
            String.concat(s, " Nil")
        }
    }

    instance ToString[Nel[a]] with [a : ToString, List[a] : ToString] {
        def toString(l: Nel[a]): String = match l {
            case Nel(x, xs) => "Nel(${x}, ${xs})"
        }
    }

    instance ToString[Result[t, e]] with [t : ToString, e : ToString] {
        def toString(r: Result[t, e]): String = match r {
            case Ok(v)  => "Ok(#{v})"
            case Err(v) => "Err(#{v})"
        }
    }

    instance ToString[Validation[t, e]] with [t : ToString, Nel[e] : ToString] {
        def toString(x: Validation[t, e]): String = match x {
            case Success(v) => "Success(${v})"
            case Failure(v) => "Failure(${v})"
        }
    }

}
