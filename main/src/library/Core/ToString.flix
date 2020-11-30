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
    class ToString[a] {
        pub def toString(x: a): String
    }

    instance ToString[Unit] {
        def toString(x: Unit): String = "()"
    }

    instance ToString[Bool] {
        def toString(x: Bool): String = match x {
            case true => "true"
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

    instance ToString[List[a]] with [a : ToString] {
        def toString(xs: List[a]): String = {
            let strings = List.map(x -> String.concat(toString(x), " :: "), xs);
            let start = String.flatten(strings);
            String.concat(start, "Nil")
        }
    }

    instance ToString[Option[a]] with [a : ToString] {
        def toString(x: Option[a]): String = match x {
            case Some(y) => "Some(${toString(y)})"
            case None => "None"
        }
    }

    instance ToString[Result[a, b]] with [a : ToString, b : ToString] {
        def toString(x: Result[a, b]): String = match x {
            case Ok(y) => "Ok(${toString(y)})"
            case Err(z) => "Err(${toString(z)})"
        }
    }

    instance ToString[Validation[a, b]] with [a : ToString, Nel[b] : ToString] {
        def toString(x: Validation[a, b]): String = match x {
            case Success(y) => "Success(${toString(y)})"
            case Failure(z) => "Failure(${toString(z)})"
        }
    }

    instance ToString[Nel[a]] with [List[a] : ToString] {
        def toString(x: Nel[a]): String = toString(Nel.toList(x))
    }
}