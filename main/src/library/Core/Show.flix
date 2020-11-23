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

namespace Show {
    class Show[a] {
        pub def show(x: a): String
    }

    instance Show[Unit] {
        def show(x: Unit): String = "()"
    }

    instance Show[Bool] {
        def show(x: Bool): String = match x {
            case true => "true"
            case false => "false"
        }
    }

    instance Show[String] {
        def show(x: String): String = x
    }

    instance Show[Int8] {
        def show(x: Int8): String = Int8.toString(x)
    }

    instance Show[Int16] {
        def show(x: Int16): String = Int16.toString(x)
    }

    instance Show[Int32] {
        def show(x: Int32): String = Int32.toString(x)
    }

    instance Show[Int64] {
        def show(x: Int64): String = Int64.toString(x)
    }

    instance Show[BigInt] {
        def show(x: BigInt): String = BigInt.toString(x)
    }

    instance Show[Float32] {
        def show(x: Float32): String = Float32.toString(x)
    }

    instance Show[Float64] {
        def show(x: Float64): String = Float64.toString(x)
    }

    instance Show[List[a]] with [a : Show] { // TODO nested lists are ambiguous (?)
        def show(xs: List[a]): String = {
            let strings = List.map(x -> String.concat(show(x), " :: "), xs);
            let start = String.flatten(strings);
            String.concat(start, "Nil")
        }
    }

    instance Show[Option[a]] with [a : Show] {
        def show(x: Option[a]): String = match x {
            case Some(y) => "Some(${show(y)})"
            case None => "None"
        }
    }

    instance Show[Result[a, b]] with [a : Show, b : Show] {
        def show(x: Result[a, b]): String = match x {
            case Ok(y) => "Ok(${show(y)})"
            case Err(z) => "Err(${show(z)})"
        }
    }

    instance Show[Validation[a, b]] with [a : Show, Nel[b] : Show] {
        def show(x: Validation[a, b]): String = match x {
            case Success(y) => "Success(${show(y)})"
            case Failure(z) => "Failure(${show(z)})"
        }
    }

    instance Show[Nel[a]] with [List[a] : Show] {
        def show(x: Nel[a]): String = show(Nel.toList(x))
    }
}