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

namespace FromString {
    class FromString[a] {
        pub def fromString(x: String): Result[a, String]
    }

    instance FromString[String] {
        def fromString(x: String): Result[String, String] = Ok(x)
    }

    instance FromString[Int8] {
        def fromString(x: String): Result[Int8, String] = Int8.fromString(x)
    }

    instance FromString[Int16] {
        def fromString(x: String): Result[Int16, String] = Int16.fromString(x)
    }

    instance FromString[Int32] {
        def fromString(x: String): Result[Int32, String] = Int32.fromString(x)
    }

    instance FromString[Int64] {
        def fromString(x: String): Result[Int64, String] = Int64.fromString(x)
    }

    instance FromString[BigInt] {
        def fromString(x: String): Result[BigInt, String] = BigInt.fromString(x)
    }

    instance FromString[Float32] {
        def fromString(x: String): Result[Float32, String] = Float32.fromString(x)
    }

    instance FromString[Float64] {
        def fromString(x: String): Result[Float64, String] = Float64.fromString(x)
    }
}