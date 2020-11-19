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

namespace Read {
    class Read[a] {
        pub def read(x: String): Result[a, String]
    }

    instance Read[String] {
        def read(x: String): Result[String, String] = Ok(x)
    }

    instance Read[Int8] {
        def read(x: String): Result[Int8, String] = Int8.fromString(x)
    }

    instance Read[Int16] {
        def read(x: String): Result[Int16, String] = Int16.fromString(x)
    }

    instance Read[Int32] {
        def read(x: String): Result[Int32, String] = Int32.fromString(x)
    }

    instance Read[Int64] {
        def read(x: String): Result[Int64, String] = Int64.fromString(x)
    }

    instance Read[BigInt] {
        def read(x: String): Result[BigInt, String] = BigInt.fromString(x)
    }

    instance Read[Float32] {
        def read(x: String): Result[Float32, String] = Float32.fromString(x)
    }

    instance Read[Float64] {
        def read(x: String): Result[Float64, String] = Float64.fromString(x)
    }
}