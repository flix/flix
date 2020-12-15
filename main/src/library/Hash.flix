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

namespace Hash {

    ///
    /// A type class for types that can be hashed.
    ///
    pub class Hash[a] {
        ///
        /// Returns a hash value for the given x.
        ///
        pub def hash(x: a): Int
    }

    instance Hash[Unit] {
        def hash(x: Unit): Int = 0
    }

    instance Hash[Bool] {
        def hash(x: Bool): Int = match x {
            case true => 1231
            case false => 1237
        }
    }

    instance Hash[Char] {
        def hash(x: Char): Int =
            import java.lang.Character:hashCode(Char);
            hashCode(x) as & Pure
    }

    instance Hash[Float32] {
        def hash(x: Float32): Int =
            import java.lang.Float:hashCode(Float32);
            hashCode(x) as & Pure
    }

    instance Hash[Float64] {
        def hash(x: Float64): Int =
            import java.lang.Double:hashCode(Float64);
            hashCode(x) as & Pure
    }

    instance Hash[Int8] {
        def hash(x: Int8): Int =
            import java.lang.Byte:hashCode(Int8);
            hashCode(x) as & Pure
    }

    instance Hash[Int16] {
        def hash(x: Int16): Int =
            import java.lang.Short:hashCode(Int16);
            hashCode(x) as & Pure
    }

    instance Hash[Int32] {
        def hash(x: Int32): Int =
            import java.lang.Integer:hashCode(Int32);
            hashCode(x) as & Pure
    }

    instance Hash[Int64] {
        def hash(x: Int64): Int =
            import java.lang.Long:hashCode(Int64);
            hashCode(x) as & Pure
    }

    instance Hash[String] {
        def hash(x: String): Int =
            import java.lang.String.hashCode();
            hashCode(x) as & Pure
    }

    instance Hash[BigInt] {
        def hash(x: BigInt): Int =
            import java.math.BigInteger.hashCode();
            hashCode(x) as & Pure
    }

    instance Hash[Option[a]] with [a : Hash] {
        def hash(o: Option[a]): Int = match o {
            case Some(y) => Hash/Hash.hash(y)
            case None => 0
        }
    }

    instance Hash[List[a]] with [a : Hash] {
        def hash(l: List[a]): Int =
            List.foldLeft(
                acc -> next -> 31 * acc + Hash/Hash.hash(next),
                1,
                l
            )
    }

    instance Hash[Nel[a]] with [a : Hash] {
        def hash(l: Nel[a]): Int = match l {
            case Nel(x, xs) => 31 * Hash/Hash.hash(xs) + Hash/Hash.hash(x)
        }
    }

    instance Hash[Result[t, e]] with [t : Hash, e : Hash] {
        def hash(r: Result[t, e]): Int = match r {
            case Ok(v)  => Hash/Hash.hash(v)
            case Err(v) => Hash/Hash.hash(v)
        }
    }

    instance Hash[Validation[t, e]] with [t : Hash, e : Hash] {
        def hash(x: Validation[t, e]): Int = match x {
            case Success(v) => Hash/Hash.hash(v)
            case Failure(v) => Hash/Hash.hash(v)
        }
    }

    // TODO: Move instances to proper namespaces, e.g. to Option, List, etc.
    // TODO: Move tests from TestHash into proper namespaces/files, e.g. TestOption, TestList, etc.
    // TODO: Add instances for Array, Map, MutList, MutMap, MutSet, Set
    // TODO: Write more systematic tests that use different types (?).

}
