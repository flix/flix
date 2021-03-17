/*
 *  Copyright 2021 Magnus Madsen
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

///
/// A type class for types that can be boxed.
///
lawless class Boxable[a] {

    ///
    /// Boxes the given `x`.
    ///
    pub def box(x: a): Boxed

    ///
    /// Unboxes the given `x`.
    ///
    pub def unbox(x: Boxed): a

}

namespace Boxable {

    pub def lift1[a: Boxable, t: Boxable](f: a -> t): Boxed -> Boxed =
        (a: Boxed) -> box(f(unbox(a)))

    pub def lift1b[a: Boxable](f: a -> Bool): Boxed -> Bool =
        (a: Boxed) -> f(unbox(a))

    pub def lift2b[a: Boxable, b: Boxable](f: (a, b) -> Bool): (Boxed, Boxed) -> Bool =
        (a: Boxed, b: Boxed) -> f(unbox(a), unbox(b))


}

instance Boxable[Int32] {
    pub def box(x: Int32): Boxed = BoxedInt32(x)
    pub def unbox(x: Boxed): Int32 = match x {
        case BoxedInt32(v) => v
        case _             => ?bug
    }
}

// TODO: Just construct these automatically? Or demand them?

// TODO: Add most common types.

instance Boxable[String] {
    pub def box(x: String): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as String, o2 as String);
        let toString = o -> ToString.toString(o as String);
        BoxedObject(value, compare, toString)
    }
    pub def unbox(x: Boxed): String= match x {
        case BoxedObject(v, _, _) => v as String
        case _                    => ?bug
    }
}

// TODO: User-defined instances.. Do anything automatically?
