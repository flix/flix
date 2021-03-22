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

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    pub def lift1[a: Boxable, t: Boxable](f: a -> t): Boxed -> Boxed =
        (a: Boxed) -> box(f(unbox(a)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift2[a: Boxable, b: Boxable, t: Boxable](f: a -> b -> t): (Boxed, Boxed) -> Boxed =
        (a: Boxed, b: Boxed) -> box(f(unbox(a), unbox(b)))

// TODO: Add for N up to 5

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    pub def lift1b[a: Boxable](f: a -> Bool): Boxed -> Bool =
        (a: Boxed) -> f(unbox(a))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift2b[a: Boxable, b: Boxable](f: a -> b -> Bool): (Boxed, Boxed) -> Bool =
        (a: Boxed, b: Boxed) -> f(unbox(a), unbox(b))

    // TODO: Add for N up to 5

}

instance Boxable[Unit] {
    pub def box(x: Unit): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as Unit, o2 as Unit);
        let toString = o -> ToString.toString(o as Unit);
        BoxedObject(value, compare, toString)
    }
    pub def unbox(_: Boxed): Unit = ()
}

instance Boxable[Bool] {
    pub def box(x: Bool): Boxed = BoxedBool(x)
    pub def unbox(x: Boxed): Bool = match x {
        case BoxedBool(v) => v
        case _            => ?bug
    }
}

instance Boxable[Char] {
    pub def box(x: Char): Boxed = BoxedChar(x)
    pub def unbox(x: Boxed): Char = match x {
        case BoxedChar(v) => v
        case _            => ?bug
    }
}

instance Boxable[Int8] {
    pub def box(x: Int8): Boxed = BoxedInt8(x)
    pub def unbox(x: Boxed): Int8 = match x {
        case BoxedInt8(v) => v
        case _            => ?bug
    }
}

instance Boxable[Int16] {
    pub def box(x: Int16): Boxed = BoxedInt16(x)
    pub def unbox(x: Boxed): Int16 = match x {
        case BoxedInt16(v) => v
        case _             => ?bug
    }
}

instance Boxable[Int32] {
    pub def box(x: Int32): Boxed = BoxedInt32(x)
    pub def unbox(x: Boxed): Int32 = match x {
        case BoxedInt32(v) => v
        case _             => ?bug
    }
}

instance Boxable[Int64] {
    pub def box(x: Int64): Boxed = BoxedInt64(x)
    pub def unbox(x: Boxed): Int64 = match x {
        case BoxedInt64(v) => v
        case _             => ?bug
    }
}

instance Boxable[Float32] {
    pub def box(x: Float32): Boxed = BoxedFloat32(x)
    pub def unbox(x: Boxed): Float32 = match x {
        case BoxedFloat32(v) => v
        case _               => ?bug
    }
}

instance Boxable[Float64] {
    pub def box(x: Float64): Boxed = BoxedFloat64(x)
    pub def unbox(x: Boxed): Float64 = match x {
        case BoxedFloat64(v) => v
        case _               => ?bug
    }
}

instance Boxable[BigInt] {
    pub def box(x: BigInt): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as BigInt, o2 as BigInt);
        let toString = o -> ToString.toString(o as BigInt);
        BoxedObject(value, compare, toString)
    }
    pub def unbox(x: Boxed): BigInt = match x {
        case BoxedObject(v, _, _) => v as BigInt
        case _                    => ?bug
    }
}

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

// TODO: Add types such as Option, List, etc.

// TODO: Add tuples.

// TODO: It would be nice if we could derive instances for user-defined types.
