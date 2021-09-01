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
lawless class Boxable[a] with Order[a], ToString[a] {

    ///
    /// Boxes the given `x`.
    ///
    pub def box(x: a): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as a, o2 as a);
        let toString = o -> ToString.toString(o as a);
        BoxedObject(value, compare, toString)
    }

    ///
    /// Unboxes the given `x`.
    ///
    pub def unbox(x: Boxed): a = match x {
        case BoxedObject(v, _, _) => v as a
        case _                    => ?bug
    }

}

namespace Boxable {

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift1(f: t1 -> t): Boxed -> Boxed with Boxable[t1], Boxable[t] =
        (b1: Boxed) -> box(f(unbox(b1)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift2(f: t1 -> t2 -> t): Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> box(f(unbox(b1), unbox(b2)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift3(f: t1 -> t2 -> t3 -> t): Boxed -> Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift4(f: t1 -> t2 -> t3 -> t4 -> t): Boxed -> Boxed -> Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3), unbox(b4)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift5(f: t1 -> t2 -> t3 -> t4 -> t5 -> t): Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> (b5: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3), unbox(b4), unbox(b5)))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    pub def lift1b(f: t1 -> Bool): Boxed -> Bool with Boxable[t1] =
        (b1: Boxed) -> f(unbox(b1))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift2b(f: t1 -> t2 -> Bool): Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2] =
        (b1: Boxed) -> (b2: Boxed) -> f(unbox(b1), unbox(b2))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift3b(f: t1 -> t2 -> t3 -> Bool): Boxed -> Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2], Boxable[t3] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift4b(f: t1 -> t2 -> t3 -> t4 -> Bool): Boxed -> Boxed -> Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3), unbox(b4))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    pub def lift5b(f: t1 -> t2 -> t3 -> t4 -> t5 -> Bool): Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> (b5: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3), unbox(b4), unbox(b5))

}

instance Boxable[Unit] {
    pub override def box(x: Unit): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as Unit, o2 as Unit);
        let toString = o -> ToString.toString(o as Unit);
        BoxedObject(value, compare, toString)
    }
    pub override def unbox(_: Boxed): Unit = ()
}

instance Boxable[Bool] {
    pub override def box(x: Bool): Boxed = BoxedBool(x)
    pub override def unbox(x: Boxed): Bool = match x {
        case BoxedBool(v) => v
        case _            => ?bug
    }
}

instance Boxable[Char] {
    pub override def box(x: Char): Boxed = BoxedChar(x)
    pub override def unbox(x: Boxed): Char = match x {
        case BoxedChar(v) => v
        case _            => ?bug
    }
}

instance Boxable[Int8] {
    pub override def box(x: Int8): Boxed = BoxedInt8(x)
    pub override def unbox(x: Boxed): Int8 = match x {
        case BoxedInt8(v) => v
        case _            => ?bug
    }
}

instance Boxable[Int16] {
    pub override def box(x: Int16): Boxed = BoxedInt16(x)
    pub override def unbox(x: Boxed): Int16 = match x {
        case BoxedInt16(v) => v
        case _             => ?bug
    }
}

instance Boxable[Int32] {
    pub override def box(x: Int32): Boxed = BoxedInt32(x)
    pub override def unbox(x: Boxed): Int32 = match x {
        case BoxedInt32(v) => v
        case _             => ?bug
    }
}

instance Boxable[Int64] {
    pub override def box(x: Int64): Boxed = BoxedInt64(x)
    pub override def unbox(x: Boxed): Int64 = match x {
        case BoxedInt64(v) => v
        case _             => ?bug
    }
}

instance Boxable[Float32] {
    pub override def box(x: Float32): Boxed = BoxedFloat32(x)
    pub override def unbox(x: Boxed): Float32 = match x {
        case BoxedFloat32(v) => v
        case _               => ?bug
    }
}

instance Boxable[Float64] {
    pub override def box(x: Float64): Boxed = BoxedFloat64(x)
    pub override def unbox(x: Boxed): Float64 = match x {
        case BoxedFloat64(v) => v
        case _               => ?bug
    }
}

instance Boxable[BigInt]

instance Boxable[String]

instance Boxable[(a, b)]

instance Boxable[(a, b, c)]

instance Boxable[(a, b, c, d)]

instance Boxable[(a, b, c, d, e)]

instance Boxable[(a, b, c, d, e, f)]

instance Boxable[(a, b, c, d, e, f, g)]

instance Boxable[(a, b, c, d, e, f, g, h)]

instance Boxable[(a, b, c, d, e, f, g, h, i)]

instance Boxable[(a, b, c, d, e, f, g, h, i, j)]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k)]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l)]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m)]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m, n)]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)]
