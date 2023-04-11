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
pub class Boxable[a] with Order[a] {

}

mod Boxable {

    ///
    /// Boxes the given `x`.
    ///
    pub def box(x: a): Boxed with Order[a] = typematch x {
        case v: Bool    => Boxed.BoxedBool(v)
        case v: Char    => Boxed.BoxedChar(v)
        case v: Int8    => Boxed.BoxedInt8(v)
        case v: Int16   => Boxed.BoxedInt16(v)
        case v: Int32   => Boxed.BoxedInt32(v)
        case v: Int64   => Boxed.BoxedInt64(v)
        case v: Float32 => Boxed.BoxedFloat32(v)
        case v: Float64 => Boxed.BoxedFloat64(v)
        case _: _ =>
            let cmp = (o1, o2) -> Order.compare(unchecked_cast(o1 as a), unchecked_cast(o2 as a));
            Boxed.BoxedObject(unchecked_cast(x as ##java.lang.Object), cmp)
    }

    ///
    /// Unboxes the given `x`.
    ///
    pub def unbox(x: Boxed): a = typematch (Proxy.Proxy: Proxy[a]) {
        case _: Proxy[Bool] => match x {
            case Boxed.BoxedBool(v)      => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: Proxy[Char] => match x {
            case Boxed.BoxedChar(v)      => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: Proxy[Int8] => match x {
            case Boxed.BoxedInt8(v)      => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: Proxy[Int16] => match x {
            case Boxed.BoxedInt16(v)     => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: Proxy[Int32] => match x {
            case Boxed.BoxedInt32(v)     => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: Proxy[Int64] => match x {
            case Boxed.BoxedInt64(v)     => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: Proxy[Float32] => match x {
            case Boxed.BoxedFloat32(v)   => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: Proxy[Float64] => match x {
            case Boxed.BoxedFloat64(v)   => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
        case _: _ => match x {
            case Boxed.BoxedObject(v, _) => unchecked_cast(v as a)
            case _                       => unreachable!()
        }
    }

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift1(f: t1 -> t): Boxed -> Boxed with Order[t1], Order[t] =
        (b1: Boxed) -> box(f(unbox(b1)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift2(f: t1 -> t2 -> t): Boxed -> Boxed -> Boxed with Order[t1], Order[t2], Order[t] =
        (b1: Boxed) -> (b2: Boxed) -> box(f(unbox(b1), unbox(b2)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift3(f: t1 -> t2 -> t3 -> t): Boxed -> Boxed -> Boxed -> Boxed with Order[t1], Order[t2], Order[t3], Order[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift4(f: t1 -> t2 -> t3 -> t4 -> t): Boxed -> Boxed -> Boxed -> Boxed -> Boxed with Order[t1], Order[t2], Order[t3], Order[t4], Order[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3), unbox(b4)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift5(f: t1 -> t2 -> t3 -> t4 -> t5 -> t): Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> (b5: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3), unbox(b4), unbox(b5)))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    @Internal
    pub def lift1b(f: t1 -> Bool): Boxed -> Bool with Order[t1] =
        (b1: Boxed) -> f(unbox(b1))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift2b(f: t1 -> t2 -> Bool): Boxed -> Boxed -> Bool with Order[t1], Order[t2] =
        (b1: Boxed) -> (b2: Boxed) -> f(unbox(b1), unbox(b2))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift3b(f: t1 -> t2 -> t3 -> Bool): Boxed -> Boxed -> Boxed -> Bool with Order[t1], Order[t2], Order[t3] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift4b(f: t1 -> t2 -> t3 -> t4 -> Bool): Boxed -> Boxed -> Boxed -> Boxed -> Bool with Order[t1], Order[t2], Order[t3], Order[t4] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3), unbox(b4))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift5b(f: t1 -> t2 -> t3 -> t4 -> t5 -> Bool): Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Bool with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> (b5: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3), unbox(b4), unbox(b5))

}

instance Boxable[Unit] {
}

instance Boxable[Bool] {
}

instance Boxable[Char] {
}

instance Boxable[Int8] {
}

instance Boxable[Int16] {
}

instance Boxable[Int32] {
}

instance Boxable[Int64] {
}

instance Boxable[Float32] {
}

instance Boxable[Float64] {
}

instance Boxable[BigDecimal]

instance Boxable[BigInt]

instance Boxable[String]

instance Boxable[(a, b)] with Boxable[a], Boxable[b]

instance Boxable[(a, b, c)] with Boxable[a], Boxable[b], Boxable[c]

instance Boxable[(a, b, c, d)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d]

instance Boxable[(a, b, c, d, e)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e]

instance Boxable[(a, b, c, d, e, f)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f]

instance Boxable[(a, b, c, d, e, f, g)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g]

instance Boxable[(a, b, c, d, e, f, g, h)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h]

instance Boxable[(a, b, c, d, e, f, g, h, i)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i]

instance Boxable[(a, b, c, d, e, f, g, h, i, j)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l], Boxable[m]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m, n)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l], Boxable[m], Boxable[n]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l], Boxable[m], Boxable[n], Boxable[o]
