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
mod Boxable {

    use Vector.get

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



    @Internal
    pub def lift1X1(f: i1 -> (o1)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[o1] =
        (v: Vector[Boxed]) ->
            let (o1) = f(unbox(get(0, v)));
            Vector#{box(o1)}

    @Internal
    pub def lift1X2(f: i1 -> (o1, o2)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[o1], Order[o2] =
        (v: Vector[Boxed]) ->
            let (o1, o2) = f(unbox(get(0, v)));
            Vector#{box(o1), box(o2)}

    @Internal
    pub def lift1X3(f: i1 -> (o1, o2, o3)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[o1], Order[o2], Order[o3] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3) = f(unbox(get(0, v)));
            Vector#{box(o1), box(o2), box(o3)}

    @Internal
    pub def lift1X4(f: i1 -> (o1, o2, o3, o4)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[o1], Order[o2], Order[o3], Order[o4] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4) = f(unbox(get(0, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4)}

    @Internal
    pub def lift1X5(f: i1 -> (o1, o2, o3, o4, o5)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[o1], Order[o2], Order[o3], Order[o4], Order[o5] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4, o5) = f(unbox(get(0, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4), box(o5)}

    @Internal
    pub def lift2X1(f: i1 -> i2 -> (o1)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[o1] =
        (v: Vector[Boxed]) ->
            let (o1) = f(unbox(get(0, v)), unbox(get(1, v)));
            Vector#{box(o1)}

    @Internal
    pub def lift2X2(f: i1 -> i2 -> (o1, o2)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[o1], Order[o2] =
        (v: Vector[Boxed]) ->
            let (o1, o2) = f(unbox(get(0, v)), unbox(get(1, v)));
            Vector#{box(o1), box(o2)}

    @Internal
    pub def lift2X3(f: i1 -> i2 -> (o1, o2, o3)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[o1], Order[o2], Order[o3] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3) = f(unbox(get(0, v)), unbox(get(1, v)));
            Vector#{box(o1), box(o2), box(o3)}

    @Internal
    pub def lift2X4(f: i1 -> i2 -> (o1, o2, o3, o4)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[o1], Order[o2], Order[o3], Order[o4] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4) = f(unbox(get(0, v)), unbox(get(1, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4)}

    @Internal
    pub def lift2X5(f: i1 -> i2 -> (o1, o2, o3, o4, o5)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[o1], Order[o2], Order[o3], Order[o4], Order[o5] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4, o5) = f(unbox(get(0, v)), unbox(get(1, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4), box(o5)}

    @Internal
    pub def lift3X1(f: i1 -> i2 -> i3 -> (o1)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[o1] =
        (v: Vector[Boxed]) ->
            let (o1) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)));
            Vector#{box(o1)}

    @Internal
    pub def lift3X2(f: i1 -> i2 -> i3 -> (o1, o2)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[o1], Order[o2] =
        (v: Vector[Boxed]) ->
            let (o1, o2) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)));
            Vector#{box(o1), box(o2)}

    @Internal
    pub def lift3X3(f: i1 -> i2 -> i3 -> (o1, o2, o3)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[o1], Order[o2], Order[o3] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)));
            Vector#{box(o1), box(o2), box(o3)}

    @Internal
    pub def lift3X4(f: i1 -> i2 -> i3 -> (o1, o2, o3, o4)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[o1], Order[o2], Order[o3], Order[o4] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4)}

    @Internal
    pub def lift3X5(f: i1 -> i2 -> i3 -> (o1, o2, o3, o4, o5)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[o1], Order[o2], Order[o3], Order[o4], Order[o5] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4, o5) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4), box(o5)}

    @Internal
    pub def lift4X1(f: i1 -> i2 -> i3 -> i4 -> (o1)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[o1] =
        (v: Vector[Boxed]) ->
            let (o1) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)));
            Vector#{box(o1)}

    @Internal
    pub def lift4X2(f: i1 -> i2 -> i3 -> i4 -> (o1, o2)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[o1], Order[o2] =
        (v: Vector[Boxed]) ->
            let (o1, o2) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)));
            Vector#{box(o1), box(o2)}

    @Internal
    pub def lift4X3(f: i1 -> i2 -> i3 -> i4 -> (o1, o2, o3)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[o1], Order[o2], Order[o3] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)));
            Vector#{box(o1), box(o2), box(o3)}

    @Internal
    pub def lift4X4(f: i1 -> i2 -> i3 -> i4 -> (o1, o2, o3, o4)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[o1], Order[o2], Order[o3], Order[o4] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4)}

    @Internal
    pub def lift4X5(f: i1 -> i2 -> i3 -> i4 -> (o1, o2, o3, o4, o5)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[o1], Order[o2], Order[o3], Order[o4], Order[o5] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4, o5) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4), box(o5)}

    @Internal
    pub def lift5X1(f: i1 -> i2 -> i3 -> i4 -> i5 -> (o1)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[i5], Order[o1] =
        (v: Vector[Boxed]) ->
            let (o1) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)), unbox(get(4, v)));
            Vector#{box(o1)}

    @Internal
    pub def lift5X2(f: i1 -> i2 -> i3 -> i4 -> i5 -> (o1, o2)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[i5], Order[o1], Order[o2] =
        (v: Vector[Boxed]) ->
            let (o1, o2) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)), unbox(get(4, v)));
            Vector#{box(o1), box(o2)}

    @Internal
    pub def lift5X3(f: i1 -> i2 -> i3 -> i4 -> i5 -> (o1, o2, o3)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[i5], Order[o1], Order[o2], Order[o3] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)), unbox(get(4, v)));
            Vector#{box(o1), box(o2), box(o3)}

    @Internal
    pub def lift5X4(f: i1 -> i2 -> i3 -> i4 -> i5 -> (o1, o2, o3, o4)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[i5], Order[o1], Order[o2], Order[o3], Order[o4] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)), unbox(get(4, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4)}

    @Internal
    pub def lift5X5(f: i1 -> i2 -> i3 -> i4 -> i5 -> (o1, o2, o3, o4, o5)): Vector[Boxed] -> Vector[Boxed]
        with Order[i1], Order[i2], Order[i3], Order[i4], Order[i5], Order[o1], Order[o2], Order[o3], Order[o4], Order[o5] =
        (v: Vector[Boxed]) ->
            let (o1, o2, o3, o4, o5) = f(unbox(get(0, v)), unbox(get(1, v)), unbox(get(2, v)), unbox(get(3, v)), unbox(get(4, v)));
            Vector#{box(o1), box(o2), box(o3), box(o4), box(o5)}

}
