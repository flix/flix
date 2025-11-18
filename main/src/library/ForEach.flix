/*
 *  Copyright 2025 Ry Wiese
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
/// A trait for data structures that support a forEach operation.
///
trait ForEach[t] {

    ///
    /// The type of elements in the data structure.
    ///
    type Elm: Type

    ///
    /// The effect of `forEach`.
    ///
    type Aef: Eff = {}

    ///
    /// Applies `f` to each element in the data structure.
    ///
    pub def forEach(f: ForEach.Elm[t] -> Unit \ ef, t: t): Unit \ (ef + ForEach.Aef[t])

}

mod ForEach {

     ///
     /// A view for `ForEach` instance `t` that wraps each element with its index.
     ///
     pub enum WithIndex[t](t)

     instance ForEach[WithIndex[t]] with ForEach[t] {
         type Elm = (Int32, ForEach.Elm[t])
         type Aef = ForEach.Aef[t]
         pub def forEach(f: ((Int32, ForEach.Elm[t])) -> Unit \ ef, t: WithIndex[t]): Unit \ ef + ForEach.Aef[t] = region rc {
             let WithIndex.WithIndex(base) = t;
             let indexRef = Ref.fresh(rc, 0);
             base |> ForEach.forEach(elm -> {
                 let index = indexRef |> Ref.get;
                 f((index, elm));
                 indexRef |> Ref.put(index + 1)
             })
         }
     }

     ///
     /// Returns a view for `m` that wraps each element with its index.
     ///
     pub def withIndex(m: t): WithIndex[t] with ForEach[t] = WithIndex.WithIndex(m)

    ///
    /// A view for `ForEach` instance `t` that filters out elements that don't match the predicate.
    ///
    pub enum WithFilter[t, a](a -> Bool, t)

    instance ForEach[WithFilter[t, a]] with ForEach[t] where ForEach.Elm[t] ~ a {
        type Elm = a
        type Aef = ForEach.Aef[t]
        pub def forEach(f: a -> Unit \ ef, t: WithFilter[t, a]): Unit \ ef + ForEach.Aef[t] =
            let WithFilter.WithFilter(p, base) = t;
            base |> ForEach.forEach(elm -> if (p(elm)) { f(elm) })
    }

    ///
    /// Returns a view for `m` that filters out elements that don't match `p`.
    ///
    pub def withFilter(p: a -> Bool, m: t): WithFilter[t, a] with ForEach[t] where ForEach.Elm[t] ~ a = WithFilter.WithFilter(p, m)

    ///
    /// A view for `ForEach` instance `t` that applies a mapping function to each element.
    ///
    pub enum WithMap[t, a, b](a -> b, t)

    instance ForEach[WithMap[t, a, b]] with ForEach[t] where ForEach.Elm[t] ~ a {
        type Elm = b
        type Aef = ForEach.Aef[t]
        pub def forEach(f: b -> Unit \ ef, t: WithMap[t, a, b]): Unit \ ef + ForEach.Aef[t] =
            let WithMap.WithMap(mapper, base) = t;
            base |> ForEach.forEach(elm -> f(mapper(elm)))
    }

    ///
    /// Returns a view for `m` that applies `f` to each element.
    ///
    pub def withMap(f: a -> b, m: t): WithMap[t, a, b] with ForEach[t] = WithMap.WithMap(f, m)

    ///
    /// A view for `Iterable` instances `s` and `t` that zips `s` and `t` together.
    ///
    pub enum WithZip[s, t](s, t)

    instance ForEach[WithZip[s, t]] with Iterable[s], Iterable[t] {
        type Elm = (Iterable.Elm[s], Iterable.Elm[t])
        type Aef = Iterable.Aef[s] + Iterable.Aef[t]
        pub def forEach(f: ((Iterable.Elm[s], Iterable.Elm[t])) -> Unit \ ef, t: WithZip[s, t]): Unit \ ef + Iterable.Aef[s] + Iterable.Aef[t] =
            let WithZip.WithZip(xs, ys) = t;
            region rc {
                (Iterable.iterator(rc, xs) `Iterator.zip` Iterable.iterator(rc, ys)) |> Iterator.forEach(f)
            }

    }

    ///
    /// Returns a view that zips `left` and `right` together.
    ///
    pub def withZip(left: s, right: t): WithZip[s, t] with Iterable[s], Iterable[t] = WithZip.WithZip(left, right)

}
