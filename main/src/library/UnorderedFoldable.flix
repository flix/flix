/*
 *  Copyright 2022 Jakob Schneider Villumsen
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
/// A trait for unordered data structures that can be folded.
///
pub trait UnorderedFoldable[t : Type] {
    type Elm: Type
    type Aef: Eff

    ///
    /// Unordered fold of `t`.
    ///
    /// Applies `f` to all elements in `t` and combines the results
    /// i.e. `CommutativeMonoid.combine(f(a))` for all `a` in `t`.
    ///
    pub def foldMap(f: UnorderedFoldable.Elm[t] -> b \ ef, t: t): b \ UnorderedFoldable.Aef[t] + ef with CommutativeMonoid[b]

    ///
    /// Returns the result of applying `CommutativeMonoid.combine` to all the elements in `t`.
    ///
    pub def fold(t: t): a \ UnorderedFoldable.Aef[t] with CommutativeMonoid[a] where UnorderedFoldable.Elm[t] ~ a =
        UnorderedFoldable.foldMap(identity, t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t): Bool \ UnorderedFoldable.Aef[t] =
        UnorderedFoldable.forAll(_ -> false, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists(f: UnorderedFoldable.Elm[t] -> Bool \ ef, t: t): Bool \ UnorderedFoldable.Aef[t] + ef =
        use SemiGroup.Any.Any;
        let Any(x) = UnorderedFoldable.foldMap(a -> Any(f(a)), t);
        x

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forAll(f: UnorderedFoldable.Elm[t] -> Bool \ ef, t: t): Bool \ UnorderedFoldable.Aef[t] + ef =
        use SemiGroup.All.All;
        let All(x) = UnorderedFoldable.foldMap(a -> All(f(a)), t);
        x

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: a, t: t): Bool \ UnorderedFoldable.Aef[t] with Eq[a] where UnorderedFoldable.Elm[t] ~ a =
        UnorderedFoldable.exists(y -> x == y, t)

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def size(t: t): Int32 \ UnorderedFoldable.Aef[t] =
        UnorderedFoldable.foldMap(_ -> 1, t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: UnorderedFoldable.Elm[t] -> Bool \ ef, t: t): Int32 \ UnorderedFoldable.Aef[t] + ef =
        UnorderedFoldable.foldMap(a -> if (f(a)) 1 else 0, t)

}

instance UnorderedFoldable[Array[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldMap(f: a -> b \ ef, arr: Array[a, r]): b \ { ef, r } with CommutativeMonoid[b] = Array.foldMap(f, arr)
}

instance UnorderedFoldable[MutDeque[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldMap(f: a -> b \ ef, arr: MutDeque[a, r]): b \ { ef, r } with CommutativeMonoid[b] = MutDeque.foldMap(f, arr)
}

instance UnorderedFoldable[MutList[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldMap(f: a -> b \ ef, arr: MutList[a, r]): b \ { ef, r } with CommutativeMonoid[b] = MutList.foldMap(f, arr)
}


instance UnorderedFoldable[MutMap[k, v, r]] {
    type Elm = v
    type Aef = r

    pub def foldMap(f: v -> b \ ef, arr: MutMap[k, v, r]): b \ { ef, r } with CommutativeMonoid[b] = {
        arr |> MutMap.toMap |> Map.foldMap(f)
    }
}

instance UnorderedFoldable[MutSet[a, r]] {
    type Elm = a
    type Aef = r

    pub def foldMap(f: a -> b \ ef, arr: MutSet[a, r]): b \ { ef, r } with CommutativeMonoid[b] = MutSet.foldMap(f, arr)
}

instance UnorderedFoldable[MutQueue[a, r]] with Order[a] {
    type Elm = a
    type Aef = r

    pub def foldMap(f: a -> b \ ef, arr: MutQueue[a, r]): b \ { ef, r } with CommutativeMonoid[b] = {
        arr |> MutQueue.toList |> List.foldMap(f)
    }
}
