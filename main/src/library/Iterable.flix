/*
 *  Copyright 2022 Nina Andrup Pedersen
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
/// A trait for immutable data structures that can be iterated.
///
pub trait Iterable[t: Type] {
    type Elm: Type
    type Aef: Eff

    ///
    /// Returns an iterator over `t`.
    ///
    pub def iterator(rc: Region[r], t: t): Iterator[Iterable.Elm[t], r + Iterable.Aef[t], r] \ r

    ///
    /// Returns an iterator over `t` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], t: t): Iterator[(Int32, Iterable.Elm[t]), r + Iterable.Aef[t], r] \ r =
        Iterable.iterator(rc, t) |> Iterator.zipWithIndex

}

instance Iterable[Array[a, r]] {
    type Elm = a
    type Aef = r

    pub def iterator(rc: Region[r1], t: Array[a, r]): Iterator[a, r + r1, r1] \ r1 = {
        Array.iterator(rc, t)
    }
}

instance Iterable[MultiMap[k, v]] {
    type Elm = (k, Set[v])
    type Aef = Pure

    pub def iterator(rc: Region[r], t: MultiMap[k, v]): Iterator[(k, Set[v]), r, r] \ r = {
        MultiMap.iterator(rc, t)
    }
}

instance Iterable[MutDeque[a, r]] {
    type Elm = a
    type Aef = r

    pub def iterator(rc: Region[r1], t: MutDeque[a, r]): Iterator[a, r + r1, r1] \ r1 = {
        MutDeque.iterator(rc, t)
    }
}

instance Iterable[MutDisjointSets[r, a]] {
    type Elm = a
    type Aef = r

    pub def iterator(rc: Region[r1], t: MutDisjointSets[r, a]): Iterator[a, r + r1, r1] \ r1 = {
        let MutDisjointSets.MutDisjointSets({forest | _}) = t;
        MutMap.iterator(rc, forest)
        |> Iterator.map(match (_, MutDisjointSets.Node.Node({value | _})) -> value)
    }
}

instance Iterable[MutList[a, r]] {
    type Elm = a
    type Aef = r

    pub def iterator(rc: Region[r1], t: MutList[a, r]): Iterator[a, r + r1, r1] \ r1 = {
        MutList.iterator(rc, t)
    }
}

instance Iterable[MutMap[k, v, r]] {
    type Elm = (k, v)
    type Aef = r

    pub def iterator(rc: Region[r1], t: MutMap[k, v, r]): Iterator[(k, v), r + r1, r1] \ r1 = {
        MutMap.iterator(rc, t)
    }
}

instance Iterable[MutQueue[a, r]] {
    type Elm = a
    type Aef = r

    pub def iterator(rc: Region[r1], t: MutQueue[a, r]): Iterator[a, r + r1, r1] \ r1 = {
        MutQueue.iterator(rc, t)
    }
}

instance Iterable[MutSet[a, r]] {
    type Elm = a
    type Aef = r

    pub def iterator(rc: Region[r1], t: MutSet[a, r]): Iterator[a, r + r1, r1] \ r1 = {
        MutSet.iterator(rc, t)
    }
}

instance Iterable[Result[e, t]] {
    type Elm = t
    type Aef = Pure

    pub def iterator(rc: Region[r], t: Result[e, t]): Iterator[t, r, r] \ r = {
        t |> Result.toOption |> Option.iterator(rc)
    }
}

instance Iterable[Validation[e, t]] {
    type Elm = t
    type Aef = Pure

    pub def iterator(rc: Region[r], t: Validation[e, t]): Iterator[t, r, r] \ r = {
        t |> Validation.toOption |> Option.iterator(rc)
    }
}
