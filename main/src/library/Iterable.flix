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
/// A type class for data structures that can be iterated.
///
pub lawless class Iterable[t: Type -> Type] {

    ///
    /// Returns an iterator of `t`.
    ///
    pub def iterator(t: t[a]): Iterator[a] & Impure

}

instance Iterable[Array] {
    pub def iterator(t: Array[a]): Iterator[a] & Impure =
        Array.toIterator(t)
}

instance Iterable[List] {
    pub def iterator(t: List[a]): Iterator[a] & Impure =
        List.toIterator(t)
}

instance Iterable[LazyList] {
    pub def iterator(t: LazyList[a]): Iterator[a] & Impure =
        LazyList.toIterator(t)
}

instance Iterable[MutList] {
    pub def iterator(t: MutList[a]): Iterator[a] & Impure =
        let i = ref 0;
        let list = ref t;
        let done = () -> MutList.isEmpty(deref list);
        let next = () -> {
                let elem = MutList.get(deref i, deref list);
                i := deref i + 1;
                elem
            };
        Iterator(done, next)
}

instance Iterable[Nel] {
    pub def iterator(t: Nel[a]): Iterator[a] & Impure =
        Nel.toIterator(t)
}

instance Iterable[Option] {
    pub def iterator(t: Option[a]): Iterator[a] & Impure =
        Option.toIterator(t)
}

instance Iterable[Set] {
    pub def iterator(t: Set[a]): Iterator[a] & Impure =
        Set.toIterator(t)
}
