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

instance Iterable[List] {
    pub def iterator(t: List[a]): Iterator[a] & Impure =
        let cursor = ref t;
        let done = () -> match (deref cursor) {
            case Nil => true
            case _   => false
        };
        let next = () -> match (deref cursor) {
            case Nil     => bug!("Empty Iterator!")
            case x :: xs => {
                cursor := xs;
                x
            }
        };
        Iterator(done, next)
}

instance Iterable[LazyList] {
    pub def iterator(t: LazyList[a]): Iterator[a] & Impure =
        let cursor = ref t;
        let done = () -> match LazyList.head(deref cursor) {
            case None    => true
            case Some(_) => false
        };
        let next = () -> match LazyList.head(deref cursor) {
            case None    => bug!("Empty iterator")
            case Some(x) =>
                cursor := LazyList.tail(deref cursor);
                x
        };
        Iterator(done, next)
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
