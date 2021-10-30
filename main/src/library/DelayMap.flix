/*
 * Copyright 2021 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


use RedBlackTree.RedBlackTree;

pub enum DelayMap[k, v] {
    case DMap(RedBlackTree[k, Lazy[v]])
}

namespace DelayMap {

    ///
    /// Returns the empty map.
    ///
    pub def empty(): DelayMap[k, v] =
        DMap(RedBlackTree.empty())

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    pub def singleton(k: k, v: v): DelayMap[k, v] with Order[k] =
        insert(k, v, empty())

    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    pub def isEmpty(m: DelayMap[k, v]): Bool =
        let DMap(t) = m;
        RedBlackTree.isEmpty(t)

    ///
    /// Returns an updated map with `k -> v`.
    ///
    pub def insert(k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        DMap(RedBlackTree.insert(k, lazy v, t))

    ///
    /// Returns `Some(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    pub def get(k: k, m: DelayMap[k, v]): Option[v] with Order[k] =
        let DMap(t) = m;
        match RedBlackTree.get(k, t) {
            case None    => None
            case Some(x) => Some(force x)
        }
}
