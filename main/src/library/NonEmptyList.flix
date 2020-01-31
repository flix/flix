/*
 * Copyright 2020 Magnus Madsen
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

///
/// The NonEmptyList type.
///

enum Nel[a] {
    case Nel(a, List[a])
}

namespace NonEmptyList {

    ///
    /// Returns the first element of the non-empty list `xs`.
    ///
    pub def head(xs: Nel[a]): a = match xs {
        case Nel(h, _) => h
    }

    // TODO: last
    // TODO: init and tail.






    pub def map(f: a -> b, xs: Nel[a]): Nel[b] = match xs {
        case Nel(h, l) => Nel(f(h), List.map(f, l))
    }

}

