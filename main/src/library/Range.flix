/*
 * Copyright 2025 Magnus Madsen
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
/// Represents a range (b, e) of discrete values from `b` (inclusive) to `e` (exclusive).
///
enum Range[t] with Eq, ToString {
    case Range(t, t)
}

instance ForEach[Range[t]] with Discrete[t] {
    type Elm = t
    pub def forEach(f: t -> Unit \ ef, t: Range[t]): Unit \ ef = Range.forEach(f, t)
}

mod Range {

    ///
    /// Applies `f` to every element of `r`.
    ///
    pub def forEach(f: t -> Unit \ ef, r: Range[t]): Unit \ ef with Discrete[t] =
        let Range(b, e) = r;
        def loop(i) =
            if (i < e) {
                f(i); loop(Discrete.succ(i))
            };
        loop(b)

}
