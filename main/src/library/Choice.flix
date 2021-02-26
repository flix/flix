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

/**
 * The Choice data type.
 */
pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
    case Absent
    case Present(a)
}

namespace Choice {

    ///
    /// Returns `Present(f(v))` if `o` is `Present(v)`. Otherwise returns `Absent`.
    ///
    pub def map[s :# Type, t :# Type, a :# Bool, p :# Bool](f: s -> t, c: Choice[s, a, p]): Choice[t, a, p] =
        choose* c {
            case Absent     => Absent
            case Present(x) => Present(f(x))
        }

    ///
    /// Returns `v` where `c` must be `Present(v)`.
    ///
    pub def unbox[s](c: Choice[s, false, true]): s =
        choose c {
            case Present(x) => x
        }

}
