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
    /// Asserts that `c` is `Absent`. Akin to a type ascription.
    ///
    /// Always returns `true`.
    ///
    pub def assertAbsent(c: Choice[_, true, false]): Bool = choose c {
        case Absent => true
    }

    ///
    /// Asserts that `c` is `Present`. Akin to a type ascription.
    ///
    /// Always returns `true`.
    ///
    pub def assertPresent(c: Choice[_, false, true]): Bool = choose c {
        case Present(_) => true
    }

    ///
    /// Returns `true` if `c` is `Absent`.
    ///
    pub def isAbsent(c: Choice[_, _, _]): Bool = choose c {
        case Absent     => true
        case Present(_) => false
    }

    ///
    /// Returns `true` if `c` is `Present`.
    ///
    pub def isPresent(c: Choice[_, _, _]): Bool = choose c {
        case Absent     => false
        case Present(_) => true
    }

    ///
    /// Returns `v` where `c` must be `Present(v)`.
    ///
    pub def unbox[s](c: Choice[s, false, true]): s =
        choose c {
            case Present(x) => x
        }

    ///
    /// Returns `v` if `o` is `Present(v).` Otherwise returns `d`.
    ///
    pub def getWithDefault(c: Choice[s, _, _], d: s): s =
        choose c {
            case Absent     => d
            case Present(v) => v
        }

    ///
    /// Returns `c1` if it is `Present(v)`. Otherwise returns `c2`.
    ///
    pub def withDefault[s :# Type, a1 :# Bool, p1 :# Bool, a2 :# Bool, p2 :# Bool, u :# Bool]
                       (c1: Choice[s, a1, p1], c2: Choice[s, a2, p2]): Choice[s, (a1 and a2) or (p1 and u), p1 or (a1 and p2)] =
        choose* c1 {
            case Absent     => c2
            case Present(v) => Present(v)
        }

    ///
    /// Returns `Present(v)` if `c` is `Present(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter[t :# Type, a :# Bool, p :# Bool, u :# Bool]
                  (f: t -> Bool, c: Choice[t, a, p]): Choice[t, a or p, (a and u) or p] =
        choose* c {
            case Absent     => Absent
            case Present(v) => if (f(v)) Present(v) else Absent
        }

    ///
    /// Returns `Present(f(v))` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    pub def map[s :# Type, t :# Type, a :# Bool, p :# Bool](f: s -> t, c: Choice[s, a, p]): Choice[t, a, p] =
        choose* c {
            case Absent     => Absent
            case Present(x) => Present(f(x))
        }

    ///
    /// Returns `f(v)` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    pub def flatMap[s :# Type, t :# Type, a1 :# Bool, p1 :# Bool, a2 :# Bool, p2 :# Bool, u :# Bool]
                   (f: s -> Choice[t, a1, p1], c: Choice[s, a2, p2]): Choice[t, (a1 and p2) or a2, (a2 and u) or (p1 and p2)] =
        choose* c {
            case Absent     => Absent
            case Present(v) => f(v)
        }

    ///
    /// Returns `v` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    pub def flatten[t :# Type, a1 :# Bool, p1 :# Bool, a2 :# Bool, p2 :# Bool, u :# Bool]
        (c: Choice[Choice[t, a1, p1], a2, p2]): Choice[t, a2 or (a1 and p2), (a2 and u) or (p1 and p2)] =
        choose* c {
            case Absent     => Absent
            case Present(v) => v
        }

    ///
    /// Returns `Absent` if `c` is `Present(_)`. Otherwise returns `Present(v)`.
    ///
    pub def invert[s :# Type, a :# Bool, p :# Bool](c: Choice[s, a, p], v: s): Choice[s, p, a] = choose* c {
        case Absent     => Present(v)
        case Present(_) => Absent
    }

}
