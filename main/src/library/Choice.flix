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
@Internal
pub enum Choice[a: Type, _isAbsent : Bool, _isPresent : Bool] {
    case Absent
    case Present(a)
}

mod Choice {

    ///
    /// Asserts that `c` is `Absent`. Akin to a type ascription.
    ///
    /// Always returns `true`.
    ///
    @experimental
    pub def assertAbsent(c: Choice[s, true, false]): Bool = relational_choose c {
        case Absent => true
    }

    ///
    /// Asserts that `c` is `Present`. Akin to a type ascription.
    ///
    /// Always returns `true`.
    ///
    @experimental
    pub def assertPresent(c: Choice[s, false, true]): Bool = relational_choose c {
        case Present(_) => true
    }

    ///
    /// Returns `true` if `c` is `Absent`.
    ///
    @experimental
    pub def isAbsent(c: Choice[s, pres, abs]): Bool = relational_choose c {
        case Absent     => true
        case Present(_) => false
    }

    ///
    /// Returns `true` if `c` is `Present`.
    ///
    @experimental
    pub def isPresent(c: Choice[s, pres, abs]): Bool = relational_choose c {
        case Absent     => false
        case Present(_) => true
    }

    ///
    /// Returns `v` where `c` must be `Present(v)`.
    ///
    @experimental
    pub def unbox(c: Choice[s, false, true]): s =
        relational_choose c {
            case Present(x) => x
        }

    ///
    /// Returns `v` if `o` is `Present(v).` Otherwise returns `d`.
    ///
    @experimental
    pub def getWithDefault(d: s, c: Choice[s, pres, abs]): s =
        relational_choose c {
            case Absent     => d
            case Present(v) => v
        }

    ///
    /// Returns `c` if it is `Present(v)`. Otherwise returns `default`.
    ///
    @experimental
    pub def withDefault[s : Type, a1 : Bool, p1 : Bool, a2 : Bool, p2 : Bool]
                       (default: {default = Choice[s, a2, p2]}, c: Choice[s, a1, p1]): Choice[s, a1 and a2, p1 or (a1 and p2)] =
        relational_choose* c {
            case Absent     => default.default
            case Present(v) => Present(v)
        }

    ///
    /// Returns `Present(v)` if `c` is `Present(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @experimental
    pub def filter[t : Type, a : Bool, p : Bool]
                  (f: t -> Bool, c: Choice[t, a, p]): Choice[t, a or p, p] =
        relational_choose* c {
            case Absent     => Absent
            case Present(v) => if (f(v)) Present(v) else Absent
        }

    ///
    /// Returns `Present(f(v))` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    @experimental
    pub def map[s: Type, t: Type, a: Bool, p: Bool](f: s -> t, c: Choice[s, a, p]): Choice[t, a, p] =
        relational_choose* c {
            case Absent     => Absent
            case Present(x) => Present(f(x))
        }

    ///
    /// Returns `f(v)` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    @experimental
    pub def flatMap[s : Type, t : Type, a1 : Bool, p1 : Bool, a2 : Bool, p2 : Bool]
                   (f: s -> Choice[t, a2, p2], c: Choice[s, a1, p1]): Choice[t, a1 or (p1 and a2), p1 and p2] =
        relational_choose* c {
            case Absent     => Absent
            case Present(v) => f(v)
        }

    ///
    /// Returns `v` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    @experimental
    pub def flatten[t : Type, a1 : Bool, p1 : Bool, a2 : Bool, p2 : Bool]
        (c: Choice[Choice[t, a1, p1], a2, p2]): Choice[t, (a1 and p2) or a2, p1 and p2] =
        relational_choose* c {
            case Absent     => Absent
            case Present(v) => v
        }

    ///
    /// Returns `Absent` if `c` is `Present(_)`. Otherwise returns `Present(v)`.
    ///
    @experimental
    pub def invert[s: Type, a: Bool, p: Bool](c: Choice[s, a, p], v: s): Choice[s, p, a] = relational_choose* c {
        case Absent     => Present(v)
        case Present(_) => Absent
    }

}
