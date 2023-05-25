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
pub enum Choice[a: Type, _isAbsent : Eff, _isPresent : Eff] {
    case Absent
    case Present(a)
}

mod Choice {

    ///
    /// Asserts that `c` is `Absent`. Akin to a type ascription.
    ///
    /// Always returns `true`.
    ///
    @Experimental
    pub def assertAbsent(c: Choice[s, true, false]): Bool = relational_choose c {
        case Absent => true
    }

    ///
    /// Asserts that `c` is `Present`. Akin to a type ascription.
    ///
    /// Always returns `true`.
    ///
    @Experimental
    pub def assertPresent(c: Choice[s, false, true]): Bool = relational_choose c {
        case Present(_) => true
    }

    ///
    /// Returns `true` if `c` is `Absent`.
    ///
    @Experimental
    pub def isAbsent(c: Choice[s, pres, abs]): Bool = relational_choose c {
        case Absent     => true
        case Present(_) => false
    }

    ///
    /// Returns `true` if `c` is `Present`.
    ///
    @Experimental
    pub def isPresent(c: Choice[s, pres, abs]): Bool = relational_choose c {
        case Absent     => false
        case Present(_) => true
    }

    ///
    /// Returns `v` where `c` must be `Present(v)`.
    ///
    @Experimental
    pub def unbox(c: Choice[s, false, true]): s =
        relational_choose c {
            case Present(x) => x
        }

    ///
    /// Returns `v` if `o` is `Present(v).` Otherwise returns `d`.
    ///
    @Experimental
    pub def getWithDefault(d: s, c: Choice[s, pres, abs]): s =
        relational_choose c {
            case Absent     => d
            case Present(v) => v
        }

    ///
    /// Returns `c` if it is `Present(v)`. Otherwise returns `default`.
    ///
    @Experimental
    pub def withDefault[s : Type, a1 : Eff, p1 : Eff, a2 : Eff, p2 : Eff]
                       (default: {default = Choice[s, a2, p2]}, c: Choice[s, a1, p1]): Choice[s, a1 + a2, p1 & (a1 + p2)] =
        relational_choose* c {
            case Absent     => default.default
            case Present(v) => Present(v)
        }

    ///
    /// Returns `Present(v)` if `c` is `Present(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @Experimental
    pub def filter[t : Type, a : Eff, p : Eff]
                  (f: t -> Bool, c: Choice[t, a, p]): Choice[t, a & p, p] =
        relational_choose* c {
            case Absent     => Absent
            case Present(v) => if (f(v)) Present(v) else Absent
        }

    ///
    /// Returns `Present(f(v))` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    @Experimental
    pub def map[s: Type, t: Type, a: Eff, p: Eff](f: s -> t, c: Choice[s, a, p]): Choice[t, a, p] =
        relational_choose* c {
            case Absent     => Absent
            case Present(x) => Present(f(x))
        }

    ///
    /// Returns `f(v)` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    @Experimental
    pub def flatMap[s : Type, t : Type, a1 : Eff, p1 : Eff, a2 : Eff, p2 : Eff]
                   (f: s -> Choice[t, a2, p2], c: Choice[s, a1, p1]): Choice[t, a1 & (p1 + a2), p1 + p2] =
        relational_choose* c {
            case Absent     => Absent
            case Present(v) => f(v)
        }

    ///
    /// Returns `v` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    @Experimental
    pub def flatten[t : Type, a1 : Eff, p1 : Eff, a2 : Eff, p2 : Eff]
        (c: Choice[Choice[t, a1, p1], a2, p2]): Choice[t, (a1 + p2) & a2, p1 + p2] =
        relational_choose* c {
            case Absent     => Absent
            case Present(v) => v
        }

    ///
    /// Returns `Absent` if `c` is `Present(_)`. Otherwise returns `Present(v)`.
    ///
    @Experimental
    pub def invert[s: Type, a: Eff, p: Eff](c: Choice[s, a, p], v: s): Choice[s, p, a] = relational_choose* c {
        case Absent     => Present(v)
        case Present(_) => Absent
    }

}
