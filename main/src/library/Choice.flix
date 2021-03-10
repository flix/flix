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
    pub def assertAbsent(c: Choice[_, true, false]): Unit = choose c {
        case Absent => ()
    }

    ///
    /// Asserts that `c` is `Present`. Akin to a type ascription.
    ///
    pub def assertPresent(c: Choice[_, false, true]): Unit = choose c {
        case Present(_) => ()
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
    /// Returns `Present(v)` if `c` is `Present(v)` and the predicate `f(v)` is true.
    ///
    /// Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter[t :# Type, a :# Bool, p :# Bool, u :# Bool]
                  (f: t -> Bool, c: Choice[t, a, p]): Choice[t, a or p, (a and u) or p] =
        choose* c {
            case Absent     => Absent
            case Present(v) => if (f(v)) Present(v) else Absent
        }

// TODO
//     pub def flatMap(f: a -> Option[b] & e, o: Option[a]): Option[b] & e = match o {
//
//    }

    ///
    /// Returns `v` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    pub def flatten[t :# Type, a1 :# Bool, p1 :# Bool, a2 :# Bool, p2 :# Bool, u :# Bool]
        (c: Choice[Choice[t, a1, p1], a2, p2]): Choice[t, a2 or (a1 and p2), (a2 and u) or (p1 and p2)] =
        choose* c {
            case Absent     => Absent
            case Present(v) => v
        }


// TODO
//    ///
//    /// Returns `Some(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Some(v_i)`. Otherwise returns `None`.
//    ///
//    @Time(List.length(xs)) @Space(List.length(xs))
//    pub def sequence(xs: List[Option[a]]): Option[List[a]] =
//        sequenceHelper(xs, _ -> None, ys -> Some(ys))

// TODO
//    ///
//    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(xs_i)` is `Some(v_i)`. Otherwise returns `None`.
//    ///
//    pub def traverse(f: a -> Option[b] & e, xs: List[a]): Option[List[b]] & e =
//        traverseHelper(f, xs, _ -> None, ys -> Some(ys))

    ///
    /// Returns `Present(f(v))` if `c` is `Present(v)`. Otherwise returns `Absent`.
    ///
    pub def map[s :# Type, t :# Type, a :# Bool, p :# Bool](f: s -> t, c: Choice[s, a, p]): Choice[t, a, p] =
        choose* c {
            case Absent     => Absent
            case Present(x) => Present(f(x))
        }

    ///
    /// Returns `Present(f(v1, v2))` if `c1` is `Present(v1)` and `c2` is `Present(v2)`. Otherwise returns `Absent`.
    ///
//    pub def map2[s1 :# Type, s2 :# Type, t :# Type, a1 :# Bool, a2 :# Bool, p1 :# Bool, p2 :# Bool]
//                (f: (s1, s2) -> t, c1: Choice[s1, a1, p1], c2: Choice[s2, a2, p2]): Choice[t, a1 or a2, p1 and p2] =
//        choose* (c1, c2) {
//            case (Absent, Absent)         => Absent
//            case (Absent, Present(_))     => Absent
//            case (Present(_), Absent)     => Absent
//            case (Present(x), Present(y)) => Present(f(x, y))
//        }

    ///
    /// Returns `Absent` if `c` is `Present(_)`. Otherwise returns `Present(v)`.
    ///
    pub def invert[s :# Type, a :# Bool, p :# Bool](c: Choice[s, a, p], v: s): Choice[s, p, a] = choose* c {
        case Absent     => Present(v)
        case Present(_) => Absent
    }

    ///
    /// Returns `Present((v1, v2))` if `c1` is `Present(v1)` and `c2` is `Present(v2)`.
    ///
    /// Otherwise returns `None`.
    ///
    // TODO: Add type signature
    pub def zip(): Unit =
        let _f = (x, y) -> choose* (x, y) {
            case (Absent, Absent)           => Absent
            case (Absent, Present(_))       => Absent
            case (Present(_), Absent)       => Absent
            case (Present(v1), Present(v2)) => Present((v1, v2))
        };
        ()

    ///
    /// Returns `(Present(v1), Present(v2))` if `c` is `Present((v1, v2))`.
    ///
    /// Otherwise returns `(None, None)`.
    ///
    /// Note that `unzip` loses all information about absence and presence.
    ///
    pub def unzip[s :# Type, t :# Type, a :# Bool, p :# Bool]
                 (c: Choice[(s, t), a, p]): (Choice[s, true, true], Choice[t, true, true]) =
        choose c {
            case Absent     => (Absent, Absent)
            case Present(v) => (Present(fst(v)), Present(snd(v)))
        }

}
