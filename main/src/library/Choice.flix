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

// TODO
//    ///
//    /// Returns `o1` if it is `Some(v)`. Otherwise returns `o2`.
//    ///
//    @Time(1) @Space(1)
//    pub def withDefault(o1: Option[a], o2: Option[a]): Option[a] = match o1 {
//        case None => o2
//        case Some(_) => o1
//    }
// TODO
//    ///
//    /// Returns `Some(y)` if `o` is `Some(x)`. Otherwise returns `o`.
//    ///
//    @Time(1) @Space(1)
//    pub def replace[a : Eq](x: a, y: a, o: Option[a]): Option[a] = match o {
//        case None => o
//        case Some(v) => Some(if (v == x) y else v)
//    }
//
//    pub def filter(f: a -> Bool, o: Option[a]): Option[a] = match o {
//        case None => None
//        case Some(v) => if (f(v)) o else None
//    }


// TODO
//     pub def flatMap(f: a -> Option[b] & e, o: Option[a]): Option[b] & e = match o {
//
//    }
// TODO
//    ///
//    /// Returns `v` if `o` is `Some(v)`. Otherwise returns `None`.
//    ///
//    @Time(1) @Space(1)
//    pub def flatten(o: Option[Option[a]]): Option[a] = match o {
//        case None => None
//        case Some(v) => v
//    }


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

// TODO
//    ///
//    /// Returns `Some((v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
//    ///
//    @Time(1) @Space(1)
//    pub def zip(o1: Option[a], o2: Option[b]): Option[(a, b)] = match (o1, o2) {
//        case (None, _) => None
//        case (_, None) => None
//        case (Some(v1), Some(v2)) => Some((v1, v2))
//    }
// TODO
//    ///
//    /// Returns `(Some(v1), Some(v2))` if `o` is `Some((v1, v2))`. Otherwise returns `(None, None)`.
//    ///
//    @Time(1) @Space(1)
//    pub def unzip(o: Option[(a, b)]): (Option[a], Option[b]) = match o {
//        case None => (None, None)
//        case Some((v1, v2)) => (Some(v1), Some(v2))
//    }

// TODO
//    pub def foreach(f: a ~> Unit, o: Option[a]): Unit & Impure = match o {
//        case None    => ()
//        case Some(v) => f(v)
//    }

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

}
