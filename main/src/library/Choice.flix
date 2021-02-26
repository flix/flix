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

namespace Choice {

    @test
    def assertAbsent01(): Bool = assertAbsent(Absent) == ()

    @test
    def assertPresent01(): Bool = assertPresent(Present(123)) == ()

    @test
    def isAbsent01(): Bool = isAbsent(Absent)

    @test
    def isAbsent02(): Bool = not isAbsent(Present(123))

    @test
    def isPresent01(): Bool = isPresent(Present(123))

    @test
    def unbox01(): Bool = unbox(Present(123)) == 123

    @test
    def unbox02(): Bool = unbox(Present("Hello World")) == "Hello World"

    @test
    def isPresent02(): Bool = not isPresent(Absent)

    @test
    def map01(): Bool = isAbsent(map(x -> x, Absent))

    @test
    def map02(): Bool = isPresent(map(x -> x, Present(123)))

    @test
    def map03(): Bool = unbox(map(x -> x * 2, Present(42))) == 84

    @test
    def invert01(): Bool = unbox(invert(Absent, 42)) == 42

    @test
    def invert02(): Bool = isAbsent(invert(Present(42), 84))

    @test
    def invert03(): Bool = isAbsent(invert(invert(Absent, 42), 84))

    @test
    def invert04(): Bool = isPresent(invert(invert(invert(Absent, 21), 42), 84))

}
