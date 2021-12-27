/*
 *  Copyright 2021 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace TestMonad {

    use Applicative.{point};
    use Monad.{join, bind, kleisliLeft, kleisliRight, whenTrueM, whenFalseM};

    // Local definition of a State monad so we can have observable side-effects
    // to test `whenTrueM` and `whenFalseM`.

    opaque type State[s, a] = s -> (s, a) & Impure

    instance Functor[State[s]] {
        pub def map(f: a -> b & ef, m: State[s, a]): State[s, b] & ef =
            State(s ->
                let State(m1) = m;
                let (s1, a) = m1(s);
                (s1, f(a)) as & Impure
            ) as & ef
    }

    instance Applicative[State[s]] {
        pub def point(x: a): State[s, a] = State(s -> (s, x) as & Impure)
        pub def ap(mf: State[s, a -> b & ef], ma: State[s, a]): State[s, b] & ef =
            State(s ->
                let State(mf1) = mf;
                let (s1, f) = mf1(s) as & Impure;
                let State(ma1) = ma;
                let (s2, b) = ma1(s1);
                (s2, f(b)) as & Impure
            ) as & ef
    }

    instance Monad[State[s]] {
        pub def flatMap(f: a -> State[s, b] & ef, m: State[s, a]): State[s, b] & ef =
            State(s ->
                let State(m1) = m;
                let (s1, a) = m1(s);
                let State(m2) = f(a) as & Impure;
                m2(s1)
            ) as & ef
    }

    def runState(m: State[s, a], s: s): (s, a) & Impure =
        let State(m1) = m;
        m1(s)

    def update(f: s -> s & ef): State[s, Unit] & ef =
        State(s ->
            (f(s) as & Impure, ())
        ) as & ef


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        let x: Option[Option[Int32]] = None;
        join(x) == None

    @test
    def join02(): Bool =
        let x: Option[Option[Int32]] = Some(None);
        join(x) == None

    @test
    def join03(): Bool =
        let x: Option[Option[Int32]] = Some(Some(1));
        join(x) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // bind                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bind01(): Bool =
        bind(None, x -> Some((x, x))) == None

    @test
    def bind02(): Bool =
        bind(Some(1), x -> Some((x, x))) == Some((1, 1))

    /////////////////////////////////////////////////////////////////////////////
    // kleisliLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def kleisliLeft01(): Bool =
        kleisliLeft(_ -> None, x2 -> Some(ToString.toString(x2)), 1) == None

    @test
    def kleisliLeft02(): Bool =
        kleisliLeft(x1 -> Some(x1 + 1), x2 -> Some(ToString.toString(x2)), 1) == Some("2")

    /////////////////////////////////////////////////////////////////////////////
    // kleisliRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def kleisliRight01(): Bool =
        kleisliRight(x1 -> Some(ToString.toString(x1)), _ -> None, 1) == None

    @test
    def kleisliRight02(): Bool =
        kleisliRight(x1 -> Some(ToString.toString(x1)), x2 -> Some(x2 + 1), 1) == Some("2")

    /////////////////////////////////////////////////////////////////////////////
    // whenTrueM                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def whenTrueM01(): Bool =
        let ma = whenTrueM(point(true), update(s -> s+100));
        runState(ma, 0) as & Pure == (100, ())

    @test
    def whenTrueM02(): Bool =
        let ma = whenTrueM(point(false), update(s -> s+100));
        runState(ma, 0) as & Pure == (0, ())

    /////////////////////////////////////////////////////////////////////////////
    // whenFalseM                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def whenFalseM01(): Bool =
        let ma = whenFalseM(point(false), update(s -> s+100));
        runState(ma, 0) as & Pure == (100, ())

    @test
    def whenFalseM02(): Bool =
        let ma = whenFalseM(point(true), update(s -> s+100));
        runState(ma, 0) as & Pure == (0, ())

}
