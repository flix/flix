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

namespace TestApplicative {

    use Applicative.{seqRight, seqLeft, whenTrue, whenFalse};

    // Local definition of a State applicative so we can have observable side-effects
    // to test `whenTrue` and `whenFalse`.

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

    def runState(m: State[s, a], s: s): (s, a) & Impure =
        let State(m1) = m;
        m1(s)

    def update(f: s -> s & ef): State[s, Unit] & ef =
        State(s ->
            (f(s) as & Impure, ())
        ) as & ef

    /////////////////////////////////////////////////////////////////////////////
    // seqLeft                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seqLeft01(): Bool =
        seqLeft(None, Some(2)) == None

    @test
    def seqLeft02(): Bool =
        seqLeft(Some(1), None) == None

    @test
    def seqLeft03(): Bool =
        seqLeft(Some(1), Some(2)) == Some(1)

    @test
    def seqLeft04(): Bool =
        seqLeft(Nil, 2 :: Nil) == Nil

    @test
    def seqLeft05(): Bool =
        seqLeft(1 :: Nil, Nil) == Nil

    @test
    def seqLeft06(): Bool =
        seqLeft(1 :: Nil, 2 :: Nil) == 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // seqRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seqRight01(): Bool =
        seqRight(None, Some(2)) == None

    @test
    def seqRight02(): Bool =
        seqRight(Some(1), None) == None

    @test
    def seqRight03(): Bool =
        seqRight(Some(1), Some(2)) == Some(2)

    @test
    def seqRight04(): Bool =
        seqRight(Nil, 2 :: Nil) == Nil

    @test
    def seqRight05(): Bool =
        seqRight(1 :: Nil, Nil) == Nil

    @test
    def seqRight06(): Bool =
        seqRight(1 :: Nil, 2 :: Nil) == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // whenTrue                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def whenTrue01(): Bool =
        let ma = whenTrue(true, update(s -> s+100));
        runState(ma, 0) as & Pure == (100, ())

    @test
    def whenTrue02(): Bool =
        let ma = whenTrue(false, update(s -> s+100));
        runState(ma, 0) as & Pure == (0, ())

    /////////////////////////////////////////////////////////////////////////////
    // whenFalse                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def whenFalse01(): Bool =
        let ma = whenFalse(false, update(s -> s+100));
        runState(ma, 0) as & Pure == (100, ())

    @test
    def whenFalse02(): Bool =
        let ma = whenFalse(true, update(s -> s+100));
        runState(ma, 0) as & Pure == (0, ())

}
