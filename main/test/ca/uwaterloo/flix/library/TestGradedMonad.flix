/*
 *  Copyright 2023 Stephen Tetley
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

mod TestGradedMonad {

    use GradedApplicative.{point};
    use GradedMonad.{flatMap, flatten, kleisliLeft, kleisliRight};

    use TestGradedMonad.Int32Reader
    use TestGradedMonad.Int32Reader.{Int32Reader}

    pub enum Int32Reader[ef: Bool, a: Type](Int32 -> a \ ef)

    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def runReader(ma: Int32Reader[ef, a], i: Int32): a \ ef =
        let Int32Reader(ma1) = ma;
        ma1(i)

    ///
    /// Return the current environemnt.
    ///
    def ask(): Int32Reader[ef, Int32] =
        Int32Reader(i -> checked_ecast(i))

    ///
    /// Use an array computation which changes the effect type (i.e. the "grade") of the Int32Reader.
    ///
    def genArray(rc: Region[r], n: Int32): Int32Reader[ef and r, Array[Int32, r]] =
        Int32Reader(i ->
            checked_ecast(Array.repeat(rc, n, i))
        )

    instance GradedFunctor[Int32Reader] {
        pub def map(f: a -> b \ ef1, ma: Int32Reader[ef, a]): Int32Reader[ef and ef1, b] =
            Int32Reader(i ->
                let a = runReader(unchecked_cast(ma as Int32Reader[ef and ef1, a]), i);
                f(a)
            )
    }

    instance GradedApplicative[Int32Reader] {
        pub def point(x: a): Int32Reader[ef, a] =
            Int32Reader(_ -> checked_ecast(x))

        pub def ap(mf: Int32Reader[ef1, a -> b \ ef2], ma: Int32Reader[ef, a]): Int32Reader[ef and ef1 and ef2, b] =
            Int32Reader(i ->
                let f = runReader(unchecked_cast(mf as Int32Reader[ef and ef1 and ef2, a -> b \ ef2]), i);
                let a = runReader(unchecked_cast(ma as Int32Reader[ef and ef1 and ef2, a]), i);
                f(a)
            )
    }

    instance GradedMonad[Int32Reader] {
        pub def flatMap(f: a -> Int32Reader[ef1, b] \ ef2, ma: Int32Reader[ef, a]): Int32Reader[ef and ef1 and ef2, b] =
            Int32Reader(i ->
                let a = runReader(unchecked_cast(ma as Int32Reader[ef and ef1 and ef2, a]), i);
                runReader(unchecked_cast(f(a) as Int32Reader[ef and ef1 and ef2, b]), i)
            )
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool =
        runReader(flatMap(x -> point(x+1), point(1)), 1) == 2

    @test
    def flatMap02(): Bool =
        runReader(flatMap(x -> point(x+1), ask()), 1) == 2

    @test
    def flatMap03(): Bool =
        runReader(flatMap(x -> if (x > 0) point(Some(x)) else point(None), ask()), 1) == Some(1)

    @test
    def flatMap04(): Bool = region rc {
        runReader(flatMap(x -> point(Array.length(x)), genArray(rc, 3)), 1) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        runReader(flatten(point(point(1))), 1) == 1

    @test
    def flatten02(): Bool =
        runReader(flatten(point(ask())), 1) == 1

    @test
    def flatten03(): Bool = region rc {
        let arr = runReader(flatten(point(genArray(rc, 3))), 1);
        Array.toList(arr) == List#{1, 1, 1}
    }
}
