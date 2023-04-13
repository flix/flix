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

    use GradedFunctor.{<$>, <$$>, <$, $>}

    use TestGradedMonad.Int32Reader
    use TestGradedMonad.Int32Reader.{Int32Reader}

    pub enum Int32Reader[ef: Bool, a: Type](Int32 -> a \ ef)

    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def runReader(ma: Int32Reader[ef, a], i: Int32): a \ ef =
        let Int32Reader(ma1) = ma;
        ma1(i)

    def ask(): Int32Reader[ef, Int32] =
        Int32Reader(i -> checked_ecast(i))

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
        use GradedApplicative.{point};
        use GradedMonad.{flatMap};
        runReader(flatMap(x -> point(x+1), ask()), 0) == 1

}
