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

mod TestGradedFunctor {

    use GradedFunctor.{<$>, <$$>, <$, $>}

    use TestGradedFunctor.Int32Reader
    use TestGradedFunctor.Int32Reader.{Int32Reader}

    ///
    /// A Reader functor with the environment type restricted to `Int32`.
    ///
    pub enum Int32Reader[ef: Bool, a: Type](Int32 -> a \ ef)

    ///
    /// Run an Int32Reader computation.
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

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        runReader(GradedFunctor.map(x -> x + 1, ask()), 0) == 1

    @test
    def map02(): Bool =
        runReader(GradedFunctor.map(x -> if (x > 0) Some(x) else None, ask()), 0) == None

    @test
    def map03(): Bool =
        runReader(GradedFunctor.map(x -> if (x > 0) Some(x) else None, ask()), 1) == Some(1)


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        runReader(GradedFunctor.replace(ask(), 2), 1) == 2

    @test
    def replace02(): Bool =
        let opti: Option[Int32] = None;
        runReader(GradedFunctor.replace(ask(), opti), 1) == None

    @test
    def replace03(): Bool =
        runReader(GradedFunctor.replace(ask(), Some(2)), 1) == Some(2)


    /////////////////////////////////////////////////////////////////////////////
    // ignore                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ignore01(): Bool =
        runReader(GradedFunctor.ignore(ask()), 1) == ()

   @test
    def ignore02(): Bool =
        runReader(GradedFunctor.ignore(ask()), 100) == ()

    /////////////////////////////////////////////////////////////////////////////
    // symMap (<$>)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symMap01(): Bool =
        runReader((x -> x + 1) <$> ask(), 1) == 2

    @test
    def symMap02(): Bool =
        runReader((x -> if (x > 0) Some(x) else None) <$> ask(), 1) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // symFlippedMap (<$$>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symFilppedMap01(): Bool =
        runReader(ask() <$$> (x -> x + 1), 1) == 2

    @test
    def symFlippedMap02(): Bool =
        runReader(ask() <$$> (x -> if (x > 0) Some(x) else None), 1) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // symReplace (<$)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symReplace01(): Bool =
        runReader(() <$ ask(), 1) == ()

    @test
    def symReplace02(): Bool =
        runReader(Some(100) <$ ask(), 1) == Some(100)

    /////////////////////////////////////////////////////////////////////////////
    // symFlippedReplace ($>)                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symFlippedReplace01(): Bool =
        runReader(ask() $> (), 1) == ()

    @test
    def symFlippedReplace02(): Bool =
        runReader(ask() $> Some(100), 1) == Some(100)

}
