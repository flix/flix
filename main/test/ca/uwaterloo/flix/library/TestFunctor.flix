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

namespace TestFunctor {

    use Functor.{lift, ignore};
    use Functor.{<$>, <$$>, <$, $>};


    /////////////////////////////////////////////////////////////////////////////
    // lift                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift01(): Bool =
        lift(x -> x + 1, None) == None

    @test
    def lift02(): Bool =
        lift(x -> x + 1, Some(1)) == Some(2)

    @test
    def lift03(): Bool =
        lift(x -> x + 1, Nil) == Nil

    @test
    def lift04(): Bool =
        lift(x -> x + 1, 1 :: Nil) == 2 :: Nil

    @test
    def lift05(): Bool =
        lift(x -> x + 1, 1 :: 2 :: Nil) == 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // as                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def as01(): Bool =
        Functor.as(None, 'A') == None

    @test
    def as02(): Bool =
        Functor.as(Some(1), 'A') == Some('A')

    @test
    def as03(): Bool =
        Functor.as(Nil, 'A') == Nil

    @test
    def as04(): Bool =
        Functor.as(1 :: Nil, 'A') == 'A' :: Nil

    @test
    def as05(): Bool =
        Functor.as(1 :: 2 :: Nil, 'A') == 'A' :: 'A' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // ignore                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ignore01(): Bool =
        ignore(None) == None

    @test
    def ignore02(): Bool =
        ignore(Some(1)) == Some(())

    @test
    def ignore03(): Bool =
        ignore(Nil) == Nil

    @test
    def ignore04(): Bool =
        ignore(1 :: Nil) == () :: Nil

    @test
    def ignore05(): Bool =
        ignore(1 :: 2 :: Nil) == () :: () :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // symMap (<$>)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symMap01(): Bool =
        (x -> x + 1) <$> None == None

    @test
    def symMap02(): Bool =
        (x -> x + 1) <$> Some(1) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // symFlippedMap (<$$>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symFilppedMap01(): Bool =
        None <$$> (x -> x + 1) == None

    @test
    def symFlippedMap02(): Bool =
        Some(1) <$$> (x -> x + 1) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // symAs (<$)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symAs01(): Bool =
        () <$ None == None

    @test
    def symAs02(): Bool =
        () <$ Some(1) == Some(())

    /////////////////////////////////////////////////////////////////////////////
    // symFlippedAs ($>)                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symFlippedAs01(): Bool =
        None $> () == None

    @test
    def symFlippedAs02(): Bool =
        Some(1) $> () == Some(())

}
