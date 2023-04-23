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

mod TestFunctor {

    use Functor.{map, ignore}
    use Functor.{<$>, <$$>, <$, $>}


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        map(x -> x + 1, None) == None

    @test
    def map02(): Bool =
        map(x -> x + 1, Some(1)) == Some(2)

    @test
    def map03(): Bool =
        map(x -> x + 1, Nil) == Nil

    @test
    def map04(): Bool =
        map(x -> x + 1, 1 :: Nil) == 2 :: Nil

    @test
    def map05(): Bool =
        map(x -> x + 1, 1 :: 2 :: Nil) == 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        Functor.replace(None, 'A') == None

    @test
    def replace02(): Bool =
        Functor.replace(Some(1), 'A') == Some('A')

    @test
    def replace03(): Bool =
        Functor.replace(Nil, 'A') == Nil

    @test
    def replace04(): Bool =
        Functor.replace(1 :: Nil, 'A') == 'A' :: Nil

    @test
    def replace05(): Bool =
        Functor.replace(1 :: 2 :: Nil, 'A') == 'A' :: 'A' :: Nil

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
