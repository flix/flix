/*
 * Copyright 2022 Stephen Tetley
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

mod TestMonadZip {

    // Generally the functions from the `MonadZip` class will be tested without
    // overloading in their respective modules. The tests here are basically
    // to test the type class instances can be invoked as expected.

    // Helper
    def necOf2(x: a, y: a) : Nec[a] = Nec.cons(x, Nec.singleton(y))


    /////////////////////////////////////////////////////////////////////////////
    // zipWithIdentity.Identity                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIdentity01(): Bool =
        MonadZip.zipWith(Int32.max, Identity.Identity(1), Identity.Identity(10)) == Identity.Identity(10)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAIdentity.Identity                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithAIdentity01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Identity.Identity(1), Identity.Identity(10)) == Identity.Identity(Identity.Identity(10))


    /////////////////////////////////////////////////////////////////////////////
    // zipIdentity.Identity                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipIdentity01(): Bool =
        MonadZip.zip(Identity.Identity(1), Identity.Identity(10)) == Identity.Identity((1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // unzipIdentity.Identity                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzipIdentity01(): Bool =
        MonadZip.unzip(Identity.Identity((1, 10))) == (Identity.Identity(1), Identity.Identity(10))


    /////////////////////////////////////////////////////////////////////////////
    // zipWithList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithList01(): Bool =
        MonadZip.zipWith(Int32.max, 1 :: 2 :: Nil, 10 :: 11 :: Nil) == 10 :: 11 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithAList01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), 1 :: 2 :: Nil, 10 :: 11 :: Nil) == Identity.Identity(10 :: 11 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipList                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipList01(): Bool =
        MonadZip.zip(1 :: 2 :: Nil, 10 :: Nil) == (1, 10) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // unzipList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzipList01(): Bool =
        MonadZip.unzip((1, 10) :: (2, 11) :: Nil) == (1 :: 2 :: Nil, 10 :: 11 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // zipWithOption                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithOption01(): Bool =
        MonadZip.zipWith(Int32.max, Some(1), None) == None

    @test
    def zipWithOption02(): Bool =
        MonadZip.zipWith(Int32.max, Some(1), Some(10)) == Some(10)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAOption                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithAOption01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Some(1), None) == Identity.Identity(None)

    @test
    def zipWithAOption02(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Some(1), Some(10)) == Identity.Identity(Some(10))

    /////////////////////////////////////////////////////////////////////////////
    // zipOption                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipOption01(): Bool =
        MonadZip.zip(Some(1), None: Option[Int32]) == None

    @test
    def zipOption02(): Bool =
        MonadZip.zip(Some(1), Some(10)) == Some((1, 10))


    /////////////////////////////////////////////////////////////////////////////
    // unzipOption                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzipOption01(): Bool =
        MonadZip.unzip(None: Option[(Int32, Int32)]) == (None, None)

    @test
    def unzipOption02(): Bool =
        MonadZip.unzip(Some((1, 10))) == (Some(1), Some(10))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithChain                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithChain01(): Bool =
        MonadZip.zipWith(Int32.max, List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil)) == List.toChain(10 :: 11 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAChain                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithAChain01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil))
            == Identity.Identity(List.toChain(10 :: 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipChain                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipChain01(): Bool =
        MonadZip.zip(List.toChain(1 :: 2 :: Nil), Chain.singleton(10)) == Chain.singleton((1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // unzipChain                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzipChain01(): Bool =
        MonadZip.unzip(List.toChain((1, 10) :: (2, 11) :: Nil)) == (List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithNel                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithNel01(): Bool =
        MonadZip.zipWith(Int32.max, Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil)) == Nel.Nel(10, 11 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithANel                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithANel01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil))
            == Identity.Identity(Nel.Nel(10, 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipNel                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipNel01(): Bool =
        MonadZip.zip(Nel.Nel(1, 2 :: Nil), Nel.Nel(10, Nil)) == Nel.Nel((1, 10), Nil)

    /////////////////////////////////////////////////////////////////////////////
    // unzipNel                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzipNel01(): Bool =
        MonadZip.unzip(Nel.Nel((1, 10), (2, 11) :: Nil)) == (Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithNec                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithNec01(): Bool =
        MonadZip.zipWith(Int32.max, necOf2(1, 2), necOf2(10, 11)) == necOf2(10, 11)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithANec                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithANec01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), necOf2(1, 2), necOf2(10, 11))
            == Identity.Identity(necOf2(10, 11))

    /////////////////////////////////////////////////////////////////////////////
    // zipNec                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipNec01(): Bool =
        MonadZip.zip(necOf2(1, 2), Nec.singleton(10)) == Nec.singleton((1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // unzipNec                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzipNec01(): Bool =
        MonadZip.unzip(necOf2((1, 10), (2, 11))) == (necOf2(1, 2), necOf2(10, 11))

}
