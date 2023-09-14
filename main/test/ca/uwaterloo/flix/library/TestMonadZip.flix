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

    @Test
    def zipWithIdentity01(): Bool =
        MonadZip.zipWith(Int32.max, Identity.Identity(1), Identity.Identity(10)) == Identity.Identity(10)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAIdentity.Identity                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAIdentity01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Identity.Identity(1), Identity.Identity(10)) == Identity.Identity(Identity.Identity(10))


    /////////////////////////////////////////////////////////////////////////////
    // zipIdentity.Identity                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipIdentity01(): Bool =
        MonadZip.zip(Identity.Identity(1), Identity.Identity(10)) == Identity.Identity((1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // unzipIdentity.Identity                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipIdentity01(): Bool =
        MonadZip.unzip(Identity.Identity((1, 10))) == (Identity.Identity(1), Identity.Identity(10))


    /////////////////////////////////////////////////////////////////////////////
    // zipWithList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithList01(): Bool =
        MonadZip.zipWith(Int32.max, 1 :: 2 :: Nil, 10 :: 11 :: Nil) == 10 :: 11 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAList01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), 1 :: 2 :: Nil, 10 :: 11 :: Nil) == Identity.Identity(10 :: 11 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipList                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipList01(): Bool =
        MonadZip.zip(1 :: 2 :: Nil, 10 :: Nil) == (1, 10) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // unzipList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipList01(): Bool =
        MonadZip.unzip((1, 10) :: (2, 11) :: Nil) == (1 :: 2 :: Nil, 10 :: 11 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // zipWithOption                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithOption01(): Bool =
        MonadZip.zipWith(Int32.max, Some(1), None) == None

    @Test
    def zipWithOption02(): Bool =
        MonadZip.zipWith(Int32.max, Some(1), Some(10)) == Some(10)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAOption                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAOption01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Some(1), None) == Identity.Identity(None)

    @Test
    def zipWithAOption02(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Some(1), Some(10)) == Identity.Identity(Some(10))

    /////////////////////////////////////////////////////////////////////////////
    // zipOption                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipOption01(): Bool =
        MonadZip.zip(Some(1), None: Option[Int32]) == None

    @Test
    def zipOption02(): Bool =
        MonadZip.zip(Some(1), Some(10)) == Some((1, 10))


    /////////////////////////////////////////////////////////////////////////////
    // unzipOption                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipOption01(): Bool =
        MonadZip.unzip(None: Option[(Int32, Int32)]) == (None, None)

    @Test
    def unzipOption02(): Bool =
        MonadZip.unzip(Some((1, 10))) == (Some(1), Some(10))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithChain                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithChain01(): Bool =
        MonadZip.zipWith(Int32.max, List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil)) == List.toChain(10 :: 11 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithAChain                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithAChain01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil))
            == Identity.Identity(List.toChain(10 :: 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipChain                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipChain01(): Bool =
        MonadZip.zip(List.toChain(1 :: 2 :: Nil), Chain.singleton(10)) == Chain.singleton((1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // unzipChain                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipChain01(): Bool =
        MonadZip.unzip(List.toChain((1, 10) :: (2, 11) :: Nil)) == (List.toChain(1 :: 2 :: Nil), List.toChain(10 :: 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithNel                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithNel01(): Bool =
        MonadZip.zipWith(Int32.max, Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil)) == Nel.Nel(10, 11 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithANel                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithANel01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil))
            == Identity.Identity(Nel.Nel(10, 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipNel                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipNel01(): Bool =
        MonadZip.zip(Nel.Nel(1, 2 :: Nil), Nel.Nel(10, Nil)) == Nel.Nel((1, 10), Nil)

    /////////////////////////////////////////////////////////////////////////////
    // unzipNel                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipNel01(): Bool =
        MonadZip.unzip(Nel.Nel((1, 10), (2, 11) :: Nil)) == (Nel.Nel(1, 2 :: Nil), Nel.Nel(10, 11 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithNec                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithNec01(): Bool =
        MonadZip.zipWith(Int32.max, necOf2(1, 2), necOf2(10, 11)) == necOf2(10, 11)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithANec                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithANec01(): Bool =
        MonadZip.zipWithA((x, y) -> Identity.Identity(Int32.max(x, y)), necOf2(1, 2), necOf2(10, 11))
            == Identity.Identity(necOf2(10, 11))

    /////////////////////////////////////////////////////////////////////////////
    // zipNec                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipNec01(): Bool =
        MonadZip.zip(necOf2(1, 2), Nec.singleton(10)) == Nec.singleton((1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // unzipNec                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzipNec01(): Bool =
        MonadZip.unzip(necOf2((1, 10), (2, 11))) == (necOf2(1, 2), necOf2(10, 11))

}
