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

mod TestApplicative {

    use Applicative.{product, productLeft, productRight, product3, product4, product5}
    use Applicative.{<*>, <**>, *>, <*}

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product01(): Bool =
        product(None: Option[Int32], Some(2)) == None

    @Test
    def product02(): Bool =
        product(Some(1), None: Option[Int32]) == None

    @Test
    def product03(): Bool =
        product(Some(1), Some(2)) == Some(1, 2)

    @Test
    def product04(): Bool =
        product(Nil: List[Int32], 2 :: Nil) == Nil

    @Test
    def product05(): Bool =
        product(1 :: Nil, Nil: List[Int32]) == Nil

    @Test
    def product06(): Bool =
        product(1 :: Nil, 2 :: Nil) == (1, 2) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // productLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def productLeft01(): Bool =
        productLeft(None: Option[Int32], Some(2)) == None

    @Test
    def productLeft02(): Bool =
        productLeft(Some(1), None) == None

    @Test
    def productLeft03(): Bool =
        productLeft(Some(1), Some(2)) == Some(1)

    @Test
    def productLeft04(): Bool =
        productLeft(Nil: List[Int32], 2 :: Nil) == Nil

    @Test
    def productLeft05(): Bool =
        productLeft(1 :: Nil, Nil) == Nil

    @Test
    def productLeft06(): Bool =
        productLeft(1 :: Nil, 2 :: Nil) == 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // productRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def productRight01(): Bool =
        productRight(None, Some(2)) == None

    @Test
    def productRight02(): Bool =
        productRight(Some(1), None: Option[Int32]) == None

    @Test
    def productRight03(): Bool =
        productRight(Some(1), Some(2)) == Some(2)

    @Test
    def productRight04(): Bool =
        productRight(Nil: List[Int32], 2 :: Nil) == Nil

    @Test
    def productRight05(): Bool =
        productRight(1 :: Nil, Nil: List[Int32]) == Nil

    @Test
    def productRight06(): Bool =
        productRight(1 :: Nil, 2 :: Nil) == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // product3                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product301(): Bool =
        product3(None: Option[Int32], None: Option[Int32], None: Option[Int32]) == None

    @Test
    def product302(): Bool =
        product3(None: Option[Int32], Some(2), Some(3)) == None

    @Test
    def product303(): Bool =
        product3(Some(1), Some(2), None: Option[Int32]) == None

    @Test
    def product304(): Bool =
        product3(Some(1), Some(2), Some(3)) == Some((1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // product4                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product401(): Bool =
        product4(None: Option[Int32], None: Option[Int32], None: Option[Int32], None: Option[Int32]) == None

    @Test
    def product402(): Bool =
        product4(None: Option[Int32], Some(2), Some(3), Some(4)) == None

    @Test
    def product403(): Bool =
        product4(Some(1), Some(2), Some(3), None: Option[Int32]) == None

    @Test
    def product404(): Bool =
        product4(Some(1), Some(2), Some(3), Some(4)) == Some((1, 2, 3, 4))

    /////////////////////////////////////////////////////////////////////////////
    // product5                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product501(): Bool =
        product5(None: Option[Int32], None: Option[Int32], None: Option[Int32], None: Option[Int32], None: Option[Int32]) == None

    @Test
    def product502(): Bool =
        product5(None: Option[Int32], Some(2), Some(3), Some(4), Some(5)) == None

    @Test
    def product503(): Bool =
        product5(Some(1), Some(2), Some(3), Some(4), None: Option[Int32]) == None

    @Test
    def product504(): Bool =
        product5(Some(1), Some(2), Some(3), Some(4), Some(5)) == Some((1, 2, 3, 4, 5))

    /////////////////////////////////////////////////////////////////////////////
    // symAp (<*>)                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symAp01(): Bool =
        None: Option[Int32 -> Int32] <*> None: Option[Int32] == None

    @Test
    def symAp02(): Bool =
        Some(x -> x+1) <*> None == None

    @Test
    def symAp03(): Bool =
        Some(x -> x+1) <*> Some(1) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // symReversedAp (<**>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symReversedAp01(): Bool =
        None: Option[Int32] <**> None: Option[Int32 -> Int32] == None

    @Test
    def symReversedAp02(): Bool =
        None <**> Some(x -> x+1) == None

    @Test
    def symReversedAp03(): Bool =
        Some(1) <**> Some(x -> x+1) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // symProductLeft (<*)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symProductLeft01(): Bool =
        None: Option[Int32] <* Some(2) == None

    @Test
    def symProductLeft02(): Bool =
        Some(1) <* None == None

    @Test
    def symProductLeft03(): Bool =
        Some(1) <* Some(2) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // symProductRight (*>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def symProductRight01(): Bool =
        None *> Some(2) == None

    @Test
    def symProductRight02(): Bool =
        Some(1) *> None: Option[Int32] == None

    @Test
    def symProductRight03(): Bool =
        Some(1) *> Some(2) == Some(2)

}
