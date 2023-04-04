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

    @test
    def product01(): Bool =
        product(None: Option[Int32], Some(2)) == None

    @test
    def product02(): Bool =
        product(Some(1), None: Option[Int32]) == None

    @test
    def product03(): Bool =
        product(Some(1), Some(2)) == Some(1, 2)

    @test
    def product04(): Bool =
        product(Nil: List[Int32], 2 :: Nil) == Nil

    @test
    def product05(): Bool =
        product(1 :: Nil, Nil: List[Int32]) == Nil

    @test
    def product06(): Bool =
        product(1 :: Nil, 2 :: Nil) == (1, 2) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // productLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productLeft01(): Bool =
        productLeft(None: Option[Int32], Some(2)) == None

    @test
    def productLeft02(): Bool =
        productLeft(Some(1), None) == None

    @test
    def productLeft03(): Bool =
        productLeft(Some(1), Some(2)) == Some(1)

    @test
    def productLeft04(): Bool =
        productLeft(Nil: List[Int32], 2 :: Nil) == Nil

    @test
    def productLeft05(): Bool =
        productLeft(1 :: Nil, Nil) == Nil

    @test
    def productLeft06(): Bool =
        productLeft(1 :: Nil, 2 :: Nil) == 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // productRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productRight01(): Bool =
        productRight(None, Some(2)) == None

    @test
    def productRight02(): Bool =
        productRight(Some(1), None: Option[Int32]) == None

    @test
    def productRight03(): Bool =
        productRight(Some(1), Some(2)) == Some(2)

    @test
    def productRight04(): Bool =
        productRight(Nil: List[Int32], 2 :: Nil) == Nil

    @test
    def productRight05(): Bool =
        productRight(1 :: Nil, Nil: List[Int32]) == Nil

    @test
    def productRight06(): Bool =
        productRight(1 :: Nil, 2 :: Nil) == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // product3                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product301(): Bool =
        product3(None: Option[Int32], None: Option[Int32], None: Option[Int32]) == None

    @test
    def product302(): Bool =
        product3(None: Option[Int32], Some(2), Some(3)) == None

    @test
    def product303(): Bool =
        product3(Some(1), Some(2), None: Option[Int32]) == None

    @test
    def product304(): Bool =
        product3(Some(1), Some(2), Some(3)) == Some((1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // product4                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product401(): Bool =
        product4(None: Option[Int32], None: Option[Int32], None: Option[Int32], None: Option[Int32]) == None

    @test
    def product402(): Bool =
        product4(None: Option[Int32], Some(2), Some(3), Some(4)) == None

    @test
    def product403(): Bool =
        product4(Some(1), Some(2), Some(3), None: Option[Int32]) == None

    @test
    def product404(): Bool =
        product4(Some(1), Some(2), Some(3), Some(4)) == Some((1, 2, 3, 4))

    /////////////////////////////////////////////////////////////////////////////
    // product5                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product501(): Bool =
        product5(None: Option[Int32], None: Option[Int32], None: Option[Int32], None: Option[Int32], None: Option[Int32]) == None

    @test
    def product502(): Bool =
        product5(None: Option[Int32], Some(2), Some(3), Some(4), Some(5)) == None

    @test
    def product503(): Bool =
        product5(Some(1), Some(2), Some(3), Some(4), None: Option[Int32]) == None

    @test
    def product504(): Bool =
        product5(Some(1), Some(2), Some(3), Some(4), Some(5)) == Some((1, 2, 3, 4, 5))

    /////////////////////////////////////////////////////////////////////////////
    // symAp (<*>)                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symAp01(): Bool =
        None: Option[Int32 -> Int32] <*> None: Option[Int32] == None

    @test
    def symAp02(): Bool =
        Some(x -> x+1) <*> None == None

    @test
    def symAp03(): Bool =
        Some(x -> x+1) <*> Some(1) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // symReversedAp (<**>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symReversedAp01(): Bool =
        None: Option[Int32] <**> None: Option[Int32 -> Int32] == None

    @test
    def symReversedAp02(): Bool =
        None <**> Some(x -> x+1) == None

    @test
    def symReversedAp03(): Bool =
        Some(1) <**> Some(x -> x+1) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // symProductLeft (<*)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symProductLeft01(): Bool =
        None: Option[Int32] <* Some(2) == None

    @test
    def symProductLeft02(): Bool =
        Some(1) <* None == None

    @test
    def symProductLeft03(): Bool =
        Some(1) <* Some(2) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // symProductRight (*>)                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def symProductRight01(): Bool =
        None *> Some(2) == None

    @test
    def symProductRight02(): Bool =
        Some(1) *> None: Option[Int32] == None

    @test
    def symProductRight03(): Bool =
        Some(1) *> Some(2) == Some(2)

}
