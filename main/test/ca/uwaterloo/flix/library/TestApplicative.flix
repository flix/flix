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

    use Applicative.{product, productLeft, productRight};

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool =
        product(None, Some(2)) == None

    @test
    def product02(): Bool =
        product(Some(1), None) == None

    @test
    def product03(): Bool =
        product(Some(1), Some(2)) == Some(1, 2)

    @test
    def product04(): Bool =
        product(Nil, 2 :: Nil) == Nil

    @test
    def product05(): Bool =
        product(1 :: Nil, Nil) == Nil

    @test
    def product06(): Bool =
        product(1 :: Nil, 2 :: Nil) == (1, 2) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // productLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productLeft01(): Bool =
        productLeft(None, Some(2)) == None

    @test
    def productLeft02(): Bool =
        productLeft(Some(1), None) == None

    @test
    def productLeft03(): Bool =
        productLeft(Some(1), Some(2)) == Some(1)

    @test
    def productLeft04(): Bool =
        productLeft(Nil, 2 :: Nil) == Nil

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
        productRight(Some(1), None) == None

    @test
    def productRight03(): Bool =
        productRight(Some(1), Some(2)) == Some(2)

    @test
    def productRight04(): Bool =
        productRight(Nil, 2 :: Nil) == Nil

    @test
    def productRight05(): Bool =
        productRight(1 :: Nil, Nil) == Nil

    @test
    def productRight06(): Bool =
        productRight(1 :: Nil, 2 :: Nil) == 2 :: Nil

}
