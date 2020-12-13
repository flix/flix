/*
 *  Copyright 2020 Matthew Lutze
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

namespace TestToString {

    use ToString.toString;

    /////////////////////////////////////////////////////////////////////////////
    // Tuple2                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple201(): Bool = toString((1, 1)) == "(1, 1)"

    @test
    def tuple202(): Bool = toString((1, Some(2))) == "(1, Some(2))"

    @test
    def tuple203(): Bool = toString((true, 1)) == "(true, 1)"

    @test
    def tuple204(): Bool = toString(((1, 2), 91)) == "((1, 2), 91)"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple3                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple301(): Bool = toString((1, 1, 82)) == "(1, 1, 82)"

    @test
    def tuple302(): Bool = toString((1, Some(2), 3)) == "(1, Some(2), 3)"

    @test
    def tuple303(): Bool = toString((true, 1, false)) == "(true, 1, false)"

    @test
    def tuple304(): Bool = toString(((1, 2), 91, true)) == "((1, 2), 91, true)"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple4                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple401(): Bool =
        toString((1, 1, 2, 3)) == "(1, 1, 2, 3)"

    @test
    def tuple402(): Bool =
        toString((1, Some(2), true, false)) == "(1, Some(2), true, false)"

    @test
    def tuple403(): Bool =
        toString((true, 1, 97, 2)) == "(true, 1, 97, 2)"

    @test
    def tuple404(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4)) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4)"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple5                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple501(): Bool =
        toString((1, 1, 2, 3, 9)) == "(1, 1, 2, 3, 9)"

    @test
    def tuple502(): Bool =
        toString((1, Some(2), true, false, true)) == "(1, Some(2), true, false, true)"

    @test
    def tuple503(): Bool =
        toString((true, 1, 97, 2, 9)) == "(true, 1, 97, 2, 9)"

    @test
    def tuple504(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple6                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple601(): Bool =
        toString((1, 1, 2, 3, 9, 42)) == "(1, 1, 2, 3, 9, 42)"

    @test
    def tuple602(): Bool =
        toString((1, Some(2), true, false, true, 0)) == "(1, Some(2), true, false, true, 0)"

    @test
    def tuple603(): Bool =
        toString((true, 1, 97, 2, 9, false)) == "(true, 1, 97, 2, 9, false)"

    @test
    def tuple604(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple7                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple701(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2)) == "(1, 1, 2, 3, 9, 42, 2)"

    @test
    def tuple702(): Bool =
        toString((1, Some(2), true, false, true, 0, false)) == "(1, Some(2), true, false, true, 0, false)"

    @test
    def tuple703(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42))) == "(true, 1, 97, 2, 9, false, Some(42))"

    @test
    def tuple704(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple8                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple801(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2, 0)) == "(1, 1, 2, 3, 9, 42, 2, 0)"

    @test
    def tuple802(): Bool =
        toString((1, Some(2), true, false, true, 0, false, Some(true))) == "(1, Some(2), true, false, true, 0, false, Some(true))"

    @test
    def tuple803(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42), 1i64)) == "(true, 1, 97, 2, 9, false, Some(42), 1)"

    @test
    def tuple804(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple9                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple901(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2, 0, 9)) == "(1, 1, 2, 3, 9, 42, 2, 0, 9)"

    @test
    def tuple902(): Bool =
        toString((1, Some(2), true, false, true, 0, false, Some(true), 1)) == "(1, Some(2), true, false, true, 0, false, Some(true), 1)"

    @test
    def tuple903(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32)) == "(true, 1, 97, 2, 9, false, Some(42), 1, 432)"

    @test
    def tuple904(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false))"

    /////////////////////////////////////////////////////////////////////////////
    // Tuple10                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple1001(): Bool =
        toString((1, 1, 2, 3, 9, 42, 2, 0, 9, 342)) == "(1, 1, 2, 3, 9, 42, 2, 0, 9, 342)"

    @test
    def tuple1002(): Bool =
        toString((1, Some(2), true, false, true, 0, false, Some(true), 1, 1337)) == "(1, Some(2), true, false, true, 0, false, Some(true), 1, 1337)"

    @test
    def tuple1003(): Bool =
        toString((true, 1, 97, 2, 9, false, Some(42), 1i64, 432i32, Some(false))) == "(true, 1, 97, 2, 9, false, Some(42), 1, 432, Some(false))"

    @test
    def tuple1004(): Bool =
        toString(((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true))) == "((1, 2, 4, 7), (1, 2), (1, 2, 4), 4, (1, 2, 3, 4, 5, 9, 1, 42, false, true))"

}