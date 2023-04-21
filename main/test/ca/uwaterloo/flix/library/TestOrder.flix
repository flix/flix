/*
 *  Copyright 2020 Esben Bjerre
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

mod TestOrder {

    /////////////////////////////////////////////////////////////////////////////
    // fromInt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromInt01(): Bool = Comparison.fromInt32(-1) == Comparison.LessThan

    @test
    def fromInt02(): Bool = Comparison.fromInt32(0) == Comparison.EqualTo

    @test
    def fromInt03(): Bool = Comparison.fromInt32(+1) == Comparison.GreaterThan

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def min01(): Bool = Order.min(0, 1) == 0

    @test
    def min02(): Bool = Order.min((), ()) == ()

    @test
    def min03(): Bool = Order.min(true, false) == false

    @test
    def min04(): Bool = Order.min(342, 42) == 42

    @test
    def min05(): Bool = Order.min('a', 'z') == 'a'

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def max01(): Bool = Order.max(0, 1) == 1

    @test
    def max02(): Bool = Order.max((), ()) == ()

    @test
    def max03(): Bool = Order.max(true, false) == true

    @test
    def max04(): Bool = Order.max(3, -3) == 3

    @test
    def max05(): Bool = Order.max('a', 'z') == 'z'

}
