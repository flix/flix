/*
 * Copyright 2025 Jakob Schneider Villumsen
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

mod Test.Exp.Fixpoint.Inject {

    @test
    def inject01(): #{ A(Bool) } = {
        inject (Nil: List[Bool]) into A/1
    }

    @test
    def inject02(): #{ A(Char) } = {
        inject (Nil: List[Char]) into A/1
    }

    @test
    def inject03(): #{ A(Float32) } = {
        inject (Nil: List[Float32]) into A/1
    }

    @test
    def inject04(): #{ A(Float64) } = {
        inject (Nil: List[Float64]) into A/1
    }

    @test
    def inject05(): #{ A(BigDecimal) } = {
        inject (Nil: List[BigDecimal]) into A/1
    }

    @test
    def inject06(): #{ A(Int8) } = {
        inject (Nil: List[Int8]) into A/1
    }

    @test
    def inject07(): #{ A(Int16) } = {
        inject (Nil: List[Int16]) into A/1
    }

    @test
    def inject08(): #{ A(Int32) } = {
        inject (Nil: List[Int32]) into A/1
    }

    @test
    def inject09(): #{ A(Int64) } = {
        inject (Nil: List[Int64]) into A/1
    }

    @test
    def inject10(): #{ A(BigInt) } = {
        inject (Nil: List[BigInt]) into A/1
    }

    @test
    def inject11(): #{ A(String) } = {
        inject (Nil: List[String]) into A/1
    }

    @test
    def inject12(): #{ A(Int32) } = {
        inject List#{1, 2, 3} into A/1
    }

    @test
    def inject13(): #{ A(Int32) } = {
        inject List#{1, 2, 3} into A/1
    }

    @test
    def inject14(): #{ A(Int32), B(Int32) } = {
        inject List#{1, 2, 3}, List#{4, 5, 6} into A/1, B/1
    }

    @test
    def inject15(): #{ A(Int32), B(Char) } = {
        inject List#{1, 2, 3}, List#{'a', 'b', 'c'} into A/1, B/1
    }

    @test
    def inject16(): #{ A(Int32, Int32) } = {
        inject List#{(1, 2), (2, 3), (3, 4)} into A/2
    }

    @test
    def inject17(): #{ A(Int32, Int32), B(Int32) } = {
        inject List#{(1, 2), (2, 3), (3, 4)}, List#{4, 5, 6} into A/2, B/1
    }

    @test
    def inject18(): #{ A(Int32, Int32), B(Char) } = {
        inject List#{(1, 2), (2, 3), (3, 4)}, List#{'a', 'b', 'c'} into A/2, B/1
    }

    @test
    def inject19(): #{ A(Int32, Int32), B(Char, Int32) } = {
        inject List#{(1, 2), (2, 3), (3, 4)}, List#{('a', 4), ('b', 5), ('c', 6)} into A/2, B/2
    }

    @test
    def inject20(): #{ A(Int32, Int32), B(Int32, Int32, Int32) } = {
        inject List#{(1, 2), (2, 3), (3, 4)}, List#{(4, 5, 6), (5, 6, 7), (6, 7, 8)} into A/2, B/3
    }

    @test
    def inject21(): #{ A(Int32, Int32), B(Int32, Int32, Int32), C(Float32) } = {
        inject List#{(1, 2), (2, 3), (3, 4)}, List#{(4, 5, 6), (5, 6, 7), (6, 7, 8)}, Set#{1.0f32, 2.0f32, 3.0f32} into A/2, B/3, C/1
    }

}
