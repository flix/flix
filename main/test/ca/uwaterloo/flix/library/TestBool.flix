/*
 * Copyright 2020 Magnus Madsen
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
mod Bool {

    /////////////////////////////////////////////////////////////////////////////
    // Logical And                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def mathAnd01(): Bool = (true ∧ true) == true

    @test
    def mathAnd02(): Bool = (true ∧ false) == false

    @test
    def mathAnd03(): Bool = (false ∧ true) == false

    @test
    def mathAnd04(): Bool = (false ∧ false) == false

    /////////////////////////////////////////////////////////////////////////////
    // Logical Or                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def mathOr01(): Bool = (true ∨ true) == true

    @test
    def mathOr02(): Bool = (true ∨ false) == true

    @test
    def mathOr03(): Bool = (false ∨ true) == true

    @test
    def mathOr04(): Bool = (false ∨ false) == false

    /////////////////////////////////////////////////////////////////////////////
    // Logical Implication                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def mathImplication01(): Bool = (true → true) == true

    @test
    def mathImplication02(): Bool = (true → false) == false

    @test
    def mathImplication03(): Bool = (false → true) == true

    @test
    def mathImplication04(): Bool = (false → false) == true

    /////////////////////////////////////////////////////////////////////////////
    // Logical Bi-implication                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def mathBiconditional01(): Bool = (true ↔ true) == true

    @test
    def mathBiconditional02(): Bool = (true ↔ false) == false

    @test
    def mathBiconditional03(): Bool = (false ↔ true) == false

    @test
    def mathBiconditional04(): Bool = (false ↔ false) == true

    /////////////////////////////////////////////////////////////////////////////
    // Logical Xor                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def mathXor01(): Bool = (true ⊕ true) == false

    @test
    def mathXor02(): Bool = (true ⊕ false) == true

    @test
    def mathXor03(): Bool = (false ⊕ true) == true

    @test
    def mathXor04(): Bool = (false ⊕ false) == false

}
