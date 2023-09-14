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
    @Test
    def mathAnd01(): Bool = (true ∧ true) == true

    @Test
    def mathAnd02(): Bool = (true ∧ false) == false

    @Test
    def mathAnd03(): Bool = (false ∧ true) == false

    @Test
    def mathAnd04(): Bool = (false ∧ false) == false

    /////////////////////////////////////////////////////////////////////////////
    // Logical Or                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mathOr01(): Bool = (true ∨ true) == true

    @Test
    def mathOr02(): Bool = (true ∨ false) == true

    @Test
    def mathOr03(): Bool = (false ∨ true) == true

    @Test
    def mathOr04(): Bool = (false ∨ false) == false

    /////////////////////////////////////////////////////////////////////////////
    // Logical Implication                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mathImplication01(): Bool = (true → true) == true

    @Test
    def mathImplication02(): Bool = (true → false) == false

    @Test
    def mathImplication03(): Bool = (false → true) == true

    @Test
    def mathImplication04(): Bool = (false → false) == true

    /////////////////////////////////////////////////////////////////////////////
    // Logical Bi-implication                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mathBiconditional01(): Bool = (true ↔ true) == true

    @Test
    def mathBiconditional02(): Bool = (true ↔ false) == false

    @Test
    def mathBiconditional03(): Bool = (false ↔ true) == false

    @Test
    def mathBiconditional04(): Bool = (false ↔ false) == true

    /////////////////////////////////////////////////////////////////////////////
    // Logical Xor                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mathXor01(): Bool = (true ⊕ true) == false

    @Test
    def mathXor02(): Bool = (true ⊕ false) == true

    @Test
    def mathXor03(): Bool = (false ⊕ true) == true

    @Test
    def mathXor04(): Bool = (false ⊕ false) == false

    /////////////////////////////////////////////////////////////////////////////
    // Negation                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def notTest01(): Bool = not(true) == false

    @Test
    def notTest02(): Bool = not(false) == true

    /////////////////////////////////////////////////////////////////////////////
    // Logical And (lazy)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def andTest01(): Bool = and(false, lazy false) == false

    @Test
    def andTest02(): Bool = and(false, lazy true) == false

    @Test
    def andTest03(): Bool = and(true, lazy false) == false

    @Test
    def andTest04(): Bool = and(true, lazy true) == true

    /////////////////////////////////////////////////////////////////////////////
    // Logical Or (lazy)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def orTest01(): Bool = or(false, lazy false) == false

    @Test
    def orTest02(): Bool = or(false, lazy true) == true

    @Test
    def orTest03(): Bool = or(true, lazy false) == true

    @Test
    def orTest04(): Bool = or(true, lazy true) == true

}
