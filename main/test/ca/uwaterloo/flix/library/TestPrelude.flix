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

/////////////////////////////////////////////////////////////////////////////
// identity                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def identity01(): Bool = identity(5) == 5

@test
def identity02(): Bool = identity(true) == true

@test
def identity03(): Bool = identity(6ii) == 6ii

/////////////////////////////////////////////////////////////////////////////
// constant                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def constant01(): Bool = (true |> constant(5)) == 5

@test
def constant02(): Bool = (5 |> constant(true)) == true

@test
def constant03(): Bool = (Nil |> constant(2ii)) == 2ii

/////////////////////////////////////////////////////////////////////////////
// flip                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def flip01(): Bool =
    let f = flip((x, y) -> x - y);
    f(5, 4) == -1

@test
def flip02(): Bool =
    let f = flip((x, y) -> x - y);
    f(4, 5) == 1

@test
def flip03(): Bool =
    let f = flip((x, y) -> x / y);
    f(10, 3) == 0

@test
def flip04(): Bool =
    let f = flip((x, y) -> x / y);
    f(3, 10) == 3

/////////////////////////////////////////////////////////////////////////////
// curry                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def curry01(): Bool =
    let f = curry((x, y) -> x - y);
    (4 |> f(5)) == 1

@test
def curry02(): Bool =
    let f = curry((x, y) -> x - y);
    (5 |> f(4)) == -1

@test
def curry03(): Bool =
    let f = curry((x, y) -> x / y);
    (3 |> f(10)) == 3

@test
def curry04(): Bool =
    let f = curry((x, y) -> x / y);
    (10 |> f(3)) == 0

/////////////////////////////////////////////////////////////////////////////
// uncurry                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def uncurry01(): Bool =
    let f = uncurry(x -> y -> x - y);
    f(5, 4) == 1

@test
def uncurry02(): Bool =
    let f = uncurry(x -> y -> x - y);
    f(4, 5) == -1

@test
def uncurry03(): Bool =
    let f = uncurry(x -> y -> x / y);
    f(10, 3) == 3

@test
def uncurry04(): Bool =
    let f = uncurry(x -> y -> x / y);
    f(3, 10) == 0

/////////////////////////////////////////////////////////////////////////////
// fst                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def fst01(): Bool = fst((5, 0.0f32)) == 5

@test
def fst02(): Bool = fst((0.0f32, 5)) == 0.0f32

/////////////////////////////////////////////////////////////////////////////
// snd                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def snd01(): Bool = snd((5, 0.0f32)) == 0.0f32

@test
def snd02(): Bool = snd((0.0f32, 5)) == 5

/////////////////////////////////////////////////////////////////////////////
// swap                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def swap01(): Bool = swap((5, 0.0f32)) == (0.0f32, 5)

@test
def swap02(): Bool = swap((0.0f32, 5)) == (5, 0.0f32)

/////////////////////////////////////////////////////////////////////////////
// Function Composition                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def forwardComposition01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f >> g;
        h(0) == 2 // (0 + 1) * 2

@test
def backwardComposition01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f << g;
        h(0) == 1 // (0 * 2) + 1

@test
def forwardPipe01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = 0 |> f |> g;
        r == 2 // (0 + 1) * 2

@test
def forwardPipe02(): Bool =
    let f = match (x, y) -> x + y;
    let r = (1, 2) |> f;
        r == 3

@test
def backwardPipe01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = f <| (g <| 0);
        r == 1 // (0 * 2) + 1

@test
def backwardPipe02(): Bool =
    let f = match (x, y) -> x + y;
    let r = f <| (1, 2);
        r == 3

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
// Logical Biconditional                                                   //
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

/////////////////////////////////////////////////////////////////////////////
// Sheffer Stroke                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def mathSheffer01(): Bool = (true ↑ true) == false

@test
def mathSheffer02(): Bool = (true ↑ false) == true

@test
def mathSheffer03(): Bool = (false ↑ true) == true

@test
def mathSheffer04(): Bool = (false ↑ false) == true