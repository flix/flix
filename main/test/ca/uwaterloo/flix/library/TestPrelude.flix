/////////////////////////////////////////////////////////////////////////////
// identity                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def identity01(): Bool = identity(5) `assertEq!` 5

@test
def identity02(): Bool = identity(true) `assertEq!` true

@test
def identity03(): Bool = identity(6ii) `assertEq!` 6ii

/////////////////////////////////////////////////////////////////////////////
// constant                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def constant01(): Bool = (true |> constant(5)) `assertEq!` 5

@test
def constant02(): Bool = (5 |> constant(true)) `assertEq!` true

@test
def constant03(): Bool = (Nil |> constant(2ii)) `assertEq!` 2ii

/////////////////////////////////////////////////////////////////////////////
// flip                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def flip01(): Bool =
    let f = flip((x, y) -> x - y);
    f(5, 4) `assertEq!` -1

@test
def flip02(): Bool =
    let f = flip((x, y) -> x - y);
    f(4, 5) `assertEq!` 1

@test
def flip03(): Bool =
    let f = flip((x, y) -> x / y);
    f(10, 3) `assertEq!` 0

@test
def flip04(): Bool =
    let f = flip((x, y) -> x / y);
    f(3, 10) `assertEq!` 3

/////////////////////////////////////////////////////////////////////////////
// curry                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def curry01(): Bool =
    let f = curry((x, y) -> x - y);
    (4 |> f(5)) `assertEq!` 1

@test
def curry02(): Bool =
    let f = curry((x, y) -> x - y);
    (5 |> f(4)) `assertEq!` -1

@test
def curry03(): Bool =
    let f = curry((x, y) -> x / y);
    (3 |> f(10)) `assertEq!` 3

@test
def curry04(): Bool =
    let f = curry((x, y) -> x / y);
    (10 |> f(3)) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// uncurry                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def uncurry01(): Bool =
    let f = uncurry(x -> y -> x - y);
    f(5, 4) `assertEq!` 1

@test
def uncurry02(): Bool =
    let f = uncurry(x -> y -> x - y);
    f(4, 5) `assertEq!` -1

@test
def uncurry03(): Bool =
    let f = uncurry(x -> y -> x / y);
    f(10, 3) `assertEq!` 3

@test
def uncurry04(): Bool =
    let f = uncurry(x -> y -> x / y);
    f(3, 10) `assertEq!` 0

/////////////////////////////////////////////////////////////////////////////
// fst                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def fst01(): Bool = fst((5, 0.0f32)) `assertEq!` 5

@test
def fst02(): Bool = fst((0.0f32, 5)) `assertEq!` 0.0f32

/////////////////////////////////////////////////////////////////////////////
// snd                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def snd01(): Bool = snd((5, 0.0f32)) `assertEq!` 0.0f32

@test
def snd02(): Bool = snd((0.0f32, 5)) `assertEq!` 5

/////////////////////////////////////////////////////////////////////////////
// swap                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def swap01(): Bool = swap((5, 0.0f32)) `assertEq!` (0.0f32, 5)

@test
def swap02(): Bool = swap((0.0f32, 5)) `assertEq!` (5, 0.0f32)

/////////////////////////////////////////////////////////////////////////////
// Function Composition                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def forwardComposition01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f >> g;
        h(0) `assertEq!` 2 // (0 + 1) * 2

@test
def backwardComposition01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f << g;
        h(0) `assertEq!` 1 // (0 * 2) + 1

@test
def forwardPipe01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = 0 |> f |> g;
        r `assertEq!` 2 // (0 + 1) * 2

@test
def forwardPipe02(): Bool =
    let f = match (x, y) -> x + y;
    let r = (1, 2) |> f;
        r `assertEq!` 3

@test
def backwardPipe01(): Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = f <| (g <| 0);
        r `assertEq!` 1 // (0 * 2) + 1

@test
def backwardPipe02(): Bool =
    let f = match (x, y) -> x + y;
    let r = f <| (1, 2);
        r `assertEq!` 3

/////////////////////////////////////////////////////////////////////////////
// Logical And                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def mathAnd01(): Bool = (true ∧ true) `assertEq!` true

@test
def mathAnd02(): Bool = (true ∧ false) `assertEq!` false

@test
def mathAnd03(): Bool = (false ∧ true) `assertEq!` false

@test
def mathAnd04(): Bool = (false ∧ false) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// Logical Or                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def mathOr01(): Bool = (true ∨ true) `assertEq!` true

@test
def mathOr02(): Bool = (true ∨ false) `assertEq!` true

@test
def mathOr03(): Bool = (false ∨ true) `assertEq!` true

@test
def mathOr04(): Bool = (false ∨ false) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// Logical Implication                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def mathImplication01(): Bool = (true → true) `assertEq!` true

@test
def mathImplication02(): Bool = (true → false) `assertEq!` false

@test
def mathImplication03(): Bool = (false → true) `assertEq!` true

@test
def mathImplication04(): Bool = (false → false) `assertEq!` true

/////////////////////////////////////////////////////////////////////////////
// Logical Biconditional                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def mathBiconditional01(): Bool = (true ↔ true) `assertEq!` true

@test
def mathBiconditional02(): Bool = (true ↔ false) `assertEq!` false

@test
def mathBiconditional03(): Bool = (false ↔ true) `assertEq!` false

@test
def mathBiconditional04(): Bool = (false ↔ false) `assertEq!` true

/////////////////////////////////////////////////////////////////////////////
// Logical Xor                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def mathXor01(): Bool = (true ⊕ true) `assertEq!` false

@test
def mathXor02(): Bool = (true ⊕ false) `assertEq!` true

@test
def mathXor03(): Bool = (false ⊕ true) `assertEq!` true

@test
def mathXor04(): Bool = (false ⊕ false) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// Sheffer Stroke                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def mathSheffer01(): Bool = (true ↑ true) `assertEq!` false

@test
def mathSheffer02(): Bool = (true ↑ false) `assertEq!` true

@test
def mathSheffer03(): Bool = (false ↑ true) `assertEq!` true

@test
def mathSheffer04(): Bool = (false ↑ false) `assertEq!` true