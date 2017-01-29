/////////////////////////////////////////////////////////////////////////////
// Function Composition                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def forwardComposition01: Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f >> g;
        h(0) `assertEq!` 2 // (0 + 1) * 2

@test
def backwardComposition01: Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let h = f << g;
        h(0) `assertEq!` 1 // (0 * 2) + 1

@test
def forwardPipe01: Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = 0 |> f |> g;
        r `assertEq!` 2 // (0 + 1) * 2

@test
def forwardPipe02: Bool =
    let f = match (x, y) -> x + y;
    let r = (1, 2) |> f;
        r `assertEq!` 3

@test
def backwardPipe01: Bool =
    let f = x -> x + 1;
    let g = x -> x * 2;
    let r = f <| (g <| 0);
        r `assertEq!` 1 // (0 * 2) + 1

@test
def backwardPipe02: Bool =
    let f = match (x, y) -> x + y;
    let r = f <| (1, 2);
        r `assertEq!` 3

/////////////////////////////////////////////////////////////////////////////
// Equality and Inequality                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def eq01: Bool = assert!(eq(42, 42))

@test
def neq01: Bool = assert!(neq(21, 42))

/////////////////////////////////////////////////////////////////////////////
// Logical And                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def mathAnd01: Bool = (true ∧ true) `assertEq!` true

@test
def mathAnd02: Bool = (true ∧ false) `assertEq!` false

@test
def mathAnd03: Bool = (false ∧ true) `assertEq!` false

@test
def mathAnd04: Bool = (false ∧ false) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// Logical Or                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def mathOr01: Bool = (true ∨ true) `assertEq!` true

@test
def mathOr02: Bool = (true ∨ false) `assertEq!` true

@test
def mathOr03: Bool = (false ∨ true) `assertEq!` true

@test
def mathOr04: Bool = (false ∨ false) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// Logical Implication                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def mathImplication01: Bool = (true → true) `assertEq!` true

@test
def mathImplication02: Bool = (true → false) `assertEq!` false

@test
def mathImplication03: Bool = (false → true) `assertEq!` true

@test
def mathImplication04: Bool = (false → false) `assertEq!` true

/////////////////////////////////////////////////////////////////////////////
// Logical Biconditional                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def mathBiconditional01: Bool = (true ↔ true) `assertEq!` true

@test
def mathBiconditional02: Bool = (true ↔ false) `assertEq!` false

@test
def mathBiconditional03: Bool = (false ↔ true) `assertEq!` false

@test
def mathBiconditional04: Bool = (false ↔ false) `assertEq!` true
