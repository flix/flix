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
