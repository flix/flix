/////////////////////////////////////////////////////////////////////////////
// Logical And                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def testMathAnd01: Bool = (true ∧ true) `assertEq!` true

@test
def testMathAnd02: Bool = (true ∧ false) `assertEq!` false

@test
def testMathAnd03: Bool = (false ∧ true) `assertEq!` false

@test
def testMathAnd04: Bool = (false ∧ false) `assertEq!` false


/////////////////////////////////////////////////////////////////////////////
// Logical Or                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def testMathOr01: Bool = (true ∨ true) `assertEq!` true

@test
def testMathOr02: Bool = (true ∨ false) `assertEq!` true

@test
def testMathOr03: Bool = (false ∨ true) `assertEq!` true

@test
def testMathOr04: Bool = (false ∨ false) `assertEq!` false


/////////////////////////////////////////////////////////////////////////////
// Logical Implication                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def testMathImplication01: Bool = (true → true) `assertEq!` true

@test
def testMathImplication02: Bool = (true → false) `assertEq!` false

@test
def testMathImplication03: Bool = (false → true) `assertEq!` true

@test
def testMathImplication04: Bool = (false → false) `assertEq!` true


/////////////////////////////////////////////////////////////////////////////
// Logical Biconditional                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def testMathBiconditional01: Bool = (true ↔ true) `assertEq!` true

@test
def testMathBiconditional02: Bool = (true ↔ false) `assertEq!` false

@test
def testMathBiconditional03: Bool = (false ↔ true) `assertEq!` false

@test
def testMathBiconditional04: Bool = (false ↔ false) `assertEq!` true
