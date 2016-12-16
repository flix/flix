/////////////////////////////////////////////////////////////////////////////
// Logical And                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def testAnd01: Bool = true ∧ true


//

//  test("∧.01") {
//    val input = "def r: Bool = true ∧ true"
//    runTest(input, Value.True)
//  }
//
//  test("∧.02") {
//    val input = "def r: Bool = true ∧ false"
//    runTest(input, Value.False)
//  }
//
//  test("∧.03") {
//    val input = "def r: Bool = false ∧ true"
//    runTest(input, Value.False)
//  }
//
//  test("∧.04") {
//    val input = "def r: Bool = false ∧ false"
//    runTest(input, Value.False)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Logical Or                                                              //
//  /////////////////////////////////////////////////////////////////////////////
//  test("∨.01") {
//    val input = "def r: Bool = true ∨ true"
//    runTest(input, Value.True)
//  }
//
//  test("∨.02") {
//    val input = "def r: Bool = true ∨ false"
//    runTest(input, Value.True)
//  }
//
//  test("∨.03") {
//    val input = "def r: Bool = false ∨ true"
//    runTest(input, Value.True)
//  }
//
//  test("∨.04") {
//    val input = "def r: Bool = false ∨ false"
//    runTest(input, Value.False)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Logical Implication                                                     //
//  /////////////////////////////////////////////////////////////////////////////
//  test("→.01") {
//    val input = "def r: Bool = true → true"
//    runTest(input, Value.True)
//  }
//
//  test("→.02") {
//    val input = "def r: Bool = true → false"
//    runTest(input, Value.False)
//  }
//
//  test("→.03") {
//    val input = "def r: Bool = false → true"
//    runTest(input, Value.True)
//  }
//
//  test("→.04") {
//    val input = "def r: Bool = false → false"
//    runTest(input, Value.True)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Logical Biconditional                                                   //
//  /////////////////////////////////////////////////////////////////////////////
//  test("↔.01") {
//    val input = "def r: Bool = true ↔ true"
//    runTest(input, Value.True)
//  }
//
//  test("↔.02") {
//    val input = "def r: Bool = true ↔ false"
//    runTest(input, Value.False)
//  }
//
//  test("↔.03") {
//    val input = "def r: Bool = false ↔ true"
//    runTest(input, Value.False)
//  }
//
//  test("↔.04") {
//    val input = "def r: Bool = false ↔ false"
//    runTest(input, Value.True)
//  }