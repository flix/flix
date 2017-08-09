/////////////////////////////////////////////////////////////////////////////
// Expression.Binary (Logical)                                             //
// BinaryOperator.{LogicalAnd,LogicalOr,Implication,Biconditional}         //
/////////////////////////////////////////////////////////////////////////////

@test
def testBinaryLogicalAnd01(): Bool = assertEq!(true && true, true)

@test
def testBinaryLogicalAnd02(): Bool = assertEq!(true && false, false)

@test
def testBinaryLogicalAnd03(): Bool = assertEq!(false && true, false)

@test
def testBinaryLogicalAnd04(): Bool = assertEq!(false && false, false)

@test
def testBinaryLogicalAnd05(): Bool = assertEq!(false && ???, false)

@test
def binaryLogicalOrTest01(): Bool = assertEq!(true || true, true)

@test
def binaryLogicalOrTest02(): Bool = assertEq!(true || false, true)

@test
def binaryLogicalOrTest03(): Bool = assertEq!(false || true, true)

@test
def binaryLogicalOrTest04(): Bool = assertEq!(false || false, false)

@test
def binaryLogicalOrTest05(): Bool = assertEq!(true || ???, true)