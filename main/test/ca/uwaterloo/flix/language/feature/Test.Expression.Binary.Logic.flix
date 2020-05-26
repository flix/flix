/////////////////////////////////////////////////////////////////////////////
// Expression.Binary (Logical)                                             //
// BinaryOperator.{LogicalAnd,LogicalOr,Implication,Biconditional}         //
/////////////////////////////////////////////////////////////////////////////

@test
def testBinaryLogicalAnd01(): Bool = (true && true) == true

@test
def testBinaryLogicalAnd02(): Bool = (true && false) == false

@test
def testBinaryLogicalAnd03(): Bool = (false && true) == false

@test
def testBinaryLogicalAnd04(): Bool = (false && false) == false

@test
def testBinaryLogicalAnd05(): Bool = (false && ???) == false

@test
def binaryLogicalOrTest01(): Bool = (true || true) == true

@test
def binaryLogicalOrTest02(): Bool = (true || false) == true

@test
def binaryLogicalOrTest03(): Bool = (false || true) == true

@test
def binaryLogicalOrTest04(): Bool = (false || false) == false

@test
def binaryLogicalOrTest05(): Bool = (true || ???) == true