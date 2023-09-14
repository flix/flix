mod Test.Exp.Binary.Logic {

    //
    // Precedence.
    //

    @Test
    def binaryLogical01a(): Bool = (not false and true) == true

    @Test
    def binaryLogical01b(): Bool = (not false and true) == true

    @Test
    def binaryLogical02a(): Bool = (true and not false) == true

    @Test
    def binaryLogical02b(): Bool = (true and not false) == true

    @Test
    def binaryLogical03a(): Bool = (not true or true) == true

    @Test
    def binaryLogical03b(): Bool = (not true or true) == true

    @Test
    def binaryLogical04a(): Bool = (true or not true) == true

    @Test
    def binaryLogical04b(): Bool = (true or not true) == true

    //
    // Logical Not.
    //

    @Test
    def binaryLogicalNotTest01a(): Bool = (not true) == false

    @Test
    def binaryLogicalNotTest01b(): Bool = (not true) == false

    @Test
    def binaryLogicalNotTest02a(): Bool = (not false) == true

    @Test
    def binaryLogicalNotTest02b(): Bool = (not false) == true

    @Test
    def binaryLogicalNotTest03a(): Bool = (not not true) == true

    @Test
    def binaryLogicalNotTest03b(): Bool = (not not true) == true

    @Test
    def binaryLogicalNotTest04a(): Bool = (not not false) == false

    @Test
    def binaryLogicalNotTest04b(): Bool = (not not false) == false

    //
    // Logical And.
    //

    @Test
    def testBinaryLogicalAnd01a(): Bool = (true and true) == true

    @Test
    def testBinaryLogicalAnd01b(): Bool = (true and true) == true

    @Test
    def testBinaryLogicalAnd02a(): Bool = (true and false) == false

    @Test
    def testBinaryLogicalAnd02b(): Bool = (true and false) == false

    @Test
    def testBinaryLogicalAnd03a(): Bool = (false and true) == false

    @Test
    def testBinaryLogicalAnd03b(): Bool = (false and true) == false

    @Test
    def testBinaryLogicalAnd04a(): Bool = (false and false) == false

    @Test
    def testBinaryLogicalAnd04b(): Bool = (false and false) == false

    @Test
    def testBinaryLogicalAnd05a(): Bool = (false and ???) == false

    @Test
    def testBinaryLogicalAnd05b(): Bool = (false and ???) == false

    @Test
    def testBinaryLogicalAnd06a(): Bool = (true and true and true) == true

    @Test
    def testBinaryLogicalAnd06b(): Bool = (true and true and true) == true

    //
    // Logical Or.
    //

    @Test
    def binaryLogicalOrTest01a(): Bool = (true or true) == true

    @Test
    def binaryLogicalOrTest01b(): Bool = (true or true) == true

    @Test
    def binaryLogicalOrTest02a(): Bool = (true or false) == true

    @Test
    def binaryLogicalOrTest02b(): Bool = (true or false) == true

    @Test
    def binaryLogicalOrTest03a(): Bool = (false or true) == true

    @Test
    def binaryLogicalOrTest03b(): Bool = (false or true) == true

    @Test
    def binaryLogicalOrTest04a(): Bool = (false or false) == false

    @Test
    def binaryLogicalOrTest04b(): Bool = (false or false) == false

    @Test
    def binaryLogicalOrTest05a(): Bool = (true or ???) == true

    @Test
    def binaryLogicalOrTest05b(): Bool = (true or ???) == true

    @Test
    def binaryLogicalOrTest06a(): Bool = (true or true or true) == true

    @Test
    def binaryLogicalOrTest06b(): Bool = (true or true or true) == true

}

