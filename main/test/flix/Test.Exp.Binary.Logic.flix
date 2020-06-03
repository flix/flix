namespace Test/Exp/Binary/Logic {

    //
    // Precedence.
    //

    @test
    def binaryLogical01a(): Bool = (!false && true) == true

    @test
    def binaryLogical01b(): Bool = (not false and true) == true

    @test
    def binaryLogical02a(): Bool = (true && !false) == true

    @test
    def binaryLogical02b(): Bool = (true and not false) == true

    @test
    def binaryLogical03a(): Bool = (!true || true) == true

    @test
    def binaryLogical03b(): Bool = (not true or true) == true

    @test
    def binaryLogical04a(): Bool = (true || !true) == true

    @test
    def binaryLogical04b(): Bool = (true or not true) == true

    //
    // Logical Not.
    //

    @test
    def binaryLogicalNotTest01a(): Bool = (!true) == false

    @test
    def binaryLogicalNotTest01b(): Bool = (not true) == false

    @test
    def binaryLogicalNotTest02a(): Bool = (!false) == true

    @test
    def binaryLogicalNotTest02b(): Bool = (not false) == true

    @test
    def binaryLogicalNotTest03a(): Bool = (!!true) == true

    @test
    def binaryLogicalNotTest03b(): Bool = (not not true) == true

    @test
    def binaryLogicalNotTest04a(): Bool = (!!false) == false

    @test
    def binaryLogicalNotTest04b(): Bool = (not not false) == false

    //
    // Logical And.
    //

    @test
    def testBinaryLogicalAnd01a(): Bool = (true && true) == true

    @test
    def testBinaryLogicalAnd01b(): Bool = (true and true) == true

    @test
    def testBinaryLogicalAnd02a(): Bool = (true && false) == false

    @test
    def testBinaryLogicalAnd02b(): Bool = (true and false) == false

    @test
    def testBinaryLogicalAnd03a(): Bool = (false && true) == false

    @test
    def testBinaryLogicalAnd03b(): Bool = (false and true) == false

    @test
    def testBinaryLogicalAnd04a(): Bool = (false && false) == false

    @test
    def testBinaryLogicalAnd04b(): Bool = (false and false) == false

    @test
    def testBinaryLogicalAnd05a(): Bool = (false && ???) == false

    @test
    def testBinaryLogicalAnd05b(): Bool = (false and ???) == false

    @test
    def testBinaryLogicalAnd06a(): Bool = (true && true && true) == true

    @test
    def testBinaryLogicalAnd06b(): Bool = (true and true and true) == true

    //
    // Logical Or.
    //

    @test
    def binaryLogicalOrTest01a(): Bool = (true || true) == true

    @test
    def binaryLogicalOrTest01b(): Bool = (true or true) == true

    @test
    def binaryLogicalOrTest02a(): Bool = (true || false) == true

    @test
    def binaryLogicalOrTest02b(): Bool = (true or false) == true

    @test
    def binaryLogicalOrTest03a(): Bool = (false || true) == true

    @test
    def binaryLogicalOrTest03b(): Bool = (false or true) == true

    @test
    def binaryLogicalOrTest04a(): Bool = (false || false) == false

    @test
    def binaryLogicalOrTest04b(): Bool = (false or false) == false

    @test
    def binaryLogicalOrTest05a(): Bool = (true || ???) == true

    @test
    def binaryLogicalOrTest05b(): Bool = (true or ???) == true

    @test
    def binaryLogicalOrTest06a(): Bool = (true || true || true) == true

    @test
    def binaryLogicalOrTest06b(): Bool = (true or true or true) == true

}

