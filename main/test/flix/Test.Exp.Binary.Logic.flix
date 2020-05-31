namespace Test/Exp/Binary/Logic {

    //
    // Logical Not.
    //

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
    def binaryLogicalOrTest01(): Bool = (true || true) == true

    @test
    def binaryLogicalOrTest02(): Bool = (true || false) == true

    @test
    def binaryLogicalOrTest03(): Bool = (false || true) == true

    @test
    def binaryLogicalOrTest04(): Bool = (false || false) == false

    @test
    def binaryLogicalOrTest05(): Bool = (true || ???) == true

}

