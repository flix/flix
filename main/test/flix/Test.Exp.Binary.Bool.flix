mod Test.Exp.Binary.Logic {

    use Assert.assertEq

    //
    // Precedence.
    //

    @Test
    def binaryLogical01a(): Unit \ Assert =
        assertEq(expected = true, not false and true)

    @Test
    def binaryLogical01b(): Unit \ Assert =
        assertEq(expected = true, not false and true)

    @Test
    def binaryLogical02a(): Unit \ Assert =
        assertEq(expected = true, true and not false)

    @Test
    def binaryLogical02b(): Unit \ Assert =
        assertEq(expected = true, true and not false)

    @Test
    def binaryLogical03a(): Unit \ Assert =
        assertEq(expected = true, not true or true)

    @Test
    def binaryLogical03b(): Unit \ Assert =
        assertEq(expected = true, not true or true)

    @Test
    def binaryLogical04a(): Unit \ Assert =
        assertEq(expected = true, true or not true)

    @Test
    def binaryLogical04b(): Unit \ Assert =
        assertEq(expected = true, true or not true)

    //
    // Logical Not.
    //

    @Test
    def binaryLogicalNotTest01a(): Unit \ Assert =
        assertEq(expected = false, not true)

    @Test
    def binaryLogicalNotTest01b(): Unit \ Assert =
        assertEq(expected = false, not true)

    @Test
    def binaryLogicalNotTest02a(): Unit \ Assert =
        assertEq(expected = true, not false)

    @Test
    def binaryLogicalNotTest02b(): Unit \ Assert =
        assertEq(expected = true, not false)

    @Test
    def binaryLogicalNotTest03a(): Unit \ Assert =
        assertEq(expected = true, not not true)

    @Test
    def binaryLogicalNotTest03b(): Unit \ Assert =
        assertEq(expected = true, not not true)

    @Test
    def binaryLogicalNotTest04a(): Unit \ Assert =
        assertEq(expected = false, not not false)

    @Test
    def binaryLogicalNotTest04b(): Unit \ Assert =
        assertEq(expected = false, not not false)

    //
    // Logical And.
    //

    @Test
    def testBinaryLogicalAnd01a(): Unit \ Assert =
        assertEq(expected = true, true and true)

    @Test
    def testBinaryLogicalAnd01b(): Unit \ Assert =
        assertEq(expected = true, true and true)

    @Test
    def testBinaryLogicalAnd02a(): Unit \ Assert =
        assertEq(expected = false, true and false)

    @Test
    def testBinaryLogicalAnd02b(): Unit \ Assert =
        assertEq(expected = false, true and false)

    @Test
    def testBinaryLogicalAnd03a(): Unit \ Assert =
        assertEq(expected = false, false and true)

    @Test
    def testBinaryLogicalAnd03b(): Unit \ Assert =
        assertEq(expected = false, false and true)

    @Test
    def testBinaryLogicalAnd04a(): Unit \ Assert =
        assertEq(expected = false, false and false)

    @Test
    def testBinaryLogicalAnd04b(): Unit \ Assert =
        assertEq(expected = false, false and false)

    @Test
    def testBinaryLogicalAnd05a(): Unit \ Assert =
        assertEq(expected = false, false and ???)

    @Test
    def testBinaryLogicalAnd05b(): Unit \ Assert =
        assertEq(expected = false, false and ???)

    @Test
    def testBinaryLogicalAnd05c(): Unit \ Assert =
        assertEq(expected = false, false and {let crash = ???; false})

    @Test
    def testBinaryLogicalAnd06a(): Unit \ Assert =
        assertEq(expected = true, true and true and true)

    @Test
    def testBinaryLogicalAnd06b(): Unit \ Assert =
        assertEq(expected = true, true and true and true)

    //
    // Logical Or.
    //

    @Test
    def binaryLogicalOrTest01a(): Unit \ Assert =
        assertEq(expected = true, true or true)

    @Test
    def binaryLogicalOrTest01b(): Unit \ Assert =
        assertEq(expected = true, true or true)

    @Test
    def binaryLogicalOrTest02a(): Unit \ Assert =
        assertEq(expected = true, true or false)

    @Test
    def binaryLogicalOrTest02b(): Unit \ Assert =
        assertEq(expected = true, true or false)

    @Test
    def binaryLogicalOrTest03a(): Unit \ Assert =
        assertEq(expected = true, false or true)

    @Test
    def binaryLogicalOrTest03b(): Unit \ Assert =
        assertEq(expected = true, false or true)

    @Test
    def binaryLogicalOrTest04a(): Unit \ Assert =
        assertEq(expected = false, false or false)

    @Test
    def binaryLogicalOrTest04b(): Unit \ Assert =
        assertEq(expected = false, false or false)

    @Test
    def binaryLogicalOrTest05a(): Unit \ Assert =
        assertEq(expected = true, true or ???)

    @Test
    def binaryLogicalOrTest05b(): Unit \ Assert =
        assertEq(expected = true, true or ???)

    @Test
    def binaryLogicalOrTest05c(): Unit \ Assert =
        assertEq(expected = true, true or {let crash = ???; true})

    @Test
    def binaryLogicalOrTest06a(): Unit \ Assert =
        assertEq(expected = true, true or true or true)

    @Test
    def binaryLogicalOrTest06b(): Unit \ Assert =
        assertEq(expected = true, true or true or true)

}
