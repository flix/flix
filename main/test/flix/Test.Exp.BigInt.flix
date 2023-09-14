mod Test.Exp.BigInt {

    @Test
    def testBigInt01(): BigInt = 0ii

    @Test
    def testBigInt02(): BigInt = 1ii

    @Test
    def testBigInt03(): BigInt = 10ii

    @Test
    def testBigInt04(): BigInt = 9223372036854775808ii

    @Test
    def testBigInt05(): BigInt = -9223372036854775809ii

    @Test
    def testBigInt6(): BigInt = 1_000ii

    @Test
    def testBigInt7(): BigInt = 42_000ii

    @Test
    def testBigInt8(): BigInt = 1_000_000ii

    @Test
    def testBigInt9(): BigInt = 42_000_000ii

    @Test
    def testBigInt10(): BigInt = 1_000_000_000ii

    @Test
    def testBigInt11(): BigInt = 42_000_000_000ii

    @Test
    def testBigInt12(): BigInt = 1_000_000_000_000_000_000ii

    @Test
    def testBigInt13(): BigInt = 42_000_000_000_000_000_000ii

    @Test
    def testBigInt14(): BigInt = 1_000_000_000_000_000_000_000ii

    @Test
    def testBigInt15(): BigInt = 42_000_000_000_000_000_000_000ii

    @Test
    def testBigInt16(): BigInt = 0x1ABCDEF9876543201ABCii

    @Test
    def testBigInt17(): BigInt = 0x1A_BC_DE_F9_87_65_43_20_1A_BCii

    @Test
    def testBigInt18(): BigInt = 0x0ii

    @Test
    def testBigInt19(): BigInt = 0x1ii

    @Test
    def testBigInt20(): BigInt = 0x1abcdef9876543201abcii

    @Test
    def testBigInt21(): BigInt = 0x1a_bc_de_f9_87_65_43_20_1a_bcii

}
