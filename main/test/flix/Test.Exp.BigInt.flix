mod Test.Exp.BigInt {

    @test
    def testBigInt01(): BigInt = 0ii

    @test
    def testBigInt02(): BigInt = 1ii

    @test
    def testBigInt03(): BigInt = 10ii

    @test
    def testBigInt04(): BigInt = 9223372036854775808ii

    @test
    def testBigInt05(): BigInt = -9223372036854775809ii

    @test
    def testBigInt6(): BigInt = 1_000ii

    @test
    def testBigInt7(): BigInt = 42_000ii

    @test
    def testBigInt8(): BigInt = 1_000_000ii

    @test
    def testBigInt9(): BigInt = 42_000_000ii

    @test
    def testBigInt10(): BigInt = 1_000_000_000ii

    @test
    def testBigInt11(): BigInt = 42_000_000_000ii

    @test
    def testBigInt12(): BigInt = 1_000_000_000_000_000_000ii

    @test
    def testBigInt13(): BigInt = 42_000_000_000_000_000_000ii

    @test
    def testBigInt14(): BigInt = 1_000_000_000_000_000_000_000ii

    @test
    def testBigInt15(): BigInt = 42_000_000_000_000_000_000_000ii

    @test
    def testBigInt16(): BigInt = 0x1ABCDEF9876543201ABCii

    @test
    def testBigInt17(): BigInt = 0x1A_BC_DE_F9_87_65_43_20_1A_BCii

    @test
    def testBigInt18(): BigInt = 0x0ii

    @test
    def testBigInt19(): BigInt = 0x1ii

    @test
    def testBigInt20(): BigInt = 0x1abcdef9876543201abcii

    @test
    def testBigInt21(): BigInt = 0x1a_bc_de_f9_87_65_43_20_1a_bcii

}
