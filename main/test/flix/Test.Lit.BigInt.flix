mod Test.Lit.BigInt {

    @CompileTest
    def bigIntLit01(): BigInt = 0ii

    @CompileTest
    def bigIntLit02(): BigInt = 1ii

    @CompileTest
    def bigIntLit03(): BigInt = 10ii

    @CompileTest
    def bigIntLit04(): BigInt = 9223372036854775808ii

    @CompileTest
    def bigIntLit05(): BigInt = -9223372036854775809ii

    @CompileTest
    def bigIntLit06(): BigInt = 1_000ii

    @CompileTest
    def bigIntLit07(): BigInt = 42_000ii

    @CompileTest
    def bigIntLit08(): BigInt = 1_000_000ii

    @CompileTest
    def bigIntLit09(): BigInt = 42_000_000ii

    @CompileTest
    def bigIntLit10(): BigInt = 1_000_000_000ii

    @CompileTest
    def bigIntLit11(): BigInt = 42_000_000_000ii

    @CompileTest
    def bigIntLit12(): BigInt = 1_000_000_000_000_000_000ii

    @CompileTest
    def bigIntLit13(): BigInt = 42_000_000_000_000_000_000ii

    @CompileTest
    def bigIntLit14(): BigInt = 1_000_000_000_000_000_000_000ii

    @CompileTest
    def bigIntLit15(): BigInt = 42_000_000_000_000_000_000_000ii

    @CompileTest
    def bigIntLit16(): BigInt = 0x1ABCDEF9876543201ABCii

    @CompileTest
    def bigIntLit17(): BigInt = 0x1A_BC_DE_F9_87_65_43_20_1A_BCii

    @CompileTest
    def bigIntLit18(): BigInt = 0x0ii

    @CompileTest
    def bigIntLit19(): BigInt = 0x1ii

    @CompileTest
    def bigIntLit20(): BigInt = 0x1abcdef9876543201abcii

    @CompileTest
    def bigIntLit21(): BigInt = 0x1a_bc_de_f9_87_65_43_20_1a_bcii

}
