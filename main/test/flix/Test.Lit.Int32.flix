mod Test.Lit.Int32 {

    @CompileTest
    def int32Lit01(): Int32 = -254542i32

    @CompileTest
    def int32Lit02(): Int32 = 45649878i32

    @CompileTest
    def int32Lit03(): Int32 = -32769i32

    @CompileTest
    def int32Lit04(): Int32 = 32768i32

    @CompileTest
    def int32Lit05(): Int32 = 2147483647i32

    @CompileTest
    def int32Lit06(): Int32 = -2147483648i32

    @CompileTest
    def int32Lit07(): Int32 = 1_0i32

    @CompileTest
    def int32Lit08(): Int32 = 1_000i32

    @CompileTest
    def int32Lit09(): Int32 = 42_000i32

    @CompileTest
    def int32Lit10(): Int32 = 1_000_000i32

    @CompileTest
    def int32Lit11(): Int32 = 42_000_000i32

    @CompileTest
    def int32Lit12(): Int32 = 1_000_000_000i32

    @CompileTest
    def int32Lit13(): Int32 = 0x1ABCDEF9i32

    @CompileTest
    def int32Lit14(): Int32 = 0x1A_BC_DE_F9i32

    @CompileTest
    def int32Lit15(): Int32 = 0x0i32

    @CompileTest
    def int32Lit16(): Int32 = 0x1i32

    @CompileTest
    def int32Lit17(): Int32 = 0x1abcdef9i32

    @CompileTest
    def int32Lit18(): Int32 = 0x1a_bc_de_f9i32

}
