mod Test.Lit.BigDecimal {

    @CompileTest
    def bigDecimalLit01(): BigDecimal = 0.0ff

    @CompileTest
    def bigDecimalLit02(): BigDecimal = -0.0ff

    @CompileTest
    def bigDecimalLit03(): BigDecimal = 1.0ff

    @CompileTest
    def bigDecimalLit04(): BigDecimal = 2.0ff

    @CompileTest
    def bigDecimalLit05(): BigDecimal = 4.2ff

    @CompileTest
    def bigDecimalLit06(): BigDecimal = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @CompileTest
    def bigDecimalLit07(): BigDecimal = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

    @CompileTest
    def bigDecimalLit08(): BigDecimal = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0ff

    @CompileTest
    def bigDecimalLit09(): BigDecimal = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001ff

    @CompileTest
    def bigDecimalLit10(): BigDecimal = 1_0.0_0ff

    @CompileTest
    def bigDecimalLit11(): BigDecimal = 1_000.123_456_789ff

    @CompileTest
    def bigDecimalLit12(): BigDecimal = 42_000.0ff

    @CompileTest
    def bigDecimalLit13(): BigDecimal = 1_000_000_000_000_000_000_000_000_000.0ff

    @CompileTest
    def bigDecimalLit14(): BigDecimal = 0.000_000_000_000_000_000_000_000_000_1ff

    @CompileTest
    def bigDecimalLit15(): BigDecimal = 1ff

    @CompileTest
    def bigDecimalLit16(): BigDecimal = 1e23ff

    @CompileTest
    def bigDecimalLit17(): BigDecimal = 1.23e45ff

}
