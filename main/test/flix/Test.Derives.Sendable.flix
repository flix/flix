mod Test.Derives.Sendable {

    pub enum Enum1(Int32) with Sendable

    pub enum Enum2(Int32, Bool) with Sendable

    pub enum PolyEnum1[a](a) with Sendable

    pub enum PolyEnum2[a, b](a, b) with Sendable

    def requiresSendable(x: a): a with Sendable[a] = x

    @test
    def testSendable01(): Enum1 = requiresSendable(Enum1.Enum1(42))

    @test
    def testSendable02(): Enum2 = requiresSendable(Enum2.Enum2(42, true))

    @test
    def testSendable03(): PolyEnum1[Int32] = requiresSendable(PolyEnum1.PolyEnum1(42))

    @test
    def testSendable04(): PolyEnum1[Enum1] = requiresSendable(PolyEnum1.PolyEnum1(Enum1.Enum1(42)))

    @test
    def testSendable05(): PolyEnum2[Int32, Bool] = requiresSendable(PolyEnum2.PolyEnum2(42, true))
}
