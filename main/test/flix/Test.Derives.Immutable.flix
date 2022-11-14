namespace Test/Derives/Immutable {
    
    pub enum Enum1(Int32) with Immutable

    pub enum Enum2(Int32, Bool) with Immutable

    pub enum PolyEnum1[a](a) with Immutable

    pub enum PolyEnum2[a, b](a, b) with Immutable

    def requiresImmutable(x: a): a with Immutable[a] = x

    @test
    def testImmutable01(): Enum1 = requiresImmutable(Enum1(42))

    @test
    def testImmutable02(): Enum2 = requiresImmutable(Enum2(42, true))

    @test 
    def testImmutable03(): PolyEnum1[Int32] = requiresImmutable(PolyEnum1(42))

    @test 
    def testImmutable04(): PolyEnum1[Enum1] = requiresImmutable(PolyEnum1(Enum1(42)))

    @test 
    def testImmutable05(): PolyEnum2[Int32, Bool] = requiresImmutable(PolyEnum2(42, true))
}
