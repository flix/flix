namespace Test/Derives/Immutable {
    
    pub enum Enum(Int32, Bool) with Immutable

    pub enum PolyEnum[a, b](a, b) with Immutable

    def requiresImmutable(x: a): a with Immutable[a] = x

    @test
    def testImmutable01(): Enum = requiresImmutable(Enum(42, true))

    @test 
    def testImmutable02(): PolyEnum[Int32, Bool] = requiresImmutable(PolyEnum(42, true))
}
