namespace Test/Derivation {
    pub enum E[a] with Eq {
        case C1
        case C2(a)
    }

    @test
    def empty(): Bool = true
}