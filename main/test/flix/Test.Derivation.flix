namespace Test/Derivation {
    pub enum E1[a] with Eq {
        case C1
        case C2(a)
    }

    pub enum E2 with Eq, ToString

    pub enum E3 with Eq, Order, ToString

    @test
    def empty(): Bool = true
}