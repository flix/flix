namespace Test/Derives/Boxable {
    use Boxable.box;

    pub enum ImplicitEnum with Eq, Order, ToString {
        case EmptyCase
    }

    pub enum ExplicitEnum with Eq, Order, ToString, Boxable {
        case EmptyCase
    }

    @test
    def testBoxImplicit01(): Bool = box(ImplicitEnum.EmptyCase) == box(ImplicitEnum.EmptyCase)

    @test
    def testBoxExplicit01(): Bool = box(ExplicitEnum.EmptyCase) == box(ExplicitEnum.EmptyCase)
}
