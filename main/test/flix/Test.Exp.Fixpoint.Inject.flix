mod Test.Exp.Fixpoint.Inject {

    @test
    def inject01(): #{ Pred(Bool) | r } = {
        inject (Nil: List[Bool]) into Pred/1
    }

    @test
    def inject02(): #{ Pred(Char) | r } = {
        inject (Nil: List[Char]) into Pred/1
    }

    @test
    def inject03(): #{ Pred(Float32) | r } = {
        inject (Nil: List[Float32]) into Pred/1
    }

    @test
    def inject04(): #{ Pred(Float64) | r } = {
        inject (Nil: List[Float64]) into Pred/1
    }

    @test
    def inject05(): #{ Pred(BigDecimal) | r } = {
        inject (Nil: List[BigDecimal]) into Pred/1
    }

    @test
    def inject06(): #{ Pred(Int8) | r } = {
        inject (Nil: List[Int8]) into Pred/1
    }

    @test
    def inject07(): #{ Pred(Int16) | r } = {
        inject (Nil: List[Int16]) into Pred/1
    }

    @test
    def inject08(): #{ Pred(Int32) | r } = {
        inject (Nil: List[Int32]) into Pred/1
    }

    @test
    def inject09(): #{ Pred(Int64) | r } = {
        inject (Nil: List[Int64]) into Pred/1
    }

    @test
    def inject10(): #{ Pred(BigInt) | r } = {
        inject (Nil: List[BigInt]) into Pred/1
    }

    @test
    def inject11(): #{ Pred(String) | r } = {
        inject (Nil: List[String]) into Pred/1
    }

}
