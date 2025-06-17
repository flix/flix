mod Test.Exp.Fixpoint.Inject {

    @test
    def inject01(): #{ A(Bool) | r } = {
        inject (Nil: List[Bool]) into A/1
    }

    @test
    def inject02(): #{ A(Char) | r } = {
        inject (Nil: List[Char]) into A/1
    }

    @test
    def inject03(): #{ A(Float32) | r } = {
        inject (Nil: List[Float32]) into A/1
    }

    @test
    def inject04(): #{ A(Float64) | r } = {
        inject (Nil: List[Float64]) into A/1
    }

    @test
    def inject05(): #{ A(BigDecimal) | r } = {
        inject (Nil: List[BigDecimal]) into A/1
    }

    @test
    def inject06(): #{ A(Int8) | r } = {
        inject (Nil: List[Int8]) into A/1
    }

    @test
    def inject07(): #{ A(Int16) | r } = {
        inject (Nil: List[Int16]) into A/1
    }

    @test
    def inject08(): #{ A(Int32) | r } = {
        inject (Nil: List[Int32]) into A/1
    }

    @test
    def inject09(): #{ A(Int64) | r } = {
        inject (Nil: List[Int64]) into A/1
    }

    @test
    def inject10(): #{ A(BigInt) | r } = {
        inject (Nil: List[BigInt]) into A/1
    }

    @test
    def inject11(): #{ A(String) | r } = {
        inject (Nil: List[String]) into A/1
    }

    @test
    def inject12(): #{ A(Int32) } = {
        inject List#{1, 2, 3} into A/1
    }

    @test
    def inject13(): #{ A(Int32) | r } = {
        inject List#{1, 2, 3} into A/1
    }

}
