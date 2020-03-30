namespace Test/Term/Lit/Var/QuantVar {

    @test
    def testQuantVar01(): #{ A(Int) } =
        let c = A(x) :- A(x).;
        solve c

    @test
    def testQuantVar02(): #{ B(Int, String) } =
        let c = B(x, y) :- B(x, y).;
        solve c

    @test
    def testQuantVar03(): #{ C(Int, String, Bool) } =
        let c = C(x, y, z) :- C(x, y, z).;
        solve c

    @test
    def testQuantVar04(): #{ A(Int), B(Int, String), C(Int, String, Bool) } =
        let c1 = A(x) :- A(x).;
        let c2 = B(x, y) :- B(x, y).;
        let c3 = C(x, y, z) :- C(x, y, z).;
        solve (c1 <+> c2 <+> c3)

    @test
    def testQuantVar05(): #{ A(Int), B(Int, String) } =
        let c = A(x) :- B(x, y).;
        solve c

    @test
    def testQuantVar06(): #{ A(Int), C(Int, String, Bool) } =
        let c = A(x) :- C(x, y, z).;
        solve c
    @test
    def testQuantVar07(): #{ B(Int, String), C(Int, String, Bool) } =
        let c = B(x, y) :- C(x, y, z).;
        solve c

    @test
    def testQuantVar08(): #{ A(Int), B(Int, String), C(Int, String, Bool) } =
        let c = A(x) :- A(x), B(x, y), C(x, y, z).;
        solve c

    @test
    def testQuantVar09(): #{ A(Int), B(Int, String), C(Int, String, Bool) } =
        let c = C(x, y, z) :- A(x), B(x, y), C(x, y, z).;
        solve c

}
