namespace Test/Predicate/Nullary/False {

    @test
    def testFalsePredicate01(): #{ False() } =
        let x = False().;
        solve x

    @test
    def testFalsePredicate02(): #{ False() } =
        let x = False().;
        let u = x <+> x;
        solve u

    @test
    def testFalsePredicate03(): #{ False() } =
        let x = False().;
        let y = False().;
        let u = x <+> y;
        solve u

    @test
    def testFalsePredicate04(): #{ False() } =
        let x = False().;
        let y = False().;
        let u = x <+> y;
        let v = y <+> x;
        solve u <+> v

    @test
    def testFalsePredicate05(): #{ False() } =
        let x = False().;
        let y = False().;
        let _ = x <+> y;
        let v = y <+> x;
        let w = y <+> v;
        solve w

    @test
    def testFalsePredicate06(): #{ False() } =
        let x = False().;
        let y = False().;
        let u = solve x;
        let v = solve y;
        u <+> v

    @test
    def testFalsePredicate07(): #{ False() } =
        let x = False().;
        let y = False().;
        let u = solve (x <+> y);
        let v = solve (y <+> x);
        u <+> v

}
