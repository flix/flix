namespace Test/Exp/Fixpoint/Entails {

    @test
    def testFixpointEntails01(): Bool =
        #{ A(0, 0). } |= #{ A(0, 0). }

    @test
    def testFixpointEntails02(): Bool =
        #{ A(0, 1). } |= #{ A(0, 1). }

    @test
    def testFixpointEntails03(): Bool =
        #{ A(1, 1). } |= #{ A(1, 1). }

    @test
    def testFixpointEntails04(): Bool =
        not (#{ A(0, 0). } |= #{ A(0, 1). })

    @test
    def testFixpointEntails05(): Bool =
        not (#{ A(0, 0). } |= #{ A(1, 0). })

    @test
    def testFixpointEntails06(): Bool =
        not (#{ A(0, 0). } |= #{ A(1, 1). })

    @test
    def testFixpointEntails07(): Bool =
        #{ A(0, 0). A(0, 0). } |= #{ A(0, 0). }

    @test
    def testFixpointEntails08(): Bool =
        #{ A(0, 0). A(0, 0). A(0, 0). } |= A(0, 0).

    @test
    def testFixpointEntails09(): Bool =
        A(0, 0). |= #{ A(0, 0). A(0, 0). }

    @test
    def testFixpointEntails10(): Bool =
        A(0, 0). |= #{ A(0, 0). A(0, 0). A(0, 0). }

    @test
    def testFixpointEntails11(): Bool =
        #{ A(0, 0). B(0, 0). } |= A(0, 0).

    @test
    def testFixpointEntails12(): Bool =
        #{ B(0, 0). A(0, 0). } |= A(0, 0).

    @test
    def testFixpointEntails13(): Bool =
        #{ B(0, 0). A(0, 0). B(1, 1). } |= A(0, 0).

    @test
    def testFixpointEntails14(): Bool =
        #{ A(0, 0). B(0, 0). } |= #{ A(0, 0). B(0, 0). }

    @test
    def testFixpointEntails15(): Bool =
        #{ A(0, 0). B(0, 0). } |= #{ B(0, 0). A(0, 0). }

    @test
    def testFixpointEntails16(): Bool =
        let c1 = A(0, 0).;
        let c2 = B(x, y) :- A(x, y).;
        (solve (c1 <+> c2)) |= B(0, 0).

    @test
    def testFixpointEntails17(): Bool =
        let c1 = #{ A(0, 0). A(0, 1). };
        let c2 = B(x, y) :- A(x, y).;
        (solve (c1 <+> c2)) |= #{ B(0, 0). B(0, 1). }

    @test
    def testFixpointEntails18(): Bool =
        let c1 = #{ A(0, 0). A(0, 1). B(1, 1). };
        let c2 = B(x, y) :- A(x, y).;
        (solve (c1 <+> c2)) |= #{ B(0, 0). B(0, 1). B(1, 1). }

    @test
    def testFixpointEntails20(): Bool =
        let c1 = #{ E(0, 1). E(1, 2). };
        let c2 = P(x, y) :- E(x, y).;
        let c3 = P(x, z) :- P(x, y), E(y, z).;
        (solve (c1 <+> c2 <+> c3)) |= P(0, 1).

    @test
    def testFixpointEntails21(): Bool =
        let c1 = #{ E(0, 1). E(1, 2). };
        let c2 = P(x, y) :- E(x, y).;
        let c3 = P(x, z) :- P(x, y), E(y, z).;
        (solve (c1 <+> c2 <+> c3)) |= P(1, 2).

    @test
    def testFixpointEntails22(): Bool =
        let c1 = #{ E(0, 1). E(1, 2). };
        let c2 = P(x, y) :- E(x, y).;
        let c3 = P(x, z) :- P(x, y), E(y, z).;
        (solve (c1 <+> c2 <+> c3)) |= P(0, 2).

    @test
    def testFixpointEntails23(): Bool =
        let c1 = #{ E(0, 1). E(1, 2). E(2, 3). };
        let c2 = P(x, y) :- E(x, y).;
        let c3 = P(x, z) :- P(x, y), E(y, z).;
        (solve (c1 <+> c2 <+> c3)) |= #{ P(0, 2). P(1, 3). P(0, 3). }

    @test
    def testFixpointEntails24(): Bool =
        let c1 = #{ E(0, 1). E(1, 2). E(2, 3). E(3, 4). };
        let c2 = P(x, y) :- E(x, y).;
        let c3 = P(x, z) :- P(x, y), E(y, z).;
        (solve (c1 <+> c2 <+> c3)) |= (P(0, 4).)

    @test
    def testFixpointEntails25(): Bool =
        let c1 = #{ E(0, 1). E(1, 2). E(2, 3). E(3, 4). E(4, 0). };
        let c2 = P(x, y) :- E(x, y).;
        let c3 = P(x, z) :- P(x, y), E(y, z).;
        (solve (c1 <+> c2 <+> c3)) |= (P(2, 2).)

}
