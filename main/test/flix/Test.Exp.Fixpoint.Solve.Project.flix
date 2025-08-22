mod Test.Exp.Fixpoint.Project {

    /////////////////////////////////////////////////////////////////////////////
    // Tests with single predicate projection                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testProjectSingleInt01(): Bool = {
        let p = #{
            A(1). A(2). A(3).
            B(10). B(20).
            C(100).
        };
        let q = solve p project A;
        let expected = Vector#{1, 2, 3};
        let actual = query q select x from A(x);
        expected `Assert.eq` actual
    }

    @Test
    def testProjectSingleInt02(): Bool = {
        let p = #{
            A(42).
            B(1). B(2). B(3).
            C(100). C(200).
        };
        let q = solve p project B;
        let expected = Vector#{1, 2, 3};
        let actual = query q select x from B(x);
        expected `Assert.eq` actual
    }

    @Test
    def testProjectSingleString01(): Bool = {
        let p = #{
            A("a"). A("b").
            B("x"). B("y"). B("z").
            C("m").
        };
        let q = solve p project A;
        let expected = Vector#{"a", "b"};
        let actual = query q select x from A(x);
        expected `Assert.eq` actual
    }

    @Test
    def testProjectSingleString02(): Bool = {
        let p = #{
            A("p").
            B("q"). B("r"). B("s").
            C("t").
        };
        let q = solve p project B;
        let expected = Vector#{"q", "r", "s"};
        let actual = query q select x from B(x);
        expected `Assert.eq` actual
    }

    @Test
    def testProjectSingleBool01(): Bool = {
        let p = #{
            A(true). A(false).
            B(true).
            C(false).
        };
        let q = solve p project A;
        let expected = Vector#{false, true};
        let actual = query q select x from A(x);
        expected `Assert.eq` actual
    }

    @Test
    def testProjectSingleChar01(): Bool = {
        let p = #{
            A('a'). A('b'). A('c').
            B('x'). B('y').
            C('z').
        };
        let q = solve p project A;
        let expected = Vector#{'a', 'b', 'c'};
        let actual = query q select x from A(x);
        expected `Assert.eq` actual
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with double predicate projection                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testProjectDoubleInt01(): Bool = {
        let p = #{
            A(1). A(2).
            B(10). B(20).
            C(100). C(200). C(300).
        };
        let q = solve p project A, B;
        let expectedA = Vector#{1, 2};
        let expectedB = Vector#{10, 20};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB
    }

    @Test
    def testProjectDoubleInt02(): Bool = {
        let p = #{
            A(5).
            B(15). B(25). B(35).
            C(50). C(60).
        };
        let q = solve p project B, C;
        let expectedB = Vector#{15, 25, 35};
        let expectedC = Vector#{50, 60};
        let actualB = query q select x from B(x);
        let actualC = query q select x from C(x);
        expectedB `Assert.eq` actualB and expectedC `Assert.eq` actualC
    }

    @Test
    def testProjectDoubleString01(): Bool = {
        let p = #{
            A("a"). A("b").
            B("x"). B("y").
            C("p"). C("q"). C("r").
        };
        let q = solve p project A, C;
        let expectedA = Vector#{"a", "b"};
        let expectedC = Vector#{"p", "q", "r"};
        let actualA = query q select x from A(x);
        let actualC = query q select x from C(x);
        expectedA `Assert.eq` actualA and expectedC `Assert.eq` actualC
    }

    @Test
    def testProjectDoubleMixed01(): Bool = {
        let p = #{
            A(42). A(99).
            B("x"). B("y"). B("z").
            C(true). C(false).
        };
        let q = solve p project A, B;
        let expectedA = Vector#{42, 99};
        let expectedB = Vector#{"x", "y", "z"};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB
    }

    @Test
    def testProjectDoubleMixed02(): Bool = {
        let p = #{
            A('x'). A('y'). A('z').
            B(7). B(8).
            C("m"). C("n").
        };
        let q = solve p project A, B;
        let expectedA = Vector#{'x', 'y', 'z'};
        let expectedB = Vector#{7, 8};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with triple predicate projection                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testProjectTripleInt01(): Bool = {
        let p = #{
            A(1). A(2).
            B(10). B(20).
            C(100). C(200).
            D(1000). D(2000). D(3000).
        };
        let q = solve p project A, B, C;
        let expectedA = Vector#{1, 2};
        let expectedB = Vector#{10, 20};
        let expectedC = Vector#{100, 200};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        let actualC = query q select x from C(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB and expectedC `Assert.eq` actualC
    }

    @Test
    def testProjectTripleInt02(): Bool = {
        let p = #{
            A(3). A(6). A(9).
            B(12). B(15).
            C(18).
            D(21). D(24). D(27). D(30).
        };
        let q = solve p project A, B, D;
        let expectedA = Vector#{3, 6, 9};
        let expectedB = Vector#{12, 15};
        let expectedD = Vector#{21, 24, 27, 30};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        let actualD = query q select x from D(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB and expectedD `Assert.eq` actualD
    }

    @Test
    def testProjectTripleString01(): Bool = {
        let p = #{
            A("a"). A("b").
            B("x"). B("y"). B("z").
            C("m").
            D("p"). D("q").
        };
        let q = solve p project A, B, C;
        let expectedA = Vector#{"a", "b"};
        let expectedB = Vector#{"x", "y", "z"};
        let expectedC = Vector#{"m"};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        let actualC = query q select x from C(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB and expectedC `Assert.eq` actualC
    }

    @Test
    def testProjectTripleMixed01(): Bool = {
        let p = #{
            A(100). A(200). A(300).
            B("a"). B("b").
            C(true). C(false).
            D('x'). D('y'). D('z'). D('w').
        };
        let q = solve p project A, B, C;
        let expectedA = Vector#{100, 200, 300};
        let expectedB = Vector#{"a", "b"};
        let expectedC = Vector#{false, true};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        let actualC = query q select x from C(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB and expectedC `Assert.eq` actualC
    }

    @Test
    def testProjectTripleMixed02(): Bool = {
        let p = #{
            A('p'). A('q').
            B(777). B(888). B(999).
            C("x"). C("y"). C("z").
            D(false).
        };
        let q = solve p project A, B, C;
        let expectedA = Vector#{'p', 'q'};
        let expectedB = Vector#{777, 888, 999};
        let expectedC = Vector#{"x", "y", "z"};
        let actualA = query q select x from A(x);
        let actualB = query q select x from B(x);
        let actualC = query q select x from C(x);
        expectedA `Assert.eq` actualA and expectedB `Assert.eq` actualB and expectedC `Assert.eq` actualC
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with derived predicates                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testProjectSingleWithRules01(): Bool = {
        let p = #{
            A(1). A(2). A(3).
            P(x) :- A(x).
            B(10). B(20).
        };
        let q = solve p project P;
        let expected = Vector#{1, 2, 3};
        let actual = query q select x from P(x);
        expected `Assert.eq` actual
    }

    @Test
    def testProjectSingleWithRules02(): Bool = {
        let p = #{
            A(1). A(2). A(3).
            P(x) :- A(x).
            Q(x) :- P(x).
            B(10). B(20).
        };
        let q = solve p project Q;
        let expected = Vector#{1, 2, 3};
        let actual = query q select x from Q(x);
        expected `Assert.eq` actual
    }

    @Test
    def testProjectDoubleWithRules01(): Bool = {
        let p = #{
            A(1). A(2).
            P(x) :- A(x).
            Q(y) :- P(y).
            B(100).
        };
        let q = solve p project P, Q;
        let expectedP = Vector#{1, 2};
        let expectedQ = Vector#{1, 2};
        let actualP = query q select x from P(x);
        let actualQ = query q select x from Q(x);
        expectedP `Assert.eq` actualP and expectedQ `Assert.eq` actualQ
    }

    @Test
    def testProjectDoubleWithRules02(): Bool = {
        let p = #{
            A(1). A(2).
            P(x) :- A(x).
            B(3). B(4).
            Q(x) :- B(x).
        };
        let q = solve p project P, Q;
        let expectedP = Vector#{1, 2};
        let expectedQ = Vector#{3, 4};
        let actualP = query q select x from P(x);
        let actualQ = query q select x from Q(x);
        expectedP `Assert.eq` actualP and expectedQ `Assert.eq` actualQ
    }

    @Test
    def testProjectTripleWithRules01(): Bool = {
        let p = #{
            A(1). A(2).
            P(x) :- A(x).
            Q(x) :- P(x).
            R(x) :- Q(x).
        };
        let q = solve p project P, Q, R;
        let expectedP = Vector#{1, 2};
        let expectedQ = Vector#{1, 2};
        let expectedR = Vector#{1, 2};
        let actualP = query q select x from P(x);
        let actualQ = query q select x from Q(x);
        let actualR = query q select x from R(x);
        expectedP `Assert.eq` actualP and expectedQ `Assert.eq` actualQ and expectedR `Assert.eq` actualR
    }

    @Test
    def testProjectEmptyResult01(): Bool = {
        let p = #{
            A(1). A(2).
            B("a").
        };
        let q = solve p project C;
        let expected = (Vector.empty(): Vector[Int32]);
        let actual = query q select x from C(x);
        expected `Assert.eq` actual
    }

}
