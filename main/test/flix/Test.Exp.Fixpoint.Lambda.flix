mod Test.Exp.Fixpoint.Lambda {

    pub def testLambdaCopy01(): Bool = {
        let p1 = #(Input, Output) -> #{
            Local(x) :- Input(x).
            Output(x) :- Local(x).
        };
        let p2 = #{
            Input(1). Input(2). Input(3).
        };
        query p1, p2 select x from Output(x) |> List.memberOf(2)
    }

    pub def testLambdaCopy02(): Bool = {
        let p1 = #(Input, Output) -> #{
            Local1(x) :- Input(x).
            Local2(x) :- Local1(x).
            Output(x) :- Local2(x).
        };
        let p2 = #{
            Input(1). Input(2). Input(3).
        };
        query p1, p2 select x from Output(x) |> List.memberOf(2)
    }

    pub def testLambdaMultipleLocals01(): Bool = {
        let p1 = #(Input, Output) -> #{
            Local("abc").
            Local("def").
            Output(x) :- Input(x).
        };
        let p2 = #{
            Input(1). Input(2). Input(3).
            Local(1). Local(2). Local(3).
        };
        query p1, p2 select x from Local(x) |> List.memberOf(2)
    }

    pub def testLambdaMultipleLocals02(): Bool = {
        let p1 = #(A) -> #{
            Local("abc").
            A(42) :- Local("abc").
        };
        let p2 = #(B) -> #{
            Local(123).
            B(42) :- Local(123).
        };
        let p3 = #{
            R(x) :- A(x), B(x).
        };
        query p1, p2, p3 select x from R(x) |> List.memberOf(42)
    }

    pub def testLambdaLambda01(): Bool = {
        let p1 = #{
            A(1). B(1). C(1).
        };
        let p2 = #(A, B, C) -> p1;
        let p3 = #(A, B) -> p2;
        let p4 = #(A) -> p3;
        query p4 select x from A(x) |> List.memberOf(1)
    }

    pub def testLambdaLambda02(): Bool = {
        let p1 = #{
            A(1). B(1). C(1).
        };
        let p2 = #(A, B, C) -> p1;
        let p3 = #(A, B) -> p2;
        let p4 = #(A) -> p3;
        not (query p4 select x from B(x) |> List.memberOf(1))
    }

    pub def testLambdaLambda03(): Bool = {
        let p1 = #{
            A(1). B(1). C(1).
        };
        let p2 = #(A, B, C) -> p1;
        let p3 = #(A, B) -> p2;
        let p4 = #(A) -> p3;
        not (query p4 select x from C(x) |> List.memberOf(1))
    }

    pub def testLambdaLambda04(): Bool = {
        let p1 = #{
            A(1). B(1). C(1).
            R(42) :- A(x), B(x), C(x).
        };
        let p2 = #(R, A, B) -> p1;
        let p3 = #(R, A) -> p2;
        query p3 select x from R(x) |> List.memberOf(42)
    }

    pub def testStratification01(): Bool =
        let p1 = #(A) -> #{
            Q(1).
            A(x) :- Q(x), not B(x).
        };
        let p2 = #(B) -> #{
            Q(2).
            B(x) :- Q(x), not A(x).
        };
        not (query p1, p2 select (x, y) from A(x), B(y) |> List.isEmpty)

    pub def testPolymorphism01(): Bool =
        query poly(123), poly("abc") select x from R(x) |> List.memberOf("OK")

    pub def testPolymorphism02(): Bool =
        query poly(123), poly(456) select x from R(x) |> List.memberOf("OK")

    pub def testPolymorphism03(): Bool =
        query poly(123), poly("abc") select x from R(x) |> List.memberOf("OK")

    pub def testPolymorphism04(): Bool =
        query poly(123), poly("abc"), poly(true) select x from R(x) |> List.memberOf("OK")

    def poly(x: a): #{ R(String) } with Order[a] =
        #(R) -> #{
            A(x).
            R("OK") :- A(x).
        }

    pub def testTypedLambda01(): Bool =
        let p = #(A(Int32)) -> #{
            A(123).
        };
        not (query p select x from A(x) |> List.isEmpty)

    pub def testTypedLambda02(): Bool =
        let p = #(A(Int32, String)) -> #{
            A(123, "Hello World").
        };
        not (query p select x from A(x) |> List.isEmpty)

    pub def testTypedLambda03(): Bool =
        let p = #(A(Int32, Int32), B(String, String)) -> #{
            A(123, 456).
            B("hello", "World").
        };
        not (query p select x from A(x) |> List.isEmpty)

    pub def testTypedLambda04(): Bool =
        let p = #(L(String; Int32)) -> #{
            L("a"; 1).
            L("a"; 2).
        };
        not (query p select (x, y) from L(x; y) |> List.isEmpty)

}
