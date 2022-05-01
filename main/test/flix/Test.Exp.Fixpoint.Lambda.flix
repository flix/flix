
namespace Test/Exp/Fixpoint/Lambda {

    pub def testLambdaCopy01(): Bool & Impure = {
        let p1 = #(Input, Output) => #{
            Local(x) :- Input(x).
            Output(x) :- Local(x).
        };
        let p2 = #{
            Input(1). Input(2). Input(3).
        };
        query p1, p2 select x from Output(x) |> Array.memberOf(2)
    }

    pub def testLambdaCopy02(): Bool & Impure = {
        let p1 = #(Input, Output) => #{
            Local1(x) :- Input(x).
            Local2(x) :- Local1(x).
            Output(x) :- Local2(x).
        };
        let p2 = #{
            Input(1). Input(2). Input(3).
        };
        query p1, p2 select x from Output(x) |> Array.memberOf(2)
    }

    pub def testLambdaMultipleLocals01(): Bool & Impure = {
        let p1 = #(Input, Output) => #{
            Local("abc").
            Local("def").
            Output(x) :- Input(x).
        };
        let p2 = #{
            Input(1). Input(2). Input(3).
            Local(1). Local(2). Local(3).
        };
        query p1, p2 select x from Local(x) |> Array.memberOf(2)
    }

    pub def testLambdaMultipleLocals02(): Bool & Impure = {
        let p1 = #(A) => #{
            Local("abc").
            A(42) :- Local("abc").
        };
        let p2 = #(B) => #{
            Local(123).
            B(42) :- Local(123).
        };
        let p3 = #{
            R(x) :- A(x), B(x).
        };
        query p1, p2, p3 select x from R(x) |> Array.memberOf(42)
    }

    pub def testLambdaLambda01(): Bool & Impure = {
        let p1 = #{
            A(1). B(1). C(1).
        };
        let p2 = #(A, B, C) => p1;
        let p3 = #(A, B) => p2;
        let p4 = #(A) => p3;
        query p4 select x from A(x) |> Array.memberOf(1)
    }

    pub def testLambdaLambda02(): Bool & Impure = {
        let p1 = #{
            A(1). B(1). C(1).
        };
        let p2 = #(A, B, C) => p1;
        let p3 = #(A, B) => p2;
        let p4 = #(A) => p3;
        not (query p4 select x from B(x) |> Array.memberOf(1))
    }

    pub def testLambdaLambda03(): Bool & Impure = {
        let p1 = #{
            A(1). B(1). C(1).
        };
        let p2 = #(A, B, C) => p1;
        let p3 = #(A, B) => p2;
        let p4 = #(A) => p3;
        not (query p4 select x from C(x) |> Array.memberOf(1))
    }

    pub def testLambdaLambda04(): Bool & Impure = {
        let p1 = #{
            A(1). B(1). C(1).
            R(42) :- A(x), B(x), C(x).
        };
        let p2 = #(R, A, B) => p1;
        let p3 = #(R, A) => p2;
        query p3 select x from R(x) |> Array.memberOf(42)
    }

}
