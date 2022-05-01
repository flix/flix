namespace Test/Exp/Fixpoint/Lambda {

    pub def testLambda01(): Bool & Impure = {
        let p1 = #(Input, Output) => #{
            Local(x) :- Input(x).
            Output(x) :- Local(x).
        };
        let p2 = #{
            Input(1). Input(2). Input(3).
        };
        query p1, p2 select x from Output(x) |> Array.memberOf(2)
    }

    pub def testLambda02(): Bool & Impure = {
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

    pub def testLambda03(): Bool & Impure = {
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

}
