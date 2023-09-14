mod Test.Exp.Fixpoint.Constraint {

    @Test
    def testUnitConstraint01(): #{A(Unit), R(Unit)} = #{
        A(()).
        R(x) :- A(x).
    }

    @Test
    def testBoolConstraint01(): #{A(Bool), R(Bool)} = #{
        A(true). A(false).
        R(x) :- A(x).
    }

    @Test
    def testCharConstraint01(): #{A(Char), R(Char)} = #{
        A('a'). A('z'). A(' ').
        R(x) :- A(x).
    }

    @Test
    def testFloat32Constraint01(): #{A(Float32), R(Float32)} = #{
        A(0.0f32). A(21.42f32). A(123.456f32).
        R(x) :- A(x).
    }

    @Test
    def testFloat64Constraint01(): #{A(Float64), R(Float64)} = #{
        A(0.0f64). A(21.42f64). A(123.456f64).
        R(x) :- A(x).
    }

    @Test
    def testBigDecimalConstraint01(): #{A(BigDecimal), R(BigDecimal)} = #{
        A(0.0ff). A(21.42ff). A(123.456ff).
        R(x) :- A(x).
    }

    @Test
    def testInt8Constraint01(): #{A(Int8), R(Int8)} = #{
        A(0i8). A(21i8). A(42i8).
        R(x) :- A(x).
    }

    @Test
    def testInt16Constraint01(): #{A(Int16), R(Int16)} = #{
        A(0i16). A(21i16). A(42i16).
        R(x) :- A(x).
    }

    @Test
    def testInt32Constraint01(): #{A(Int32), R(Int32)} = #{
        A(0i32). A(21i32). A(42i32).
        R(x) :- A(x).
    }

    @Test
    def testInt64Constraint01(): #{A(Int64), R(Int64)} = #{
        A(0i64). A(21i64). A(42i64).
        R(x) :- A(x).
    }

    @Test
    def testBigIntConstraint01(): #{A(BigInt), R(BigInt)} = #{
        A(0ii). A(21ii). A(42ii).
        R(x) :- A(x).
    }

    @Test
    def testStringConstraint01(): #{A(String), R(String)} = #{
        A("Hello"). A("World!"). A("Goodbye World!").
        R(x) :- A(x).
    }

    @Test
    def testOptionConstraint01(): #{A(Option[Int32]), R(Option[Int32])} = #{
        A(None). A(Some(21)). A(Some(42)).
        R(x) :- A(x).
    }

    @Test
    def testPredicateWithTypeVariableTerms01(): Bool = {
        let p = #{ A(12). };
        let query1 = query p select () from B();
        let test1 = query1 == Vector#{};
        let _test2 = solve p project C;
        let query2 = query (solve p project D) select () from D();
        let test3 = query2 == Vector#{};
        test1 and test3
    }

}
