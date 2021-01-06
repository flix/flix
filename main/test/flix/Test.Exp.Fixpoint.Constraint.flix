namespace Test/Exp/Fixpoint/Constraint {

    @test
    def testUnitConstraint01(): #{A(Unit), R(Unit)} = #{
        A(()).
        R(x) :- A(x).
    }

    @test
    def testBoolConstraint01(): #{A(Bool), R(Bool)} = #{
        A(true). A(false).
        R(x) :- A(x).
    }

    @test
    def testCharConstraint01(): #{A(Char), R(Char)} = #{
        A('a'). A('z'). A(' ').
        R(x) :- A(x).
    }

    @test
    def testFloat32Constraint01(): #{A(Float32), R(Float32)} = #{
        A(0.0f32). A(21.42f32). A(123.456f32).
        R(x) :- A(x).
    }

    @test
    def testFloat64Constraint01(): #{A(Float64), R(Float64)} = #{
        A(0.0f64). A(21.42f64). A(123.456f64).
        R(x) :- A(x).
    }

    @test
    def testInt8Constraint01(): #{A(Int8), R(Int8)} = #{
        A(0i8). A(21i8). A(42i8).
        R(x) :- A(x).
    }

    @test
    def testInt16Constraint01(): #{A(Int16), R(Int16)} = #{
        A(0i16). A(21i16). A(42i16).
        R(x) :- A(x).
    }

    @test
    def testInt32Constraint01(): #{A(Int32), R(Int32)} = #{
        A(0i32). A(21i32). A(42i32).
        R(x) :- A(x).
    }

    @test
    def testInt64Constraint01(): #{A(Int64), R(Int64)} = #{
        A(0i64). A(21i64). A(42i64).
        R(x) :- A(x).
    }

    @test
    def testBigIntConstraint01(): #{A(BigInt), R(BigInt)} = #{
        A(0ii). A(21ii). A(42ii).
        R(x) :- A(x).
    }

    @test
    def testStringConstraint01(): #{A(String), R(String)} = #{
        A("Hello"). A("World!"). A("Goodbye World!").
        R(x) :- A(x).
    }

    @test
    def testOptionConstraint01(): #{A(Option[Int]), R(Option[Int])} = #{
        A(None). A(Some(21)). A(Some(42)).
        R(x) :- A(x).
    }

    enum Option[t] {
        case None,
        case Some(t)
    }

    instance Eq[Option[a]] with [a : Eq] {
        def eq(o1: Option[a], o2: Option[a]): Bool = match (o1, o2) {
            case (None, None)           => true
            case (Some(v1), Some(v2))   => v1 == v2
            case _                      => false
        }

        def neq(xs: Option[a], ys: Option[a]): Bool = not (xs == ys)
    }

    instance Hash[Option[a]] with [a : Hash] {
        def hash(o: Option[a]): Int = match o {
            case None       => 1046527
            case Some(x)    => 919 * Hash.hash(x)
        }
    }

    instance ToString[Option[a]] with [a : ToString] {
        def toString(o: Option[a]): String = match o {
            case None       => "None"
            case Some(x)    => "Some(${x})"
        }
    }

}
