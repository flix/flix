namespace Test/Exp/Match/Array {

    @test
    def testMatchEmptyUnitArray01(): Bool = region r {
        match (([] @ r): ScopedArray[Unit, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyBoolArray01(): Bool = region r {
        match (([] @ r): ScopedArray[Bool, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyCharArray01(): Bool = region r {
        match (([] @ r): ScopedArray[Char, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyFloat32Array01(): Bool = region r {
        match (([] @ r): ScopedArray[Float32, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyFloat64Array01(): Bool = region r {
        match (([] @ r): ScopedArray[Float64, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyInt8Array01(): Bool = region r {
        match (([] @ r): ScopedArray[Int8, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyInt16Array01(): Bool = region r {
        match (([] @ r): ScopedArray[Int16, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyInt64Array01(): Bool = region r {
        match (([] @ r): ScopedArray[Int64, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyStringArray01(): Bool = region r {
        match (([] @ r): ScopedArray[String, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyBigIntArray01(): Bool = region r {
        match (([] @ r): ScopedArray[BigInt, _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyOptionIntArray01(): Bool = region r {
        match (([] @ r): ScopedArray[Option[Int32], _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchEmptyResultArray01(): Bool = region r {
        match (([] @ r): ScopedArray[Result[Int32, String], _]) {
            case [] => true
            case _  => false
        }
    }

    @test
    def testMatchUnitArray01(): Bool = region r {
        match [()] @ r {
            case [()] => true
            case _    => false
        }
    }

    @test
    def testMatchUnitArray02(): Bool = region r {
        match [(), ()] @ r {
            case [(), ()] => true
            case _        => false
        }
    }

    @test
    def testMatchUnitArray03(): Bool = region r {
        match [[()], [()]] @ r {
            case [[()], [()]] => true
            case _            => false
        }
    }

    @test
    def testMatchInt8Array1(): Bool = region r {
        match [1i8] @ r {
            case [1i8] => true
            case _     => false
        }
    }

    @test
    def testMatchInt8Array2(): Bool = region r {
        match [1i8] @ r {
            case [2i8] => false
            case _     => true
        }
    }

    @test
    def testMatchInt8Array3(): Bool = region r {
        match [1i8, 2i8] @ r {
            case [1i8, 2i8] => true
            case _          => false
        }
    }

    @test
    def testMatchInt8Array4(): Bool = region r {
        match [[1i8] @ r, [2i8] @ r] @ r {
            case [[1i8], [2i8]] => true
            case _              => false
        }
    }

    @test
    def testMatchInt16Array1(): Bool = region r {
        match [1i16] @ r {
            case [1i16] => true
            case _      => false
        }
    }

    @test
    def testMatchInt16Array2(): Bool = region r {
        match [1i16] @ r {
            case [2i16] => false
            case _      => true
        }
    }

    @test
    def testMatchInt16Array3(): Bool = region r {
        match [1i16, 2i16] @ r {
            case [1i16, 2i16] => true
            case _            => false
        }
    }

    @test
    def testMatchInt16Array4(): Bool = region r {
        match [[1i16] @ r, [2i16] @ r] @ r {
            case [[1i16],[2i16]] => true
            case _               => false
        }
    }

    @test
    def testMatchInt32Array1(): Bool = region r {
        match [1] @ r {
            case [1] => true
            case _   => false
        }
    }

    @test
    def testMatchInt32Array2(): Bool = region r {
        match [1, 2] @ r {
            case [1] => false
            case _   => true
        }
    }

    @test
    def testMatchInt32Array3(): Bool = region r {
        match [1, 2] @ r {
            case [1, 2] => true
            case _      => false
        }
    }

    @test
    def testMatchInt32Array4(): Bool = region r {
        match [1, 2] @ r {
            case [1, _] => true
            case _      => false
        }
    }

    @test
    def testMatchInt32Array5(): Bool = region r {
        match [2, 1] @ r {
            case [1, 2] => false
            case _      => true
        }
    }

    @test
    def testMatchInt32Array6(): Bool = region r {
        match [[1] @ r, [2] @ r] @ r {
            case [[1], [2]] => true
            case _          => false
        }
    }

    @test
    def testMatchInt32Array7(): Bool = region r {
        match [[1] @ r, [2] @ r] @ r {
            case [[1]] => false
            case _     => true
        }
    }

    @test
    def testMatchInt32Array8(): Bool = region r {
        match [[1,2,3] @ r, [1,2,3] @ r] @ r {
            case [[1,2,3], [1,2,3]] => true
            case _                  => false
        }
    }

    @test
    def testMatchInt32Array9(): Bool = region r {
        match [[[1] @ r] @ r, [[2] @ r] @ r] @ r {
            case [[[1]], [[2]]] => true
            case _              => false
        }
    }

    @test
    def testMatchInt64Array1(): Bool = region r {
        match [1i64] @ r {
            case [1i64] => true
            case _      => false
        }
    }

    @test
    def testMatchInt64Array2(): Bool = region r {
        match [1i64] @ r {
            case [2i64] => false
            case _      => true
        }
    }

    @test
    def testMatchInt64Array3(): Bool = region r {
        match [1i64, 2i64] @ r {
            case [1i64, 2i64] => true
            case _            => false
        }
    }

    @test
    def testMatchInt64Array4(): Bool = region r {
        match [[1i64] @ r, [2i64] @ r] @ r {
            case [[1i64], [2i64]] => true
            case _                => false
        }
    }

    @test
    def testMatchBigIntArray1(): Bool = region r {
        match [1i8] @ r {
            case [1i8] => true
            case _     => false
        }
    }

    @test
    def testMatchBigIntArray2(): Bool = region r {
        match [1ii] @ r {
            case [2ii] => false
            case _     => true
        }
    }

    @test
    def testMatchBigIntArray3(): Bool = region r {
        match [1ii, 2ii] @ r {
            case [1ii, 2ii] => true
            case _          => false
        }
    }

    @test
    def testMatchBigIntArray4(): Bool = region r {
        match [[1ii] @ r, [2ii] @ r] @ r {
            case [[1ii],[2ii]] => true
            case _             => false
        }
    }

    @test
    def testMatchStrArray1(): Bool = region r {
        match ["one"] @ r {
            case ["one"] => true
            case _       => false
        }
    }

    @test
    def testMatchStrArray2(): Bool = region r {
        match ["one"] @ r {
            case ["one "] => false
            case _        => true
        }
    }

    @test
    def testMatchStrArray3(): Bool = region r {
        match ["one","two"] @ r {
            case ["one", "two"] => true
            case _              => false
        }
    }

    @test
    def testMatchStrArray4(): Bool = region r {
        match [["one"] @ r, ["two"] @ r] @ r {
            case [["one"], ["two"]] => true
            case _                  => false
        }
    }

    @test
    def testMatchBoolArray1(): Bool = region r {
        match [true] @ r {
            case [true] => true
            case _      => false
        }
    }

    @test
    def testMatchBoolArray2(): Bool = region r {
        match [true] @ r {
            case [false] => false
            case _       => true
        }
    }

    @test
    def testMatchBoolArray3(): Bool = region r {
        match [true, false] @ r {
            case [true, false] => true
            case _             => false
        }
    }

    @test
    def testMatchBoolArray4(): Bool = region r {
        match [[false] @ r,[false] @ r] @ r {
            case [[false], [false]] => true
            case _                  => false
        }
    }

    @test
    def testMatchTupleArray1(): Bool = region r {
        match [(1, "one")] @ r {
            case [(1, "one")] => true
            case _            => false
        }
    }

    @test
    def testMatchTupleArray2(): Bool = region r {
        match [(1, "one")] @ r {
            case [(1, "one ")] => false // Note the whitespace in the string in the pattern
            case _             => true
        }
    }

    @test
    def testMatchTupleArray3(): Bool = region r {
        match [(1, "one")] @ r {
            case [(2, "one")] => false
            case _            => true
        }
    }

    @test
    def testMatchTupleArray4(): Bool = region r {
        match [(1, "one"), (2, "two")] @ r {
            case [(1, "one"), (2, "two")] => true
            case _                        => false
        }
    }

    @test
    def testMatchTupleArray5(): Bool = region r {
        match [[(1, "one")] @ r, [(2, "two")] @ r] @ r {
            case [[(1, "one")], [(2, "two")]] => true
            case _                            => false
        }
    }

    @test
    def testMatchFloatArray1(): Bool = region r {
        match [1.1] @ r {
            case [1.1] => true
            case _     => false
        }
    }

    @test
    def testMatchFloatArray2(): Bool = region r {
        match [1.1] @ r {
            case [1.2] => false
            case _     => true
        }
    }

    @test
    def testMatchFloatArray3(): Bool = region r {
        match [1.1, 2.2] @ r {
            case [1.1, 2.2] => true
            case _         => false
        }
    }

    @test
    def testMatchFloatArray4(): Bool = region r {
        match [[1.1] @ r, [2.2] @ r] @ r {
            case [[1.1], [2.2]] => true
            case _              => false
        }
    }

    @test
    def testMatchFloat32Array1(): Bool = region r {
        match ([1.1f32] @ r) {
            case [1.1f32] => true
            case _        => false
        }
    }

    @test
    def testMatchFloat32Array2(): Bool = region r {
        match [1.1f32] @ r {
            case [1.0f32] => false
            case _        => true
        }
    }

    @test
    def testMatchFloat32Array3(): Bool = region r {
        match [1.1f32, 2.2f32] @ r {
            case [1.1f32, 2.2f32] => true
            case _                => false
        }
    }

    @test
    def testMatchFloat32Array4(): Bool = region r {
        match [[1.1f32] @ r, [2.2f32] @ r] @ r {
            case [[1.1f32], [2.2f32]] => true
            case _                    => false
        }
    }

    @test
    def testMatchCharArray1(): Bool = region r {
        match ['1'] @ r {
            case ['1'] => true
            case _     => false
        }
    }

    @test
    def testMatchCharArray2(): Bool = region r {
        match ['1'] @ r {
            case ['2'] => false
            case _     => true
        }
    }

    @test
    def testMatchCharArray3(): Bool = region r {
        match ['1', '2'] @ r {
            case ['1', '2'] => true
            case _          => false
        }
    }

    @test
    def testMatchCharArray4(): Bool = region r {
        match [['1'] @ r, ['2'] @ r] @ r {
            case [['1'], ['2']] => true
            case _              => false
        }
    }

    @test
    def testMatchOptionArray1(): Bool = region r {
        match [Some(1)] @ r {
            case [Some(1)] => true
            case _         => false
        }
    }

    @test
    def testMatchOptionArray2(): Bool = region r {
        match [Some(1)] @ r {
            case [None] => false
            case _      => true
        }
    }

    @test
    def testMatchOptionArray3(): Bool = region r {
        match [Some(1), Some(2)] @ r {
            case [Some(1), Some(2)] => true
            case _                  => false
        }
    }

    @test
    def testMatchOptionArray4(): Bool = region r {
        match [[Some(1)] @ r, [Some(2)] @ r] @ r {
            case [[Some(1)], [Some(2)]] => true
            case _                      => false
        }
    }

    @test
    def testMatchResultArray1(): Bool = region r {
        match [Ok(42)] @ r {
            case [Ok(42)] => true
            case _        => false
        }
    }

    @test
    def testMatchResultArray2(): Bool = region r {
        match [Ok(42)] @ r {
            case [Err(23)] => false
            case _         => true
        }
    }

    @test
    def testMatchResultArray3(): Bool = region r {
        match [Ok(42), Err(23)] @ r {
            case [Ok(42), Err(23)] => true
            case _                 => false
        }
    }

    @test
    def testMatchResultArray4(): Bool = region r {
        match [[Ok(42)] @ r, [Err(23)] @ r] @ r {
            case [[Ok(42)], [Err(23)]] => true
            case _                     => false
        }
    }

    enum Option[t] {
        case None,
        case Some(t)
    }

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

}
