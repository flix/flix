mod Test.Exp.Ref.Assign {

    @test
    def testAssignUnit01(): Bool = region r {
        let l = ref () @ r;
        l := ();
        deref l == ()
    }

    @test
    def testAssignBool01(): Bool = region r {
        let l = ref true @ r;
        l := false;
        deref l == false
    }

    @test
    def testAssignBool02(): Bool = region r {
        let l = ref false @ r;
        l := true;
        deref l == true
    }

    @test
    def testAssignChar01(): Bool = region r {
        let l = ref 'a' @ r;
        l := 'b';
        deref l == 'b'
    }

    @test
    def testAssignFloat3201(): Bool = region r {
        let l = ref 123.456f32 @ r;
        l := 456.123f32;
        deref l == 456.123f32
    }

    @test
    def testAssignFloat6401(): Bool = region r {
        let l = ref 123.456f64 @ r;
        l := 456.123f64;
        deref l == 456.123f64
    }

    @test
    def testAssignBigDecimal01(): Bool = region r {
        let l = ref 123.456ff @ r;
        l := 456.123ff;
        deref l == 456.123ff
    }

    @test
    def testAssignInt801(): Bool = region r {
        let l = ref 123i8 @ r;
        l := 111i8;
        deref l == 111i8
    }

    @test
    def testAssignInt1601(): Bool = region r {
        let l = ref 123i16 @ r;
        l := 456i16;
        deref l == 456i16
    }

    @test
    def testAssignInt3201(): Bool = region r {
        let l = ref 123i32 @ r;
        l := 456i32;
        deref l == 456i32
    }

    @test
    def testAssignInt6401(): Bool = region r {
        let l = ref 123i64 @ r;
        l := 456i64;
        deref l == 456i64
    }

    @test
    def testAssignBigInt01(): Bool = region r {
        let l = ref 111111111ii @ r;
        l := 999999999ii;
        deref l == 999999999ii
    }

    @test
    def testAssignStr01(): Bool = region r {
        let l = ref "Hello World!" @ r;
        l := "Goodbye World!";
        deref l == "Goodbye World!"
    }

    @test
    def testAssignOption01(): Bool = region r {
        let l = ref None : Option[Int32] @ r;
        l := Some(123);
        deref l == Some(123)
    }

    @test
    def testAssignOption02(): Bool = region r {
        let l = ref Some(123) @ r;
        l := None;
        deref l == None
    }

    @test
    def testAssignResult01(): Bool = region r {
        let l = ref Ok(123) @ r;
        l := Err(456);
        deref l == Err(456)
    }

    @test
    def testAssignResult02(): Bool = region r {
        let l = ref Err(123) @ r;
        l := Ok(456);
        deref l == Ok(456)
    }

    @test
    def testAssignAssign01(): Bool = region r {
        let l = ref 1 @ r;
        l := 2;
        l := 3;
        deref l == 3
    }

    @test
    def testAssignAssign02(): Bool = region r {
        let l = ref 1 @ r;
        l := 2;
        l := 3;
        l := 4;
        deref l == 4
    }

    @test
    def testAssignAssign03(): Bool = region r {
        let l = ref 1 @ r;
        l := 2;
        l := 3;
        l := 4;
        l := 5;
        deref l == 5
    }

    @test
    def testAssignAssign04(): Bool = region r {
        let l = ref 1 @ r;
        l := deref l + 1;
        deref l == 2
    }

    @test
    def testAssignAssign05(): Bool = region r {
        let l = ref 1 @ r;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 3
    }

    @test
    def testAssignAssign06(): Bool = region r {
        let l = ref 1 @ r;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 4
    }

    @test
    def testAssignAssign07(): Bool = region r {
        let l = ref 1 @ r;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 5
    }

    @test
    def testRefRef01(): Bool = region r {
        let l = ref (ref 123 @ r) @ r;
        (deref l) := 456;
        deref (deref l) == 456
    }

    @test
    def testRefRef02(): Bool = region r {
        let l = ref (ref (ref 123 @ r) @ r) @ r;
        (deref (deref l)) := 456;
        deref (deref (deref l)) == 456
    }

    @test
    def testAlias01(): Bool = region r {
        let l1 = ref (ref 123 @ r) @ r;
        let l2 = ref 456 @ r;
        l1 := l2;
        deref (deref l1) == 456
    }

    @test
    def testAlias02(): Bool = region r {
        let l1 = ref (ref 123 @ r) @ r;
        let l2 = ref 456 @ r;
        l1 := l2;
        l2 := 789;
        deref (deref l1) == 789
    }

    @test
    def testMultipleRegions01(): Bool = // Pure
        region r1 {
            region r2 {
                let x = ref 1 @ r1;
                let y = ref 2 @ r2;
                (deref x + deref y) == 3
            }
        }

    @test
    def testCapturedRegions01(): Bool = // Pure
        region r1 {
            region r2 {
                let x = ref 1 @ r1;
                let y = ref 2 @ r2;
                let f = () -> x := 7;
                let g = () -> y := 8;
                f();
                g();
                (deref x + deref y) == 15
            }
        }
}
