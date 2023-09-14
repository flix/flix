mod Test.Exp.Ref.Assign {

    @Test
    def testAssignUnit01(): Bool = region rc {
        let l = ref () @ rc;
        l := ();
        deref l == ()
    }

    @Test
    def testAssignBool01(): Bool = region rc {
        let l = ref true @ rc;
        l := false;
        deref l == false
    }

    @Test
    def testAssignBool02(): Bool = region rc {
        let l = ref false @ rc;
        l := true;
        deref l == true
    }

    @Test
    def testAssignChar01(): Bool = region rc {
        let l = ref 'a' @ rc;
        l := 'b';
        deref l == 'b'
    }

    @Test
    def testAssignFloat3201(): Bool = region rc {
        let l = ref 123.456f32 @ rc;
        l := 456.123f32;
        deref l == 456.123f32
    }

    @Test
    def testAssignFloat6401(): Bool = region rc {
        let l = ref 123.456f64 @ rc;
        l := 456.123f64;
        deref l == 456.123f64
    }

    @Test
    def testAssignBigDecimal01(): Bool = region rc {
        let l = ref 123.456ff @ rc;
        l := 456.123ff;
        deref l == 456.123ff
    }

    @Test
    def testAssignInt801(): Bool = region rc {
        let l = ref 123i8 @ rc;
        l := 111i8;
        deref l == 111i8
    }

    @Test
    def testAssignInt1601(): Bool = region rc {
        let l = ref 123i16 @ rc;
        l := 456i16;
        deref l == 456i16
    }

    @Test
    def testAssignInt3201(): Bool = region rc {
        let l = ref 123i32 @ rc;
        l := 456i32;
        deref l == 456i32
    }

    @Test
    def testAssignInt6401(): Bool = region rc {
        let l = ref 123i64 @ rc;
        l := 456i64;
        deref l == 456i64
    }

    @Test
    def testAssignBigInt01(): Bool = region rc {
        let l = ref 111111111ii @ rc;
        l := 999999999ii;
        deref l == 999999999ii
    }

    @Test
    def testAssignStr01(): Bool = region rc {
        let l = ref "Hello World!" @ rc;
        l := "Goodbye World!";
        deref l == "Goodbye World!"
    }

    @Test
    def testAssignOption01(): Bool = region rc {
        let l = ref None : Option[Int32] @ rc;
        l := Some(123);
        deref l == Some(123)
    }

    @Test
    def testAssignOption02(): Bool = region rc {
        let l = ref Some(123) @ rc;
        l := None;
        deref l == None
    }

    @Test
    def testAssignResult01(): Bool = region rc {
        let l = ref Ok(123) @ rc;
        l := Err(456);
        deref l == Err(456)
    }

    @Test
    def testAssignResult02(): Bool = region rc {
        let l = ref Err(123) @ rc;
        l := Ok(456);
        deref l == Ok(456)
    }

    @Test
    def testAssignAssign01(): Bool = region rc {
        let l = ref 1 @ rc;
        l := 2;
        l := 3;
        deref l == 3
    }

    @Test
    def testAssignAssign02(): Bool = region rc {
        let l = ref 1 @ rc;
        l := 2;
        l := 3;
        l := 4;
        deref l == 4
    }

    @Test
    def testAssignAssign03(): Bool = region rc {
        let l = ref 1 @ rc;
        l := 2;
        l := 3;
        l := 4;
        l := 5;
        deref l == 5
    }

    @Test
    def testAssignAssign04(): Bool = region rc {
        let l = ref 1 @ rc;
        l := deref l + 1;
        deref l == 2
    }

    @Test
    def testAssignAssign05(): Bool = region rc {
        let l = ref 1 @ rc;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 3
    }

    @Test
    def testAssignAssign06(): Bool = region rc {
        let l = ref 1 @ rc;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 4
    }

    @Test
    def testAssignAssign07(): Bool = region rc {
        let l = ref 1 @ rc;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 5
    }

    @Test
    def testRefRef01(): Bool = region rc {
        let l = ref (ref 123 @ rc) @ rc;
        (deref l) := 456;
        deref (deref l) == 456
    }

    @Test
    def testRefRef02(): Bool = region rc {
        let l = ref (ref (ref 123 @ rc) @ rc) @ rc;
        (deref (deref l)) := 456;
        deref (deref (deref l)) == 456
    }

    @Test
    def testAlias01(): Bool = region rc {
        let l1 = ref (ref 123 @ rc) @ rc;
        let l2 = ref 456 @ rc;
        l1 := l2;
        deref (deref l1) == 456
    }

    @Test
    def testAlias02(): Bool = region rc {
        let l1 = ref (ref 123 @ rc) @ rc;
        let l2 = ref 456 @ rc;
        l1 := l2;
        l2 := 789;
        deref (deref l1) == 789
    }

    @Test
    def testMultipleRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = ref 1 @ rc1;
                let y = ref 2 @ rc2;
                (deref x + deref y) == 3
            }
        }

    @Test
    def testCapturedRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = ref 1 @ rc1;
                let y = ref 2 @ rc2;
                let f = () -> x := 7;
                let g = () -> y := 8;
                f();
                g();
                (deref x + deref y) == 15
            }
        }
}
