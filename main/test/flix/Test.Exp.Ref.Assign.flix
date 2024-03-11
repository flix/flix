mod Test.Exp.Ref.Assign {

    @test
    def testAssignUnit01(): Bool = region rc {
        let l = ref () @ rc;
        Ref.put((), l);
        deref l == ()
    }

    @test
    def testAssignBool01(): Bool = region rc {
        let l = ref true @ rc;
        Ref.put(false, l);
        deref l == false
    }

    @test
    def testAssignBool02(): Bool = region rc {
        let l = ref false @ rc;
        Ref.put(true, l);
        deref l == true
    }

    @test
    def testAssignChar01(): Bool = region rc {
        let l = ref 'a' @ rc;
        Ref.put('b', l);
        deref l == 'b'
    }

    @test
    def testAssignFloat3201(): Bool = region rc {
        let l = ref 123.456f32 @ rc;
        Ref.put(456.123f32, l);
        deref l == 456.123f32
    }

    @test
    def testAssignFloat6401(): Bool = region rc {
        let l = ref 123.456f64 @ rc;
        Ref.put(456.123f64, l);
        deref l == 456.123f64
    }

    @test
    def testAssignBigDecimal01(): Bool = region rc {
        let l = ref 123.456ff @ rc;
        Ref.put(456.123ff, l);
        deref l == 456.123ff
    }

    @test
    def testAssignInt801(): Bool = region rc {
        let l = ref 123i8 @ rc;
        Ref.put(111i8, l);
        deref l == 111i8
    }

    @test
    def testAssignInt1601(): Bool = region rc {
        let l = ref 123i16 @ rc;
        Ref.put(456i16, l);
        deref l == 456i16
    }

    @test
    def testAssignInt3201(): Bool = region rc {
        let l = ref 123i32 @ rc;
        Ref.put(456i32, l);
        deref l == 456i32
    }

    @test
    def testAssignInt6401(): Bool = region rc {
        let l = ref 123i64 @ rc;
        Ref.put(456i64, l);
        deref l == 456i64
    }

    @test
    def testAssignBigInt01(): Bool = region rc {
        let l = ref 111111111ii @ rc;
        Ref.put(999999999ii, l);
        deref l == 999999999ii
    }

    @test
    def testAssignStr01(): Bool = region rc {
        let l = ref "Hello World!" @ rc;
        Ref.put("Goodbye World!", l);
        deref l == "Goodbye World!"
    }

    @test
    def testAssignOption01(): Bool = region rc {
        let l = ref None @ rc;
        Ref.put(Some(123), l);
        deref l == Some(123)
    }

    @test
    def testAssignOption02(): Bool = region rc {
        let l = ref Some(123) @ rc;
        Ref.put(None, l);
        deref l == None
    }

    @test
    def testAssignResult01(): Bool = region rc {
        let l = ref Ok(123) @ rc;
        Ref.put(Err(456), l);
        deref l == Err(456)
    }

    @test
    def testAssignResult02(): Bool = region rc {
        let l = ref Err(123) @ rc;
        Ref.put(Ok(456), l);
        deref l == Ok(456)
    }

    @test
    def testAssignAssign01(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(2, l);
        Ref.put(3, l);
        deref l == 3
    }

    @test
    def testAssignAssign02(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        deref l == 4
    }

    @test
    def testAssignAssign03(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        Ref.put(5, l);
        deref l == 5
    }

    @test
    def testAssignAssign04(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(deref l) + 1, l);
        deref l == 2
    }

    @test
    def testAssignAssign05(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(deref l) + 1, l);
        Ref.put(deref l) + 1, l);
        deref l == 3
    }

    @test
    def testAssignAssign06(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(deref l) + 1, l);
        Ref.put(deref l) + 1, l);
        Ref.put(deref l) + 1, l);
        deref l == 4
    }

    @test
    def testAssignAssign07(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(deref l) + 1, l);
        Ref.put(deref l) + 1, l);
        Ref.put(deref l) + 1, l);
        Ref.put(deref l) + 1, l);
        deref l == 5
    }

    @test
    def testRefRef01(): Bool = region rc {
        let l = ref (ref 123 @ rc) @ rc;
        Ref.put(456, deref l);
        deref (deref l) == 456
    }

    @test
    def testRefRef02(): Bool = region rc {
        let l = ref (ref (ref 123 @ rc) @ rc) @ rc;
        Ref.put(456, deref deref l);
        deref (deref (deref l)) == 456
    }

    @test
    def testAlias01(): Bool = region rc {
        let l1 = ref (ref 123 @ rc) @ rc;
        let l2 = ref 456 @ rc;
        Ref.put(l2, l1);
        deref (deref l1) == 456
    }

    @test
    def testAlias02(): Bool = region rc {
        let l1 = ref (ref 123 @ rc) @ rc;
        let l2 = ref 456 @ rc;
        Ref.put(l2, l1);
        Ref.put(789, l2);
        deref (deref l1) == 789
    }

    @test
    def testMultipleRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = ref 1 @ rc1;
                let y = ref 2 @ rc2;
                (deref x + deref y) == 3
            }
        }

    @test
    def testCapturedRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = ref 1 @ rc1;
                let y = ref 2 @ rc2;
                let f = () -> Ref.put(7, x);
                let g = () -> Ref.put(8, y);
                f();
                g();
                (deref x + deref y) == 15
            }
        }
}
