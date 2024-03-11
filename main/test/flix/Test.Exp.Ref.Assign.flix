mod Test.Exp.Ref.Assign {

    @test
    def testAssignUnit01(): Bool = region rc {
        let l = Ref.fresh(rc, ());
        Ref.put((), l);
        Ref.get(l) == ()
    }

    @test
    def testAssignBool01(): Bool = region rc {
        let l = ref true @ rc;
        Ref.put(false, l);
        Ref.get(l) == false
    }

    @test
    def testAssignBool02(): Bool = region rc {
        let l = ref false @ rc;
        Ref.put(true, l);
        Ref.get(l) == true
    }

    @test
    def testAssignChar01(): Bool = region rc {
        let l = ref 'a' @ rc;
        Ref.put('b', l);
        Ref.get(l) == 'b'
    }

    @test
    def testAssignFloat3201(): Bool = region rc {
        let l = ref 123.456f32 @ rc;
        Ref.put(456.123f32, l);
        Ref.get(l) == 456.123f32
    }

    @test
    def testAssignFloat6401(): Bool = region rc {
        let l = ref 123.456f64 @ rc;
        Ref.put(456.123f64, l);
        Ref.get(l) == 456.123f64
    }

    @test
    def testAssignBigDecimal01(): Bool = region rc {
        let l = ref 123.456ff @ rc;
        Ref.put(456.123ff, l);
        Ref.get(l) == 456.123ff
    }

    @test
    def testAssignInt801(): Bool = region rc {
        let l = ref 123i8 @ rc;
        Ref.put(111i8, l);
        Ref.get(l) == 111i8
    }

    @test
    def testAssignInt1601(): Bool = region rc {
        let l = ref 123i16 @ rc;
        Ref.put(456i16, l);
        Ref.get(l) == 456i16
    }

    @test
    def testAssignInt3201(): Bool = region rc {
        let l = ref 123i32 @ rc;
        Ref.put(456i32, l);
        Ref.get(l) == 456i32
    }

    @test
    def testAssignInt6401(): Bool = region rc {
        let l = ref 123i64 @ rc;
        Ref.put(456i64, l);
        Ref.get(l) == 456i64
    }

    @test
    def testAssignBigInt01(): Bool = region rc {
        let l = ref 111111111ii @ rc;
        Ref.put(999999999ii, l);
        Ref.get(l) == 999999999ii
    }

    @test
    def testAssignStr01(): Bool = region rc {
        let l = ref "Hello World!" @ rc;
        Ref.put("Goodbye World!", l);
        Ref.get(l) == "Goodbye World!"
    }

    @test
    def testAssignOption01(): Bool = region rc {
        let l = ref None @ rc;
        Ref.put(Some(123), l);
        Ref.get(l) == Some(123)
    }

    @test
    def testAssignOption02(): Bool = region rc {
        let l = ref Some(123) @ rc;
        Ref.put(None, l);
        Ref.get(l) == None
    }

    @test
    def testAssignResult01(): Bool = region rc {
        let l = ref Ok(123) @ rc;
        Ref.put(Err(456), l);
        Ref.get(l) == Err(456)
    }

    @test
    def testAssignResult02(): Bool = region rc {
        let l = ref Err(123) @ rc;
        Ref.put(Ok(456), l);
        Ref.get(l) == Ok(456)
    }

    @test
    def testAssignAssign01(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.get(l) == 3
    }

    @test
    def testAssignAssign02(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        Ref.get(l) == 4
    }

    @test
    def testAssignAssign03(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        Ref.put(5, l);
        Ref.get(l) == 5
    }

    @test
    def testAssignAssign04(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(Ref.get(l) + 1, l);
        Ref.get(l) == 2
    }

    @test
    def testAssignAssign05(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.get(l) == 3
    }

    @test
    def testAssignAssign06(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.get(l) == 4
    }

    @test
    def testAssignAssign07(): Bool = region rc {
        let l = ref 1 @ rc;
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.get(l) == 5
    }

    @test
    def testRefRef01(): Bool = region rc {
        let l = ref (ref 123 @ rc) @ rc;
        Ref.put(456, Ref.get(l));
        Ref.get(Ref.get(l)) == 456
    }

    @test
    def testRefRef02(): Bool = region rc {
        let l = ref (ref (ref 123 @ rc) @ rc) @ rc;
        Ref.put(456, Ref.get(Ref.get(l)));
        Ref.get(Ref.get(Ref.get(l))) == 456
    }

    @test
    def testAlias01(): Bool = region rc {
        let l1 = ref (ref 123 @ rc) @ rc;
        let l2 = ref 456 @ rc;
        Ref.put(l2, l1);
        Ref.get(Ref.get(l1)) == 456
    }

    @test
    def testAlias02(): Bool = region rc {
        let l1 = ref (ref 123 @ rc) @ rc;
        let l2 = ref 456 @ rc;
        Ref.put(l2, l1);
        Ref.put(789, l2);
        Ref.get(Ref.get(l1)) == 789
    }

    @test
    def testMultipleRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = ref 1 @ rc1;
                let y = ref 2 @ rc2;
                Ref.get(x) + Ref.get(y) == 3
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
                (Ref.get(x) + Ref.get(y)) == 15
            }
        }
}
