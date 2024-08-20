mod Test.Exp.Ref.Assign {

    @test
    def testAssignUnit01(): Bool = region rc {
        let l = Ref.fresh(rc, ());
        Ref.put((), l);
        deref l == ()
    }

    @test
    def testAssignBool01(): Bool = region rc {
        let l = Ref.fresh(rc, true);
        Ref.put(false, l);
        deref l == false
    }

    @test
    def testAssignBool02(): Bool = region rc {
        let l = Ref.fresh(rc, false);
        Ref.put(true, l);
        deref l == true
    }

    @test
    def testAssignChar01(): Bool = region rc {
        let l = Ref.fresh(rc, 'a');
        Ref.put('b', l);
        deref l == 'b'
    }

    @test
    def testAssignFloat3201(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456f32);
        Ref.put(456.123f32, l);
        deref l == 456.123f32
    }

    @test
    def testAssignFloat6401(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456f64);
        Ref.put(456.123f64, l);
        deref l == 456.123f64
    }

    @test
    def testAssignBigDecimal01(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456ff);
        Ref.put(456.123ff, l);
        deref l == 456.123ff
    }

    @test
    def testAssignInt801(): Bool = region rc {
        let l = Ref.fresh(rc, 123i8);
        Ref.put(111i8, l);
        deref l == 111i8
    }

    @test
    def testAssignInt1601(): Bool = region rc {
        let l = Ref.fresh(rc, 123i16);
        Ref.put(456i16, l);
        deref l == 456i16
    }

    @test
    def testAssignInt3201(): Bool = region rc {
        let l = Ref.fresh(rc, 123i32);
        Ref.put(456i32, l);
        deref l == 456i32
    }

    @test
    def testAssignInt6401(): Bool = region rc {
        let l = Ref.fresh(rc, 123i64);
        Ref.put(456i64, l);
        deref l == 456i64
    }

    @test
    def testAssignBigInt01(): Bool = region rc {
        let l = Ref.fresh(rc, 111111111ii);
        Ref.put(999999999ii, l);
        deref l == 999999999ii
    }

    @test
    def testAssignStr01(): Bool = region rc {
        let l = Ref.fresh(rc, "Hello World!");
        Ref.put("Goodbye World!", l);
        deref l == "Goodbye World!"
    }

    @test
    def testAssignOption01(): Bool = region rc {
        let l = Ref.fresh(rc, None);
        Ref.put(Some(123), l);
        deref l == Some(123)
    }

    @test
    def testAssignOption02(): Bool = region rc {
        let l = Ref.fresh(rc, Some(123));
        Ref.put(None, l);
        deref l == None
    }

    @test
    def testAssignResult01(): Bool = region rc {
        let l = Ref.fresh(rc, Ok(123));
        Ref.put(Err(456), l);
        deref l == Err(456)
    }

    @test
    def testAssignResult02(): Bool = region rc {
        let l = Ref.fresh(rc, Err(123));
        Ref.put(Ok(456), l);
        deref l == Ok(456)
    }

    @test
    def testAssignAssign01(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(2, l);
        Ref.put(3, l);
        deref l == 3
    }

    @test
    def testAssignAssign02(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        deref l == 4
    }

    @test
    def testAssignAssign03(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        Ref.put(5, l);
        deref l == 5
    }

    @test
    def testAssignAssign04(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(deref l + 1, l);
        deref l == 2
    }

    @test
    def testAssignAssign05(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(deref l + 1, l);
        Ref.put(deref l + 1, l);
        deref l == 3
    }

    @test
    def testAssignAssign06(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(deref l + 1, l);
        Ref.put(deref l + 1, l);
        Ref.put(deref l + 1, l);
        deref l == 4
    }

    @test
    def testAssignAssign07(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(deref l + 1, l);
        Ref.put(deref l + 1, l);
        Ref.put(deref l + 1, l);
        Ref.put(deref l + 1, l);
        deref l == 5
    }

    @test
    def testRefRef01(): Bool = region rc {
        let l = Ref.fresh(rc, Ref.fresh(rc, 123));
        Ref.put(456, deref l);
        deref (deref l) == 456
    }

    @test
    def testRefRef02(): Bool = region rc {
        let l = Ref.fresh(rc, Ref.fresh(rc, Ref.fresh(rc, 123)));
        Ref.put(456, deref (deref l));
        deref (deref (deref l)) == 456
    }

    @test
    def testAlias01(): Bool = region rc {
        let l1 = Ref.fresh(rc, Ref.fresh(rc, 123));
        let l2 = Ref.fresh(rc, 456);
        Ref.put(l2, l1);
        deref (deref l1) == 456
    }

    @test
    def testAlias02(): Bool = region rc {
        let l1 = Ref.fresh(rc, Ref.fresh(rc, 123));
        let l2 = Ref.fresh(rc, 456);
        Ref.put(l2, l1);
        Ref.put(789, l2);
        deref (deref l1) == 789
    }

    @test
    def testMultipleRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = Ref.fresh(rc1, 1);
                let y = Ref.fresh(rc2, 2);
                (deref x + deref y) == 3
            }
        }

    @test
    def testCapturedRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = Ref.fresh(rc1, 1);
                let y = Ref.fresh(rc2, 2);
                let f = () -> Ref.put(7, x);
                let g = () -> Ref.put(8, y);
                f();
                g();
                (deref x + deref y) == 15
            }
        }
}
