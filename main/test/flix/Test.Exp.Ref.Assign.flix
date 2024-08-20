mod Test.Exp.Ref.Assign {

    @test
    def testAssignUnit01(): Bool = region rc {
        let l = Ref.fresh(rc, ());
        Ref.put((), l);
        Ref.get(l) == ()
    }

    @test
    def testAssignBool01(): Bool = region rc {
        let l = Ref.fresh(rc, true);
        Ref.put(false, l);
        Ref.get(l) == false
    }

    @test
    def testAssignBool02(): Bool = region rc {
        let l = Ref.fresh(rc, false);
        Ref.put(true, l);
        Ref.get(l) == true
    }

    @test
    def testAssignChar01(): Bool = region rc {
        let l = Ref.fresh(rc, 'a');
        Ref.put('b', l);
        Ref.get(l) == 'b'
    }

    @test
    def testAssignFloat3201(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456f32);
        Ref.put(456.123f32, l);
        Ref.get(l) == 456.123f32
    }

    @test
    def testAssignFloat6401(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456f64);
        Ref.put(456.123f64, l);
        Ref.get(l) == 456.123f64
    }

    @test
    def testAssignBigDecimal01(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456ff);
        Ref.put(456.123ff, l);
        Ref.get(l) == 456.123ff
    }

    @test
    def testAssignInt801(): Bool = region rc {
        let l = Ref.fresh(rc, 123i8);
        Ref.put(111i8, l);
        Ref.get(l) == 111i8
    }

    @test
    def testAssignInt1601(): Bool = region rc {
        let l = Ref.fresh(rc, 123i16);
        Ref.put(456i16, l);
        Ref.get(l) == 456i16
    }

    @test
    def testAssignInt3201(): Bool = region rc {
        let l = Ref.fresh(rc, 123i32);
        Ref.put(456i32, l);
        Ref.get(l) == 456i32
    }

    @test
    def testAssignInt6401(): Bool = region rc {
        let l = Ref.fresh(rc, 123i64);
        Ref.put(456i64, l);
        Ref.get(l) == 456i64
    }

    @test
    def testAssignBigInt01(): Bool = region rc {
        let l = Ref.fresh(rc, 111111111ii);
        Ref.put(999999999ii, l);
        Ref.get(l) == 999999999ii
    }

    @test
    def testAssignStr01(): Bool = region rc {
        let l = Ref.fresh(rc, "Hello World!");
        Ref.put("Goodbye World!", l);
        Ref.get(l) == "Goodbye World!"
    }

    @test
    def testAssignOption01(): Bool = region rc {
        let l = Ref.fresh(rc, None);
        Ref.put(Some(123), l);
        Ref.get(l) == Some(123)
    }

    @test
    def testAssignOption02(): Bool = region rc {
        let l = Ref.fresh(rc, Some(123));
        Ref.put(None, l);
        Ref.get(l) == None
    }

    @test
    def testAssignResult01(): Bool = region rc {
        let l = Ref.fresh(rc, Ok(123));
        Ref.put(Err(456), l);
        Ref.get(l) == Err(456)
    }

    @test
    def testAssignResult02(): Bool = region rc {
        let l = Ref.fresh(rc, Err(123));
        Ref.put(Ok(456), l);
        Ref.get(l) == Ok(456)
    }

    @test
    def testAssignAssign01(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.get(l) == 3
    }

    @test
    def testAssignAssign02(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        Ref.get(l) == 4
    }

    @test
    def testAssignAssign03(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(2, l);
        Ref.put(3, l);
        Ref.put(4, l);
        Ref.put(5, l);
        Ref.get(l) == 5
    }

    @test
    def testAssignAssign04(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(Ref.get(l) + 1, l);
        Ref.get(l) == 2
    }

    @test
    def testAssignAssign05(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
    }

    @test
    def testAssignAssign06(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.get(l) == 4
    }

    @test
    def testAssignAssign07(): Bool = region rc {
        let l = Ref.fresh(rc, 1);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.put(Ref.get(l) + 1, l);
        Ref.get(l) == 5
    }

    @test
    def testRefRef01(): Bool = region rc {
        let l = Ref.fresh(rc, Ref.fresh(rc, 123));
        Ref.put(456, Ref.get(l));
        Ref.get(Ref.get(l)) == 456
    }

    @test
    def testRefRef02(): Bool = region rc {
        let l = Ref.fresh(rc, Ref.fresh(rc, Ref.fresh(rc, 123)));
        Ref.put(456, Ref.get(Ref.get(l)));
        Ref.get(Ref.get(Ref.get(l))) == 456
    }

    @test
    def testAlias01(): Bool = region rc {
        let l1 = Ref.fresh(rc, Ref.fresh(rc, 123));
        let l2 = Ref.fresh(rc, 456);
        Ref.put(l2, l1);
        Ref.get(Ref.get(l1)) == 456
    }

    @test
    def testAlias02(): Bool = region rc {
        let l1 = Ref.fresh(rc, Ref.fresh(rc, 123));
        let l2 = Ref.fresh(rc, 456);
        Ref.put(l2, l1);
        Ref.put(789, l2);
        Ref.get(Ref.get(l1)) == 789
    }

    @test
    def testMultipleRegions01(): Bool = // Pure
        region rc1 {
            region rc2 {
                let x = Ref.fresh(rc1, 1);
                let y = Ref.fresh(rc2, 2);
                (Ref.get(x) + Ref.get(y)) == 3
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
                (Ref.get(x) + Ref.get(y)) == 15
            }
        }
}
