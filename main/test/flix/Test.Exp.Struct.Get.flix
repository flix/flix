mod Test.Exp.Struct.Get {

    struct Get [r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    @test
    def testStructGet01(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42 and s€cowboy == true
        }

    @test
    def testStructGet02(): String =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName
        }

    @test
    def testStructGet03(): Int32 =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€age
        }

    @test
    def testStructGet04(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy
        }

    @test
    def testStructMultiGet01(): Unit =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let _fstName = s€fstName;
            let _lstName = s€lstName;
            let _age = s€age;
            let _cowboy = s€cowboy;
            ()
        }

    @test
    def testStructMultiGet02(): Unit =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let _cowboy = s€cowboy;
            let _age = s€age;
            let _lstName = s€lstName;
            let _fstName = s€fstName;
            ()
        }

    @test
    def testStructMultiGet03(): Unit =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let _fstName1 = s€fstName;
            let _fstName2 = s€fstName;
            let _lstName1 = s€lstName;
            let _lstName2 = s€lstName;
            ()
        }

    @test
    def testStructMultiGet04(): Unit =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let _lstName1 = s€lstName;
            let _fstName1 = s€fstName;
            let _lstName2 = s€lstName;
            let _fstName2 = s€fstName;
            ()
        }

/* These aren't testable until we get struct names working
    struct Inner [r] {
        x: Int32,
        y: Int32
    }

    struct Outer [r] {
        a: TwoInts[r]
        b: TwoInts[r]
    }

    @test
    def testNestedGet01(): Unit =
        region rc {
            let s = new Outer { a = new Inner { x = 1, y = 2}, b = new Inner { x = 1, y = 4 } } @ rc;
            let _ax = s€a€x;
            let _ay = s€a€y;
            let _bx = s€b€x;
            let _by = s€b€y;
            ()
        }

    struct A [r] {
        a: B
    }

    struct B [r] {
        b: C
    }

    struct C [r] {
        c: D
    }

    struct D [r] {
        d: Int32
    }

    @test
    def testNestedGet02(): Unit =
        region rc {
            let s = new A { a = new B { b = new C { c = new D { d = 42 } } } } @ rc;
            let _a = s€a;
            let _b = s€a€b;
            let _c = s€a€b€c;
            let _d = s€a€b€c€d;
            ()
        }
*/

}
