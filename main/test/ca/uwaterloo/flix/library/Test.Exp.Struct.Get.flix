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
    def testStructMultiGet01(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let fstName = s€fstName;
            let lstName = s€lstName;
            let age = s€age;
            let cowboy = s€cowboy;
            fstName == "Lucky" and lstName == "Luke" and age == 42 and cowboy
        }

    @test
    def testStructMultiGet02(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let cowboy = s€cowboy;
            let age = s€age;
            let lstName = s€lstName;
            let fstName = s€fstName;
            fstName == "Lucky" and lstName == "Luke" and age == 42 and cowboy
        }

    @test
    def testStructMultiGet03(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let fstName1 = s€fstName;
            let fstName2 = s€fstName;
            let lstName1 = s€lstName;
            let lstName2 = s€lstName;
            fstName1 == "Lucky" and fstName2 == "Lucky" and lstName1 == "Luke" and lstName2 == "Luke"
        }

    @test
    def testStructMultiGet04(): Bool =
        region rc {
            let s = new Get { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            let lstName1 = s€lstName;
            let fstName1 = s€fstName;
            let lstName2 = s€lstName;
            let fstName2 = s€fstName;
            fstName1 == "Lucky" and fstName2 == "Lucky" and lstName1 == "Luke" and lstName2 == "Luke"
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
