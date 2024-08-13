mod Test.Exp.Struct.Put {

    struct Put[r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    @test
    def testStructPut01(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName == "Unlucky"
        }

    @test
    def testStructPut02(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€lstName == "Uriel"
        }

    @test
    def testStructPut03(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€age = 23;
            s€age == 23
        }

    @test
    def testStructPut04(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            not s€cowboy and s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42
        }

    @test
    def testStructMultiPut01(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€lstName = "Uriel";
            s€age = 23;
            s€cowboy = false;
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut02(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            s€age = 23;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€fstName == "Unlucky" and s€lstName == "Uriel" and s€age == 23 and s€cowboy == false
        }

    @test
    def testStructMultiPut03(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName = "Lucky";
            s€lstName = "Uriel";
            s€lstName = "Luke";
            s€lstName == "Luke" and s€fstName == "Lucky" and s€age == 42 and s€cowboy == true
        }

    @test
    def testStructMultiPut04(): Bool =
        region rc {
            let s = new Put { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€lstName = "Luke";
            s€fstName = "Lucky";
            s€fstName == "Lucky" and s€lstName == "Luke" and s€age == 42 and s€cowboy == true
        }

/* Cannot test these while struct name is required to be mod name
    struct Inner[r] {
        x: Int32,
        y: Int32
    }

    struct Outer[r] {
        a: Inner[r]
        b: Inner[r]
    }

    @test
    def testNestedPut01(): Unit =
        region rc {
            let s = new Outer { a = new Inner { x = 1, y = 2}, b = new Inner { x = 1, y = 4 } } @ rc;
            let _ax = s€a€x;
            let _ay = s€a€y;
            let _bx = s€b€x;
            let _by = s€b€y;
            ()
        }

    struct A[r] {
        a: B
    }

    struct B[r] {
        b: C
    }

    struct C[r] {
        c: D
    }

    struct D[r] {
        d: Int32
    }

    @test
    def testNestedPut02(): Unit =
        region rc {
            let s = new A { a = new B { b = new C { c = new D { d = 42 } } } } @ rc;
            let t = new A { a = new B { b = new C { c = new D { d = 5 } } } } @ rc;
            s€a = t€a;
            s€a€b = t€a€b;
            t€a€b€c = s€a€b€c;
            s€a€b€c€d = 15;
            ()
        }
        */
}
