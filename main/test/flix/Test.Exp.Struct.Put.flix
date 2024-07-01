mod Test.Exp.Struct.Put {

    struct Cowboy [r] {
        fstName: String,
        lstName: String,
        age: Int,
        cowboy: Bool
    }

    @test
    def testStructPut01(): String =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky"
        }

    @test
    def testStructPut02(): String =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel"
        }

    @test
    def testStructPut03(): Int32 =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€age = 23
        }

    @test
    def testStructPut04(): Bool =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false
        }

    @test
    def testStructMultiPut01(): Unit =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€lstName = "Uriel";
            s€age = 23;
            s€cowboy = false;
            ()
        }

    @test
    def testStructMultiPut02(): Unit =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€cowboy = false;
            s€age = 23;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            ()
        }

    @test
    def testStructMultiPut03(): Unit =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€fstName = "Unlucky";
            s€fstName = "Lucky";
            s€lstName = "Uriel";
            s€lstName = "Luke";
            ()
        }

    @test
    def testStructMultiPut04(): Unit =
        region rc {
            let s = new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            s€lstName = "Uriel";
            s€fstName = "Unlucky";
            s€lstName = "Luke";
            s€fstName = "Lucky";
            ()
        }

    struct Inner [r] {
        x: Int32,
        y: Int32
    }

    struct Outer [r] {
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
