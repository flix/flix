mod Test.Exp.Struct.Put {

    struct Put [r] {
        _fstName: String,
        _lstName: String,
        _age: Int32,
        _cowboy: Bool
    }

    @test
    def testStructPut01(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_fstName = "Unlucky";
            s€_fstName == "Unlucky"
        }

    @test
    def testStructPut02(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_lstName = "Uriel";
            s€_lstName == "Uriel"
        }

    @test
    def testStructPut03(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_age = 23;
            s€_age == 23
        }

    @test
    def testStructPut04(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_cowboy = false;
            not s€_cowboy and s€_fstName == "Lucky" and s€_lstName == "Luke" and s€_age == 42
        }

    @test
    def testStructMultiPut01(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_fstName = "Unlucky";
            s€_lstName = "Uriel";
            s€_age = 23;
            s€_cowboy = false;
            s€_fstName == "Unlucky" and s€_lstName == "Uriel" and s€_age == 23 and s€_cowboy == false
        }

    @test
    def testStructMultiPut02(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_cowboy = false;
            s€_age = 23;
            s€_lstName = "Uriel";
            s€_fstName = "Unlucky";
            s€_fstName == "Unlucky" and s€_lstName == "Uriel" and s€_age == 23 and s€_cowboy == false
        }

    @test
    def testStructMultiPut03(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_fstName = "Unlucky";
            s€_fstName = "Lucky";
            s€_lstName = "Uriel";
            s€_lstName = "Luke";
            s€_lstName == "Luke" and s€_fstName == "Lucky" and s€_age == 42 and s€_cowboy == true
        }

    @test
    def testStructMultiPut04(): Bool =
        region rc {
            let s = new Put { _fstName = "Lucky", _lstName = "Luke", _age = 42, _cowboy = true } @ rc;
            s€_lstName = "Uriel";
            s€_fstName = "Unlucky";
            s€_lstName = "Luke";
            s€_fstName = "Lucky";
            s€_fstName == "Lucky" and s€_lstName == "Luke" and s€_age == 42 and s€_cowboy == true
        }

/* Cannot test these while struct name is required to be mod name
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
        */
}
