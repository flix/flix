mod Mod1 {
    pub struct S [r] {}
    struct S2 [r] {}

    mod Mod2 {
        @test
        def foo(): Unit =
            region rc {
                new Mod1.S2 {} @ rc;
                ()
            }
    }
}

mod Test.Exp.Struct.New {

    struct Empty [r] { }

    struct OneEmpty [r] {
        a: Empty[r]
    }

    struct TwoEmpty [r] {
        a: Empty[r],
        b: Empty[r]
    }

    struct EmptyC [r] {
        c: Empty[r]
    }

    struct EmptyB [r] {
        b: EmptyC[r]
    }

    struct EmptyA [r] {
        a: EmptyB[r]
    }

    struct A [r] {
        a: B[r]
    }

    struct B [r] {
        b: C[r]
    }

    struct C [r] {
        c: D[r]
    }

    struct D [r] {
        d: Int32
    }

    struct Name [r] {
        name: String
    }

    struct FullName [r] {
        fstName: String,
        lstName: String
    }

    struct Person [r] {
        fstName: String,
        lstName: String,
        age: Int32
    }

    struct Cowboy [r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    struct TwoInts [r] {
        x: Int32,
        y: Int32
    }

    struct ThreeInts [r] {
        x: Int32,
        y: Int32,
        z: Int32
    }

    struct Outer [r] {
        a: TwoInts[r],
        b: TwoInts[r]
    }

    struct TwoIntsHolder [r] {
        a: TwoInts[r]
    }

    struct A3 [r] {
        a: A2[r]
    }

    struct A2 [r] {
        a: A1[r]
    }

    struct A1 [r] {
        a: Empty[r]
    }

    @test
    def testEmptyStruct01(): Unit =
        region rc {
            new Empty {} @ rc;
            ()
        }

    @test
    def testEmptyStruct02(): Unit =
        region rc {
            new OneEmpty {a = new Empty { } @ rc} @ rc;
            ()
        }

    @test
    def testEmptyStruct03(): Unit =
        region rc {
            new TwoEmpty {a = new Empty {} @ rc, b = new Empty {} @ rc} @ rc;
            ()
        }

    @test
    def testEmptyStruct04(): Unit =
        region rc {
            new EmptyA { a = new EmptyB { b = new EmptyC { c = new Empty {} @ rc } @ rc } @ rc } @ rc;
            ()
        }

    @test
    def testStructLiteral01(): Unit =
        region rc {
            new Name { name = "Lucky Luke" } @ rc;
            ()
        }

    @test
    def testStructLiteral02(): Unit =
        region rc {
            new FullName { fstName = "Lucky", lstName = "Luke" } @ rc;
            ()
        }

    @test
    def testStructLiteral03(): Unit =
        region rc {
            new Person { fstName = "Lucky", lstName = "Luke", age = 42 } @ rc;
            ()
        }

    @test
    def testStructLiteral04(): Unit =
        region rc {
            new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc;
            ()
        }

    @test
    def testStructLiteral05(): Unit =
        region rc {
            new TwoInts { x = 0, y = 0 } @ rc;
            ()
        }

    @test
    def testStructLiteral06(): Unit =
        region rc {
            new ThreeInts { x = 0, y = 0, z = 0 } @ rc;
            ()
        }

    @test
    def testStructLiteralOrder01(): Unit =
        region rc {
            new ThreeInts { x = 1, y = 2, z = 3 } @ rc;
            ()
        }

    @test
    def testStructLiteralOrder02(): Unit =
        region rc {
            new ThreeInts { y = 2, z = 3, x = 1 } @ rc;
            ()
        }

    @test
    def testStructLiteralOrder03(): Unit =
        region rc {
            new ThreeInts { z = 3, x = 1, y = 2 } @ rc;
            ()
        }

    @test
    def testNestedStructLiteral01(): Unit =
        region rc {
            new TwoIntsHolder { a = new TwoInts { x = 1, y = 2 } @ rc} @ rc;
            ()
        }

    @test
    def testNestedStructLiteral02(): Unit =
        region rc {
            new Outer { a = new TwoInts { x = 1, y = 2 } @ rc, b = new TwoInts { x = 3, y = 4 } @ rc } @ rc;
            ()
        }

    @test
    def testNestedStructLiteral03(): Unit =
        region rc {
            new A {a = new B {b = new C {c = new D {d = 42} @ rc} @ rc} @ rc} @ rc;
            ()
        }

    @test
    def testNestedLabels01(): Unit =
        region rc {
            new A3 {a = new A2 {a = new A1 {a = new Empty {} @ rc} @ rc} @ rc} @ rc;
            ()
        }

    @test
    def testOtherMod01(): Unit =
        region rc {
            new Mod1.S {} @ rc;
            ()
        }
}
