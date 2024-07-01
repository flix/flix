mod Test.Exp.Struct.New {

    struct Empty [r] { }

    struct OneEmpty [r] {
        a: Empty
    }

    struct TwoEmpty [r] {
        a: Empty,
        b: Empty
    }

    struct EmptyC [r] {
        c: Empty
    }

    struct EmptyB [r] {
        b: EmptyC
    }

    struct EmptyA [r] {
        a: EmptyB
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

    struct Name [r] {
        name: String
    }

    struct FullName [r] {
        fstName: String,
        lstName: String
    }

    struct Person [r] {
        fstName: String,
        lstName: String
        age: Int32
    }

    struct Cowboy [r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    struct OneInt [r] {
        x: Int32
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
        a: TwoInts[r]
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
    def testEmptyStruct01(): Empty[r] =
        region rc {
            new Empty {} @ rc
        }

    @test
    def testEmptyStruct02(): OneEmpty[r] =
        region rc {
            new OneEmpty {a = new Empty { } @ rc} @ rc
        }

    @test
    def testEmptyStruct03(): TwoEmpty[r] =
        region rc {
            new TwoEmpty {a = new Empty {} @ rc, b = new Empty {} @ rc} @ rc
        }

    @test
    def testEmptyStruct04(): EmptyA[r] =
        region rc {
            new EmptyA { a = new EmptyB { b = new EmptyC { c = new Empty {} @ rc } @ rc } @ rc } @ rc
        }

    @test
    def testStructLiteral01(): Name[r] =
        region rc {
            new Name { name = "Lucky Luke" } @ rc
        }

    @test
    def testStructLiteral02(): FullName[r] =
        region rc {
            new FullName { fstName = "Lucky", lstName = "Luke" } @ rc
        }

    @test
    def testStructLiteral03(): Person[r] =
        region rc {
            new Person { fstName = "Lucky", lstName = "Luke", age = 42 } @ rc
        }

    @test
    def testStructLiteral04(): Cowboy[r] =
        region rc {
            new Cowboy { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true } @ rc
        }

    @test
    def testStructLiteral05(): TwoInts[r] =
        region rc {
            new TwoInts { x = 0, y = 0 } @ rc
        }

    @test
    def testStructLiteral06(): ThreeInts[r] =
        region rc {
            new ThreeInts { x = 0, y = 0, z = 0 } @ rc
        }

    @test
    def testStructLiteralOrder01(): ThreeInts[r] =
        region rc {
            new ThreeInts { x = 1, y = 2, z = 3 } @ rc
        }

    @test
    def testStructLiteralOrder02(): ThreeInts[r] =
        region rc {
            new ThreeInts { y = 2, z = 3, x = 1 } @ rc
        }

    @test
    def testStructLiteralOrder03(): ThreeInts[r] =
        region rc {
            new ThreeInts { z = 3, x = 1, y = 2 } @rc
        }

    @test
    def testNestedStructLiteral01(): TwoIntsHolder[r] =
        region rc {
            new TwoIntsHolder { a = new TwoInts { x = 1, y = 2 } @ rc} @ rc
        }

    @test
    def testNestedStructLiteral02(): Outer[r] =
        region rc {
            new Outer { a = new Inner { x = 1, y = 2 } @ rc, b = new Inner { x = 3, y = 4 } @ rc } @ rc
        }

    @test
    def testNestedStructLiteral03(): A[r] =
        region rc {
            new A {a = new B {b = new C {c = new D {d = 42} @ rc} @ rc} @ rc} @ rc
        }

    @test
    def testNestedLabels01(): A3[r] =
        region rc {
            new A3 {a = new A2 {a = new A1{a = new Empty {} @ rc} @ rc} @ rc} @ rc
        }
}
