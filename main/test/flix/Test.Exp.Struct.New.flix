mod Mod1 {
    pub struct S {}
    struct S2 {}

    mod Mod2 {
        @test
        pub def testOuterMod01(): Unit = {
            let _ = new Mod1.S2 $  {};
            ()
        }
    }
}

mod Test.Exp.Struct.New {

    struct Empty { }

    struct OneEmpty {
        a: Empty
    }

    struct TwoEmpty {
        two_a: Empty,
        two_b: Empty
    }

    struct EmptyC {
        c: Empty
    }

    struct EmptyB {
        b: EmptyC
    }

    struct EmptyA {
        emptyb: EmptyB
    }

    struct A {
        bstruct: B
    }

    struct B {
        cstruct: C
    }

    struct C {
        dstruct: D
    }

    struct D {
        i: Int32
    }

    struct Name {
        name: String
    }

    struct FullName {
        fstName: String,
        lstName: String
    }

    struct Person {
        person_fstName: String,
        person_lstName: String,
        person_age: Int32
    }

    struct Cowboy {
        cowboy_fstName: String,
        cowboy_lstName: String,
        cowboy_age: Int32,
        cowboy: Bool
    }

    struct TwoInts {
        x: Int32,
        y: Int32
    }

    struct ThreeInts {
        three_x: Int32,
        three_y: Int32,
        three_z: Int32
    }

    struct Outer {
        inner1: TwoInts,
        inner2: TwoInts
    }

    struct TwoIntsHolder {
        twoints: TwoInts
    }

    struct A3 {
        a2: A2
    }

    struct A2 {
        a1: A1
    }

    struct A1 {
        empty: Empty
    }

    struct BinaryTree[t: Type, r: Region] {
        left: Option[BinaryTree[t, r]],
        right: Option[BinaryTree[t, r]],
        value: t
    }

    @test
    def testEmptyStruct01(): Unit = {
        let _ = new Empty $ {};
        ()
    }

    @test
    def testEmptyStruct02(): Unit = {
        let _ = new OneEmpty $ {a = new Empty $ { }};
        ()
    }

    @test
    def testEmptyStruct03(): Unit = {
        let _ = new TwoEmpty $ {two_a = new Empty $ {}, two_b = new Empty $ {}};
        ()
    }

    @test
    def testEmptyStruct04(): Unit = {
        let _ = new TwoEmpty $ {two_b = new Empty $ {}, two_a = new Empty $ {}};
        ()
    }

    @test
    def testEmptyStruct05(): Unit = {
        let _ = new EmptyA $ { emptyb = new EmptyB $ { b = new EmptyC $ { c = new Empty $ {} } } };
        ()
    }

    @test
    def testStructLiteral01(): Unit = {
        let _ = new Name $ { name = "Lucky Luke" };
        ()
    }

    @test
    def testStructLiteral02(): Unit = {
        let _ = new FullName $ { fstName = "Lucky", lstName = "Luke" };
        ()
    }

    @test
    def testStructLiteral03(): Unit = {
        let _ = new Person $ { person_fstName = "Lucky", person_lstName = "Luke", person_age = 42 };
        ()
    }

    @test
    def testStructLiteral04(): Unit = {
        let _ = new Cowboy $ { cowboy_fstName = "Lucky", cowboy_lstName = "Luke", cowboy_age = 42, cowboy = true };
        ()
    }

    @test
    def testStructLiteral05(): Unit = {
        let _ = new Cowboy $ { cowboy = true, cowboy_lstName = "Luke", cowboy_age = 42, cowboy_fstName = "Lucky" };
        ()
    }

    @test
    def testStructLiteral06(): Unit = {
        let _ = new TwoInts $ { x = 0, y = 0 };
        ()
    }

    @test
    def testStructLiteral07(): Unit = {
        let _ = new ThreeInts $ { three_x = 0, three_y = 0, three_z = 0 };
        ()
    }

    @test
    def testStructLiteral08(): Unit = {
        let _ = new ThreeInts $ { three_y = 0, three_x = 0, three_z = 0 };
        ()
    }

    @test
    def testNestedStructLiteral01(): Unit = {
        let _ = new TwoIntsHolder $ { twoints = new TwoInts $ { x = 1, y = 2 }};
        ()
    }

    @test
    def testNestedStructLiteral02(): Unit = {
        let _ = new Outer $ { inner1 = new TwoInts $ { x = 1, y = 2 }, inner2 = new TwoInts $ { x = 3, y = 4 } };
        ()
    }

    @test
    def testNestedStructLiteral03(): Unit = {
        let _ = new A $ {bstruct = new B $ {cstruct = new C $ {dstruct = new D $ {i = 42}}}};
        ()
    }

    @test
    def testNestedLabels01(): Unit = {
        let _ = new A3 $ {a2 = new A2 $ {a1 = new A1 $ {empty = new Empty $ {}}}};
        ()
    }

    @test
    def testOtherMod01(): Unit = {
        let _ = new Mod1.S $ {};
        ()
    }

    @test
    def binaryTree01(): Unit =
        region rc {
            new BinaryTree @ rc {
                left = None,
                right = None,
                value = 3
            };
            ()
        }

    @test
    def binaryTree02(): Unit =
        region rc {
            let leaf = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 3
            };
            new BinaryTree @ rc {
                left = Some(leaf),
                right = Some(leaf),
                value = 3
            };
            ()
        }

    @test
    def binaryTree03(): Unit =
        region rc {
            let leaf = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 3
            };
            let innernode = new BinaryTree @ rc {
                left = Some(leaf),
                right = Some(leaf),
                value = 3
            };
            new BinaryTree @ rc {
                left = Some(innernode),
                right = Some(innernode),
                value = 3
            };
            ()
        }

    @test
    def binaryTree04(): Unit =
        region rc {
            let leaf = new BinaryTree @ rc {
                right = None,
                left = None,
                value = 3
            };
            let innernode = new BinaryTree @ rc {
                left = Some(leaf),
                value = 3,
                right = Some(leaf)
            };
            new BinaryTree @ rc {
                value = 3,
                right = Some(innernode),
                left = Some(innernode)
            };
            ()
        }

    @test
    def testVariableNameSameAsFieldName01(): Unit =
        let left = ();
        left
}
