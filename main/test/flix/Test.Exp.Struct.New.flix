mod Mod1 {
    pub struct S[r] {}
    struct S2[r] {}

    mod Mod2 {
        @test
        def testOuterMod01(): Unit =
            region rc {
                new Mod1.S2 @ rc {};
                ()
            }
    }
}

mod Test.Exp.Struct.New {

    struct Empty[r] { }

    struct OneEmpty[r] {
        a: Empty[r]
    }

    struct TwoEmpty[r] {
        two_a: Empty[r],
        two_b: Empty[r]
    }

    struct EmptyC[r] {
        c: Empty[r]
    }

    struct EmptyB[r] {
        b: EmptyC[r]
    }

    struct EmptyA[r] {
        emptyb: EmptyB[r]
    }

    struct A[r] {
        bstruct: B[r]
    }

    struct B[r] {
        cstruct: C[r]
    }

    struct C[r] {
        dstruct: D[r]
    }

    struct D[r] {
        i: Int32
    }

    struct Name[r] {
        name: String
    }

    struct FullName[r] {
        fstName: String,
        lstName: String
    }

    struct Person[r] {
        person_fstName: String,
        person_lstName: String,
        person_age: Int32
    }

    struct Cowboy[r] {
        cowboy_fstName: String,
        cowboy_lstName: String,
        cowboy_age: Int32,
        cowboy: Bool
    }

    struct TwoInts[r] {
        x: Int32,
        y: Int32
    }

    struct ThreeInts[r] {
        three_x: Int32,
        three_y: Int32,
        three_z: Int32
    }

    struct Outer[r] {
        inner1: TwoInts[r],
        inner2: TwoInts[r]
    }

    struct TwoIntsHolder[r] {
        twoints: TwoInts[r]
    }

    struct A3[r] {
        a2: A2[r]
    }

    struct A2[r] {
        a1: A1[r]
    }

    struct A1[r] {
        empty: Empty[r]
    }

    struct BinaryTree[t, r] {
        left: Option[BinaryTree[t, r]],
        right: Option[BinaryTree[t, r]],
        value: t
    }

    @test
    def testEmptyStruct01(): Unit =
        region rc {
            new Empty @ rc {};
            ()
        }

    @test
    def testEmptyStruct02(): Unit =
        region rc {
            new OneEmpty @ rc {a = new Empty @ rc { }};
            ()
        }

    @test
    def testEmptyStruct03(): Unit =
        region rc {
            new TwoEmpty @ rc {two_a = new Empty @ rc {}, two_b = new Empty @ rc {}};
            ()
        }

    @test
    def testEmptyStruct04(): Unit =
        region rc {
            new EmptyA @ rc { emptyb = new EmptyB @ rc { b = new EmptyC @ rc { c = new Empty @ rc {} } } };
            ()
        }

    @test
    def testStructLiteral01(): Unit =
        region rc {
            new Name @ rc { name = "Lucky Luke" };
            ()
        }

    @test
    def testStructLiteral02(): Unit =
        region rc {
            new FullName @ rc { fstName = "Lucky", lstName = "Luke" };
            ()
        }

    @test
    def testStructLiteral03(): Unit =
        region rc {
            new Person @ rc { person_fstName = "Lucky", person_lstName = "Luke", person_age = 42 };
            ()
        }

    @test
    def testStructLiteral04(): Unit =
        region rc {
            new Cowboy @ rc { cowboy_fstName = "Lucky", cowboy_lstName = "Luke", cowboy_age = 42, cowboy = true };
            ()
        }

    @test
    def testStructLiteral05(): Unit =
        region rc {
            new TwoInts @ rc { x = 0, y = 0 };
            ()
        }

    @test
    def testStructLiteral06(): Unit =
        region rc {
            new ThreeInts @ rc { three_x = 0, three_y = 0, three_z = 0 };
            ()
        }

    @test
    def testNestedStructLiteral01(): Unit =
        region rc {
            new TwoIntsHolder @ rc { twoints = new TwoInts @ rc { x = 1, y = 2 }};
            ()
        }

    @test
    def testNestedStructLiteral02(): Unit =
        region rc {
            new Outer @ rc { inner1 = new TwoInts @ rc { x = 1, y = 2 }, inner2 = new TwoInts @ rc { x = 3, y = 4 } };
            ()
        }

    @test
    def testNestedStructLiteral03(): Unit =
        region rc {
            new A @ rc {bstruct = new B @ rc {cstruct = new C @ rc {dstruct = new D @ rc {i = 42}}}};
            ()
        }

    @test
    def testNestedLabels01(): Unit =
        region rc {
            new A3 @ rc {a2 = new A2 @ rc {a1 = new A1 @ rc {empty = new Empty @ rc {}}}};
            ()
        }

    @test
    def testOtherMod01(): Unit =
        region rc {
            new Mod1.S @ rc {};
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
    def testVariableNameSameAsFieldName01(): Unit =
        let left = ();
        left
}
