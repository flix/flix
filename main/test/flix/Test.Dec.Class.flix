namespace Test/Dec/Class {
    namespace Test01 {

        class X[a]
    }

    namespace Test02 {

        class X[a] {
            pub def f(x: a): Bool
        }

    }

    namespace Test03 {
        class X[a] {
            pub def f(x: a): Bool
        }

        instance X[Int32] {
            pub def f(x: Int32): Bool = x == 0
        }

        pub def g(x: Int32): Bool = Test/Dec/Class/Test03/X.f(x)
    }

    namespace Test04 {
        class C[a] {
            pub def f(x: a): Bool
        }

        class D[a] {
            pub def g(x: a): Bool
        }

        pub def h(x: a): Bool with C[a], D[a] = Test/Dec/Class/Test04/C.f(x) and Test/Dec/Class/Test04/D.g(x)
    }

    namespace Test05 {

        class C[a]

        instance C[List[a]] with C[a]
    }

    namespace Test06 {

        pub class C[a] {
            pub def f(x: a): Bool
        }

        instance C[Int32] {
            pub def f(x: Int32): Bool = x == 0
        }

        instance C[List[a]] with C[a] {
            pub def f(x: List[a]): Bool = {
                match x {
                    case Nil => true
                    case _ => false
                }
            }
        }

        pub def g(x: List[Int32]): Bool = Test/Dec/Class/Test06/C.f(x)
    }

    namespace Test07 {
        use Test/Dec/Class/Test06.C;

        pub def g(x: a): a with C[a] = x
    }

    namespace Test08 {

        class F[m : Type -> Type] {
            pub def map(f: a -> b, x: m[a]): m[b]
        }

        instance F[Option] {
            pub def map(f: a -> b, x: Option[a]): Option[b] = match x {
                case None => None
                case Some(y) => Some(f(y))
            }
        }
    }

    namespace Test09 {
        class Eff[e : Bool] {
            pub def isPure(f: a -> b & e): Bool
        }

        instance Eff[Pure] {
            pub def isPure(_f: a -> b): Bool = true
        }
    }

    namespace Test10 {
        class C[a] {
            pub def f(x: a): Bool
        }

        class D[a] with C[a]

        class E[a] with D[a]

        instance C[Int32] {
            pub def f(_x: Int32): Bool = true
        }

        instance D[Int32]

        instance E[Int32]

        pub def g(x: a): Bool with D[a] = Test/Dec/Class/Test10/C.f(x)

        pub def h(x: a): Bool with E[a] = Test/Dec/Class/Test10/C.f(x)
    }

    namespace Test11 {
        lawful class C[a] {
            pub def f(x: a, y: a): Bool

            // TODO handle namespaces better
            law reflexivity: forall (x: a, y: a) . Test/Dec/Class/Test11/C.f(x, y) == Test/Dec/Class/Test11/C.f(y, x)
        }

        instance C[Int32] {
            pub def f(_x: Int32, _y: Int32): Bool = true
        }
    }

    namespace Test12 {
        class C[a] {
            pub def f(): a

            law l: forall (_x: a) . true

            pub def g(): a

            law m: forall[a: Type](_x: a) . true
        }
    }

    namespace Test13 {
        class C[a] {
            pub def f(): a

            pub def g(): a = Test/Dec/Class/Test13/C.f()

            pub def h(): a = Test/Dec/Class/Test13/C.g()
        }

        instance C[Int32] {
            pub def f(): Int32 = 0

            override pub def h(): Int32 = 1
        }

        @test
        def testSigOverride01(): Bool = Test/Dec/Class/Test13/C.f() == 0

        @test
        def testSigOverride02(): Bool = Test/Dec/Class/Test13/C.g() == 0

        @test
        def testSigOverride03(): Bool = Test/Dec/Class/Test13/C.h() == 1
    }

    namespace Test14 {
        class C[a] {
            pub def c(x: a): Int32
        }

        class D[a] with C[a] {
            pub def d(x: a): Int32
        }

        pub def f(x: a): Int32 with D[a] = Test/Dec/Class/Test14/C.c(x) + Test/Dec/Class/Test14/D.d(x)
    }

    namespace Test15 {
        class C[a]

        class D[a]

        pub def f(x: a, y: b): Bool with C[a], D[b] = ???
    }

    namespace Test16 {
        class D[a] {
            pub def f(x: a, y: a): Bool
        }

        class C[a] {
            law l: forall (x: a, y: a) with D[a] . Test/Dec/Class/Test16/D.f(x, y)
        }
    }

    namespace Test17 {
        class D[a] {
            pub def f(x: a): Bool
        }

        class E[a: Type -> Type] {
            law l: forall (x: a[b]) with D[a[b]] . Test/Dec/Class/Test17/D.f(x)
        }
    }

    namespace Test18 {
        pub enum E[_: Bool]
        pub enum F[_: Bool, _: Type]

        class C[a]
        class D[a: Type -> Type]

        instance C[E[true]]
        instance C[F[false, a]]
        instance D[F[true]]
    }

    namespace Test19 {
        pub enum M[_: Bool, a: Type] {
            case M(a)
        }

        instance Eq[M[true, a]] with Eq[a] {
            pub def eq(x: M[true, a], y: M[true, a]): Bool = match (x, y) {
                case (M(x1), M(y1)) => x1 == y1
            }
        }

        @test
        def testCallComplexBoolSig01(): Bool = M(10): M[true, Int32] == M(10): M[true, Int32]

        @test
        def testCallComplexBoolSig02(): Bool = M(10): M[true, Int32] != M(11): M[true, Int32]
    }

    @test
    def empty(): Bool = true
}
