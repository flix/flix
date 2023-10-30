// TODO this module is replicated by `Test.Dec.Trait.flix` for the `trait` keyword.
// Remove this module at the end of the transition period from `class` to `trait`.
mod Test.Dec.Class {
    mod Test01 {

        class X[a]
    }

    mod Test02 {

        class X[a] {
            pub def f(x: a): Bool
        }

    }

    mod Test03 {
        class X[a] {
            pub def f(x: a): Bool
        }

        instance X[Int32] {
            pub def f(x: Int32): Bool = x == 0
        }

        pub def g(x: Int32): Bool = Test.Dec.Class.Test03.X.f(x)
    }

    mod Test04 {
        class C[a] {
            pub def f(x: a): Bool
        }

        class D[a] {
            pub def g(x: a): Bool
        }

        pub def h(x: a): Bool with C[a], D[a] = Test.Dec.Class.Test04.C.f(x) and Test.Dec.Class.Test04.D.g(x)
    }

    mod Test05 {

        class C[a]

        instance C[List[a]] with C[a]
    }

    mod Test06 {

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

        pub def g(x: List[Int32]): Bool = Test.Dec.Class.Test06.C.f(x)
    }

    mod Test07 {
        use Test.Dec.Class.Test06.C

        pub def g(x: a): a with C[a] = x
    }

    mod Test08 {

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

    mod Test09 {
        class Eff[ef : Eff] {
            pub def isPure(f: a -> b \ ef): Bool
        }

        instance Eff[Pure] {
            pub def isPure(_f: a -> b): Bool = true
        }
    }

    mod Test10 {
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

        pub def g(x: a): Bool with D[a] = Test.Dec.Class.Test10.C.f(x)

        pub def h(x: a): Bool with E[a] = Test.Dec.Class.Test10.C.f(x)
    }

    mod Test11 {
        lawful class C[a] {
            pub def f(x: a, y: a): Bool

            // TODO handle mods better
            law reflexivity: forall (x: a, y: a) . Test.Dec.Class.Test11.C.f(x, y) == Test.Dec.Class.Test11.C.f(y, x)
        }

        instance C[Int32] {
            pub def f(_x: Int32, _y: Int32): Bool = true
        }
    }

    mod Test12 {
        class C[a] {
            pub def f(): a

            law l: forall (_x: a) . true

            pub def g(): a

            law m: forall[a: Type](_x: a) . true
        }
    }

    mod Test13 {
        class C[a] {
            pub def f(): a

            pub def g(): a = Test.Dec.Class.Test13.C.f()

            pub def h(): a = Test.Dec.Class.Test13.C.g()
        }

        instance C[Int32] {
            pub def f(): Int32 = 0

            override pub def h(): Int32 = 1
        }

        @test
        def testSigOverride01(): Bool = Test.Dec.Class.Test13.C.f() == 0

        @test
        def testSigOverride02(): Bool = Test.Dec.Class.Test13.C.g() == 0

        @test
        def testSigOverride03(): Bool = Test.Dec.Class.Test13.C.h() == 1
    }

    mod Test14 {
        class C[a] {
            pub def c(x: a): Int32
        }

        class D[a] with C[a] {
            pub def d(x: a): Int32
        }

        pub def f(x: a): Int32 with D[a] = Test.Dec.Class.Test14.C.c(x) + Test.Dec.Class.Test14.D.d(x)
    }

    mod Test15 {
        class C[a]

        class D[a]

        pub def f(x: a, y: b): Bool with C[a], D[b] = ???
    }

    mod Test16 {
        class D[a] {
            pub def f(x: a, y: a): Bool
        }

        class C[a] {
            law l: forall (x: a, y: a) with D[a] . Test.Dec.Class.Test16.D.f(x, y)
        }
    }

    mod Test17 {
        class D[a] {
            pub def f(x: a): Bool
        }

        class E[a: Type -> Type] {
            law l: forall (x: a[b]) with D[a[b]] . Test.Dec.Class.Test17.D.f(x)
        }
    }

    @test
    def empty(): Bool = true
}
