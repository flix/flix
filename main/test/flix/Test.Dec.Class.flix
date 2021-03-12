namespace Test/Dec/Class {
    namespace Test01 {

        class X[a]
    }

    namespace Test02 {

        lawless class X[a] {
            pub def f(x: a): Bool
        }

    }

    namespace Test03 {
        lawless class X[a] {
            pub def f(x: a): Bool
        }

        instance X[Int] {
            pub def f(x: Int): Bool = x == 0
        }

        pub def g(x: Int): Bool = Test/Dec/Class/Test03/X.f(x)
    }

    namespace Test04 {
        lawless class C[a] {
            pub def f(x: a): Bool
        }

        lawless class D[a] {
            pub def g(x: a): Bool
        }

        pub def h[a : C : D](x: a): Bool = Test/Dec/Class/Test04/C.f(x) and Test/Dec/Class/Test04/D.g(x)
    }

    namespace Test05 {
        use Test/Dec/Class.List;

        class C[a]

        instance C[List[a]] with [a : C]
    }

    namespace Test06 {
        use Test/Dec/Class.List;
        use Test/Dec/Class.List.Nil;
        use Test/Dec/Class.Option;

        pub lawless class C[a] {
            pub def f(x: a): Bool
        }

        instance C[Int] {
            pub def f(x: Int): Bool = x == 0
        }

        instance C[List[a]] with [a: C] {
            pub def f(x: List[a]): Bool = {
                match x {
                    case Nil => true
                    case _ => false
                }
            }
        }

        pub def g(x: List[Int]): Bool = Test/Dec/Class/Test06/C.f(x)
    }

    namespace Test07 {
        use Test/Dec/Class/Test06.C;

        pub def g[a: C](x: a): a = x
    }

    namespace Test08 {
        use Test/Dec/Class.Option;
        use Test/Dec/Class.Option.None;
        use Test/Dec/Class.Option.Some;

        lawless class F[m :# Type -> Type] {
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
        lawless class Eff[e :# Bool] {
            pub def isPure(f: a -> b & e): Bool
        }

        instance Eff[Pure] {
            pub def isPure(f: a -> b): Bool = true
        }

        instance Eff[Impure] {
            pub def isPure(f: a ~> b): Bool = false
        }
    }

    namespace Test10 {
        lawless class C[a] {
            pub def f(x: a): Bool
        }

        lawless class D[a] with C[a]

        lawless class E[a] with D[a]

        instance C[Int] {
            pub def f(_x: Int): Bool = true
        }

        instance D[Int]

        instance E[Int]

        pub def g[a : D](x: a): Bool = Test/Dec/Class/Test10/C.f(x)

        pub def h[a : E](x: a): Bool = Test/Dec/Class/Test10/C.f(x)
    }

    namespace Test11 {
        class C[a] {
            pub def f(x: a, y: a): Bool

            // TODO handle namespaces better
            law reflexivity(): Bool = forall (x: a, y: a) . Test/Dec/Class/Test11/C.f(x, y) == Test/Dec/Class/Test11/C.f(y, x)
        }

        instance C[Int] {
            pub def f(_x: Int, _y: Int): Bool = true
        }
    }

    namespace Test12 {
        lawless class C[a] {
            pub def f(): a

            law l(): Bool = true

            pub def g(): a

            law m(): Bool = true
        }
    }

    pub enum List[a] {
        case Nil
        case Cons(a, List[a])
    }

    pub enum Option[a] {
        case None
        case Some(a)
    }

    @test
    def empty(): Bool = true
}