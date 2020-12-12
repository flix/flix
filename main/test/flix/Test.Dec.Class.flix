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

        instance X[Int] {
            def f(x: Int): Bool = x == 0
        }

        pub def g(x: Int): Bool = Test/Dec/Class/Test03/X.f(x)
    }

    namespace Test04 {
        class C[a] {
            pub def f(x: a): Bool
        }

        class D[a] {
            pub def g(x: a): Bool
        }

        pub def h[a : C : D](x: a): Bool = Test/Dec/Class/Test04/C.f(x) and Test/Dec/Class/Test04/D.g(x)
    }

    namespace Test05 {
        use Test/Dec/Class.List;
        use Test/Dec/Class.Option;

        class C[a]

        instance C[List[a]] with [a : C, Option[a] : C]
    }

    namespace Test06 {
        use Test/Dec/Class.List;
        use Test/Dec/Class.List.Nil;
        use Test/Dec/Class.Option;

        pub class C[a] {
            pub def f(x: a): Bool
        }

        instance C[Int] {
            def f(x: Int): Bool = x == 0
        }

        instance C[List[a]] with [a: C] {
            def f(x: List[a]): Bool = {
                match x {
                    case Nil => true
                    case _ => false
                }
            }
        }

        pub def g(x: List[Int]): Bool = Test/Dec/Class/Test06/C.f(x)
    }

    namespace Test07 {
        use class Test/Dec/Class/Test06.C;

        pub def g[a: C](x: a): a = x
    }

    namespace Test08 {
        use Test/Dec/Class.Option;
        use Test/Dec/Class.Option.None;
        use Test/Dec/Class.Option.Some;

        class F[m :# Type -> Type] {
            pub def map(f: a -> b, x: m[a]): m[b]
        }

        instance F[Option] {
            def map(f: a -> b, x: Option[a]): Option[b] = match x {
                case None => None
                case Some(y) => Some(f(y))
            }
        }
    }

    namespace Test09 {
        class Eff[e :# Bool] {
            pub def isPure(f: a -> b & e): Bool
        }

        instance Eff[Pure] {
            def isPure(f: a -> b): Bool = true
        }

        instance Eff[Impure] {
            def isPure(f: a ~> b): Bool = false
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