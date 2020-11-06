namespace Test01 {

    class X[a]
}

namespace Test02 {

    class X[a] {
        def f(x: a): Bool
    }

}

namespace Test03 {
    class X[a] {
        def f(x: a): Bool
    }

    instance X[Int] {
        def f(x: Int): Bool = x == 0
    }

    pub def g(x: Int): Bool = f(x)
}

namespace Test04 {
    class C[a] {
        def f(x: a): Bool
    }

    class D[a] {
        def g(x: a): Bool
    }

    pub def h[a : C : D](x: a): Bool = f(x) && g(x)
}

namespace Test05 {
    class C[a]

    instance C[List[a]] with [a : C, Option[a] : C]
}

namespace Test06 {
    pub class C[a] {
        def f(x: a): Bool
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

    pub def g(x: List[Int]): Bool = f(x)
}

namespace Test07 {
    use class Test06.C;

    pub def g[a: C](x: a): a = x
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
