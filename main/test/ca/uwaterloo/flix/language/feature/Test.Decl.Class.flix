namespace Test01 {

    trait X[a]

    trait Y[a, b]

    trait Z[a, b, c]

}

namespace Test02 {

    trait X[a] {
        def f(x: a): Bool
    }

//    trait Y[a, b] {
//        def f(x: a): Bool
//        def g(x: a, y: b): Bool
//    }
//
//    trait Z[a, b, c] {
//        def f(x: a): Bool
//        def g(x: a, y: b): Bool
//        def h(x: a, y: b, z: c): Bool
//    }

}
//
//namespace Test03 {
//
//    class X[a] {
//        def f(x: a): a
//    }
//
//    class Y[a, b] {
//        def f(x: a): a
//        def g(x: a, y: b): b
//    }
//
//    class Z[a, b, c] {
//        def f(x: a): a
//        def g(x: a, y: b): b
//        def h(x: a, y: b, z: c): c
//    }
//
//}
//
//namespace Test04 {
//
//    class X[a]
//
//    class Y[a]
//
//    class Z[a]
//
//    class M[a] <= X[a]
//
//    class N[a] <= X[a], Y[a]
//
//    class O[a] <= X[a], Y[a], Z[a]
//
//}
//
//namespace Test05 {
//
//    class W[a, b, c] {
//        def f[d, e, f](x: a, y: f): Bool
//    }
//
//}
//
//namespace Test06 {
//
//    class X[a] {
//        law f(): Bool = ???
//        law g(h: a -> a): Bool = ???
//    }
//
//}
//
//namespace Test07 {
//
//    namespace A {
//        class X[a] {
//            def f(x: a): Bool
//        }
//    }
//
//    namespace B {
//        class X[a] {
//            def f(x: a): Bool
//        }
//    }
//
//    namespace A/B {
//        class X[a] {
//            def f(x: a): Bool
//        }
//    }
//
//    namespace A/B/C {
//        class X[a] {
//            def f(x: a): Bool
//        }
//    }
//
//}
//
//namespace Test08 {
//
//    namespace A {
//        pub class X[a]
//    }
//
//    namespace B {
//        pub class Y[a] <= Test08/A.X[a]
//    }
//
//    namespace C {
//        class Z[a] <= Test08/A.X[a], Test08/B.Y[a]
//    }
//
//}
//
@test
def empty(): Bool = true
