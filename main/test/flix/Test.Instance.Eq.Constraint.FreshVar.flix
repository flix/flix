mod Test.Instance.Eq.Constraint.FreshVar {

    use Assert.assertEq

    trait Container[c] {
        type Elm: Type
        pub def first(c: c): Container.Elm[c]
    }

    instance Container[List[a]] {
        type Elm = a
        pub def first(c: List[a]): a = match c {
            case hd :: _ => hd
            case Nil     => unreachable!()
        }
    }

    // `Wrapper[c]` has no `a` in its head; the type variable `a` is introduced
    // solely by the right-hand side of the equality constraint
    // `Container.Elm[c] ~ a`, where it names the element type of `c`.
    //
    // The associated type is defined directly as `a`. Reducing
    // `Container.Elm[Wrapper[List[Int32]]]` must therefore consult the equality
    // constraint and yield `Int32`, rather than a loose type variable.
    enum Wrapper[c](c)

    instance Container[Wrapper[c]] with Container[c] where Container.Elm[c] ~ a {
        type Elm = a
        pub def first(w: Wrapper[c]): a =
            let Wrapper.Wrapper(inner) = w;
            Container.first(inner)
    }

    // The introduced variable may appear inside a compound associated type.
    enum Twice[c](c, c)

    instance Container[Twice[c]] with Container[c] where Container.Elm[c] ~ a {
        type Elm = (a, a)
        pub def first(t: Twice[c]): (a, a) =
            let Twice.Twice(x, y) = t;
            (Container.first(x), Container.first(y))
    }

    // -----------------------------------------------------------------
    // A compound right-hand side introduces several variables at once and
    // effectively destructures the associated type of the wrapped value.

    trait Producer[p] {
        type Elm: Type
        pub def produce(p: p): Producer.Elm[p]
    }

    enum Pair[a, b](a, b)

    instance Producer[Pair[a, b]] {
        type Elm = (a, b)
        pub def produce(p: Pair[a, b]): (a, b) =
            let Pair.Pair(x, y) = p;
            (x, y)
    }

    enum FirstOf[p](p)

    // `Producer.Elm[p] ~ (a, b)` introduces both `a` and `b`; the element type
    // of `FirstOf[p]` is the first component, `a`.
    instance Producer[FirstOf[p]] with Producer[p] where Producer.Elm[p] ~ (a, b) {
        type Elm = a
        pub def produce(f: FirstOf[p]): a =
            let FirstOf.FirstOf(inner) = f;
            let (x, _) = Producer.produce(inner);
            x
    }

    @Test
    def testFirstList(): Unit \ Assert =
        assertEq(expected = 1, Container.first(1 :: 2 :: 3 :: Nil))

    @Test
    def testFirstWrapperInt(): Unit \ Assert =
        assertEq(expected = 1, Container.first(Wrapper.Wrapper(1 :: 2 :: 3 :: Nil)))

    @Test
    def testFirstWrapperString(): Unit \ Assert =
        assertEq(expected = "a", Container.first(Wrapper.Wrapper("a" :: "b" :: Nil)))

    @Test
    def testFirstTwice(): Unit \ Assert =
        assertEq(expected = (1, 4), Container.first(Twice.Twice(1 :: 2 :: Nil, 4 :: 5 :: Nil)))

    @Test
    def testProduceFirstOf(): Unit \ Assert =
        assertEq(expected = 1, Producer.produce(FirstOf.FirstOf(Pair.Pair(1, true))))

}
