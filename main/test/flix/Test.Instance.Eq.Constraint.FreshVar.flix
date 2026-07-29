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
    // `Container.Elm[c] ~ a`. It names the element type of `c` and is used as an
    // abbreviation in the method signature below.
    enum Wrapper[c](c)

    instance Container[Wrapper[c]] with Container[c] where Container.Elm[c] ~ a {
        type Elm = Container.Elm[c]
        pub def first(w: Wrapper[c]): a =
            let Wrapper.Wrapper(inner) = w;
            Container.first(inner)
    }

    // A second wrapper, showing the fresh variable is genuinely determined by
    // the element type of the wrapped value and not fixed to any single type.
    enum Twice[c](c, c)

    instance Container[Twice[c]] with Container[c] where Container.Elm[c] ~ a {
        type Elm = (Container.Elm[c], Container.Elm[c])
        pub def first(t: Twice[c]): (a, a) =
            let Twice.Twice(x, y) = t;
            (Container.first(x), Container.first(y))
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

}
