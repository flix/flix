mod Test.Eff.Poly.Trait {

    use Assert.assertEq

    eff AskTell[t] {
        def askTell(x: t): t
    }

    eff Render[t] {
        def render(x: t): String with ToString[t]
    }

    trait Echo[t] {
        pub def echo(x: t): t \ AskTell[t]
    }

    instance Echo[Int32] {
        pub def echo(x: Int32): Int32 \ AskTell[Int32] =
            AskTell.askTell(x)
    }

    trait Renderer[t] {
        pub def render(x: t): String \ Render[t] with ToString[t]
    }

    instance Renderer[Int32] {
        pub def render(x: Int32): String \ Render[Int32] =
            Render.render(x)
    }

    enum IntSource {
        case Mk(Int32)
    }

    trait Source[s] {
        type Out: Type
        pub def read(src: s): Source.Out[s]
    }

    instance Source[IntSource] {
        type Out = Int32

        pub def read(src: IntSource): Int32 = match src {
            case IntSource.Mk(x) => x
        }
    }

    trait Action[p] {
        type E: Eff
        pub def act(p: p): Int32 \ Action.E[p]
    }

    instance Action[IntSource] {
        type E = AskTell[Int32]

        pub def act(p: IntSource): Int32 \ AskTell[Int32] =
            AskTell.askTell(Source.read(p))
    }

    def echoViaTrait(x: t): t \ AskTell[t] with Echo[t] =
        Echo.echo(x)

    def renderViaTrait(x: t): String \ Render[t] with Renderer[t], ToString[t] =
        Renderer.render(x)

    def askViaSource(src: s): Source.Out[s] \ AskTell[Source.Out[s]] with Source[s] =
        AskTell.askTell(Source.read(src))

    def actViaTrait(p: p): Int32 \ Action.E[p] with Action[p] =
        Action.act(p)

    @Test
    def testTraitInstance(): Unit \ Assert =
        let result = run echoViaTrait(41) with handler AskTell {
            def askTell(x, k) = k(x + 1)
        };
        assertEq(expected = 42, result)

    @Test
    def testToStringBoundViaTrait(): Unit \ Assert =
        let result = run renderViaTrait(42) with handler Render {
            def render(x, k) = k("value=${x}")
        };
        assertEq(expected = "value=42", result)

    @Test
    def testAssociatedValueType(): Unit \ Assert =
        let result = run askViaSource(IntSource.Mk(41)) with handler AskTell {
            def askTell(x, k) = k(x + 1)
        };
        assertEq(expected = 42, result)

    @Test
    def testAssociatedEffectType(): Unit \ Assert =
        let result = run actViaTrait(IntSource.Mk(41)) with handler AskTell {
            def askTell(x, k) = k(x + 1)
        };
        assertEq(expected = 42, result)

}
