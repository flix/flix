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

    enum StringSource {
        case Mk(String)
    }

    instance Source[StringSource] {
        type Out = String

        pub def read(src: StringSource): String = match src {
            case StringSource.Mk(s) => s
        }
    }

    instance Action[StringSource] {
        type E = AskTell[String]

        pub def act(p: StringSource): Int32 \ AskTell[String] =
            String.length(AskTell.askTell(Source.read(p)))
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

    // The handler does not constrain the effect argument; it is inferred from the reduced associated effect.
    @Test
    def testAssociatedEffectTypeSecondInstance(): Unit \ Assert =
        let result = run actViaTrait(StringSource.Mk("abc")) with handler AskTell {
            def askTell(x, k) = k(x)
        };
        assertEq(expected = 3, result)

    eff KeyMissing[k] {
        def keyMissing(k: k): Void
    }

    def runKeyMissing(f: Unit -> a \ ef): Result[k, a] \ (ef - KeyMissing[k]) =
        run Ok(f()) with handler KeyMissing {
            def keyMissing(k, _c) = Err(k)
        }

    enum Single[k, v] {
        case Single(k, v)
    }

    instance Indexable[Single[k, v]] with Eq[k] {
        type Idx = k
        type Elm = v
        type Aef = KeyMissing[k]
        pub def get(t: Single[k, v], i: k): v \ KeyMissing[k] = match t {
            case Single.Single(k, v) => if (k == i) v else KeyMissing.keyMissing(i)
        }
    }

    def getDirect(t: Single[k, v], i: k): v \ KeyMissing[k] with Eq[k] = match t {
        case Single.Single(k, v) => if (k == i) v else KeyMissing.keyMissing(i)
    }

    // Control: the effect is declared directly on the callee.
    @Test
    def testKeyMissingDirect(): Unit \ Assert =
        let s = Single.Single("a", 1);
        let r = runKeyMissing(() -> getDirect(s, "zzz"));
        assertEq(expected = "Err(zzz)", "${r}")

    // The effect arrives through the associated effect `Indexable.Aef`, and the handler's `k` is
    // determined only by the reduced application `KeyMissing[String]`.
    @Test
    def testKeyMissingViaIndexable(): Unit \ Assert =
        let s = Single.Single("a", 1);
        let r = runKeyMissing(() -> Indexable.get(s, "zzz"));
        assertEq(expected = "Err(zzz)", "${r}")

    @Test
    def testKeyMissingViaIndexableHit(): Unit \ Assert =
        let s = Single.Single("a", 1);
        let r = runKeyMissing(() -> Indexable.get(s, "a"));
        assertEq(expected = "Ok(1)", "${r}")

}
