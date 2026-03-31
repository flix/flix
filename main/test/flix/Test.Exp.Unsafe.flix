mod Test.Exp.Unsafe {

    import java.lang.System

    use Assert.{assertEq, assertTrue};

    @Test
    def test01(): Unit = unsafe IO { println(42) }

    @Test
    def test02(): Unit = unsafe IO { println(42) }

    @Test
    def test03(): Unit \ Assert =
        let t = unsafe IO { System.currentTimeMillis() };
        assertTrue(t > 0i64)

    @Test
    def test04(): Unit \ Assert =
        assertEq(expected = 2, unsafe OutOfBounds { Vector#{1, 2, 3}[1] })

    def unsafeCompose(f: a -> b \ ef1, g: b -> c \ ef2): a -> c \ ef2 =
        x -> g(unsafe ef1 { f(x) }) // note that `unsafe .. (g(f(x))` would give ef2 - ef1

    @Test
    def test05(): Unit \ Assert = {
        def f() = {println("hello"); 42};
        def g(i) = i + 1;
        assertEq(expected = 43, unsafeCompose(f, g)())
    }

    def unsafeAbortKeyNotFoundHandler(f: a -> b \ ef): a -> b \ ef - KeyNotFound =
        x -> unsafe KeyNotFound { f(x) }

    @Test
    def test06(): Unit \ Assert = {
        assertEq(expected = 42, unsafeAbortKeyNotFoundHandler(() -> Map#{'a' => 42}['a'])())
    }

    @Test
    def test07(): Unit \ Assert + IO = region local {
        let r = Ref.fresh(local, 42);
        def unsafeDebug(): Unit \ IO = {
            let v = unsafe local { Ref.get(r) };
            println(v)
        };
        spawn unsafeDebug() @ local;
        assertEq(expected = 42, Ref.get(r))
    }

}
