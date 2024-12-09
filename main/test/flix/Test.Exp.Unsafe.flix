mod Test.Exp.Unsafe {

    import java.lang.System

    @Test
    def test01(): Unit = unsafe_remove {IO} println(42)

    @Test
    def test02(): Unit = unsafe_remove IO {println(42)}

    @Test
    def test03(): Int64 = unsafe_remove {IO} System.currentTimeMillis()

    @Test
    def test04(): Bool = unsafe_remove {OutOfBounds} Vector#{1, 2, 3}[1] == 2

    def unsafeCompose(f: a -> b \ ef1, g: b -> c \ ef2): a -> c \ ef2 =
        x -> g(unsafe_remove ef1 {f(x)}) // note that `unsafe .. (g(f(x))` would give ef2 - ef1

    @Test
    def test05(): Bool = {
        def f() = {println("hello"); 42};
        def g(i) = i + 1;
        unsafeCompose(f, g)() == 43
    }

    def unsafeAbortKeyNotFoundHandler(f: a -> b \ ef): a -> b \ ef - KeyNotFound =
        x -> unsafe_remove {KeyNotFound} f(x)

    @Test
    def test06(): Bool = {
        unsafeAbortKeyNotFoundHandler(() -> Map#{'a' => 42}['a'])() == 42
    }

    @Test
    def test07(): Bool \ IO = region local {
        let r = Ref.fresh(local, 42);
        def unsafeDebug(): Unit \ IO = {
            let v = unsafe_remove local {Ref.get(r)};
            println(v)
        };
        spawn unsafeDebug() @ local;
        Ref.get(r) == 42
    }

}
