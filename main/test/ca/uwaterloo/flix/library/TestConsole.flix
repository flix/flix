mod TestConsole {

    use Assert.assertEq
    use Sys.Console

    /////////////////////////////////////////////////////////////////////////////
    // print                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def print01(): Unit \ Assert =
        let (_, out, err) = runWithLists(Nil, () -> Console.print("hello"));
        assertEq(expected = "hello" :: Nil, out);
        assertEq(expected = Nil, err)

    /////////////////////////////////////////////////////////////////////////////
    // println                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def println01(): Unit \ Assert =
        let (_, out, _) = runWithLists(Nil, () -> Console.println(42));
        assertEq(expected = "42\n" :: Nil, out)

    @Test
    def println02(): Unit \ Assert =
        let (_, out, _) = runWithLists(Nil, () -> Console.println(Color.Red));
        assertEq(expected = "Red\n" :: Nil, out)

    @Test
    def println03(): Unit \ Assert =
        let (_, out, err) = runWithLists(Nil, () -> {
            Console.print("a");
            Console.println("b");
            Console.eprint("c");
            Console.println("d")
        });
        assertEq(expected = "a" :: "b\n" :: "d\n" :: Nil, out);
        assertEq(expected = "c" :: Nil, err)

    /////////////////////////////////////////////////////////////////////////////
    // eprint                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eprint01(): Unit \ Assert =
        let (_, out, err) = runWithLists(Nil, () -> Console.eprint("oops"));
        assertEq(expected = Nil, out);
        assertEq(expected = "oops" :: Nil, err)

    /////////////////////////////////////////////////////////////////////////////
    // eprintln                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eprintln01(): Unit \ Assert =
        let (_, out, err) = runWithLists(Nil, () -> Console.eprintln(42));
        assertEq(expected = Nil, out);
        assertEq(expected = "42\n" :: Nil, err)

    /////////////////////////////////////////////////////////////////////////////
    // readln                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def readln01(): Unit \ Assert =
        let (res, _, _) = runWithLists("a" :: "b" :: Nil, () -> {
            let x = Console.readln();
            let y = Console.readln();
            let z = Console.readln();
            (x, y, z)
        });
        assertEq(expected = (Some("a"), Some("b"), None), res)

    @Test
    def readln02(): Unit \ Assert =
        let (res, _, _) = runWithLists("" :: Nil, () -> Console.readln());
        assertEq(expected = Some(""), res)

    /////////////////////////////////////////////////////////////////////////////
    // readlnWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def readlnWith01(): Unit \ Assert =
        let (res, out, err) = runWithLists("5" :: Nil, () -> Console.readlnWith("n: ", parseInt32));
        assertEq(expected = Some(5), res);
        assertEq(expected = "n: " :: Nil, out);
        assertEq(expected = Nil, err)

    @Test
    def readlnWith02(): Unit \ Assert =
        let (res, out, err) = runWithLists("x" :: "7" :: Nil, () -> Console.readlnWith("n: ", parseInt32));
        assertEq(expected = Some(7), res);
        assertEq(expected = "n: " :: "n: " :: Nil, out);
        assertEq(expected = "Not a number.\n" :: Nil, err)

    @Test
    def readlnWith03(): Unit \ Assert =
        let (res, out, err) = runWithLists("x" :: Nil, () -> Console.readlnWith("n: ", parseInt32));
        assertEq(expected = None, res);
        assertEq(expected = "n: " :: "n: " :: Nil, out);
        assertEq(expected = "Not a number.\n" :: Nil, err)

    /////////////////////////////////////////////////////////////////////////////
    // readlnAs                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def readlnAs01(): Unit \ Assert =
        let (res, out, err) = runWithLists("abc" :: "42" :: Nil, () -> Console.readlnAs("n: "));
        assertEq(expected = Some(42), res);
        assertEq(expected = "n: " :: "n: " :: Nil, out);
        assertEq(expected = "Invalid input. Please try again.\n" :: Nil, err)

    @Test
    def readlnAs02(): Unit \ Assert =
        let (res, _, _) = runWithLists("true" :: Nil, () -> Console.readlnAs("b: "));
        assertEq(expected = Some(true), res)

    @Test
    def readlnAs03(): Unit \ Assert =
        let (res, _, _) = runWithLists("  42  " :: Nil, () -> Console.readlnAs("n: "));
        assertEq(expected = Some(42), res)

    @Test
    def readlnAs04(): Unit \ Assert =
        let (res, out, err) = runWithLists(Nil, () -> Console.readlnAs("n: "));
        assertEq(expected = (None: Option[Int32]), res);
        assertEq(expected = "n: " :: Nil, out);
        assertEq(expected = Nil, err)

    /////////////////////////////////////////////////////////////////////////////
    // confirm                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def confirm01(): Unit \ Assert =
        let (res, out, err) = runWithLists("y" :: Nil, () -> Console.confirm("Deploy?", default = false));
        assertEq(expected = true, res);
        assertEq(expected = "Deploy? [y/N] " :: Nil, out);
        assertEq(expected = Nil, err)

    @Test
    def confirm02(): Unit \ Assert =
        let (res, out, _) = runWithLists("n" :: Nil, () -> Console.confirm("Deploy?", default = true));
        assertEq(expected = false, res);
        assertEq(expected = "Deploy? [Y/n] " :: Nil, out)

    @Test
    def confirm03(): Unit \ Assert =
        let (res, _, _) = runWithLists("  YES  " :: Nil, () -> Console.confirm("Deploy?", default = false));
        assertEq(expected = true, res)

    @Test
    def confirm04(): Unit \ Assert =
        let (res, _, _) = runWithLists("" :: Nil, () -> Console.confirm("Deploy?", default = true));
        assertEq(expected = true, res)

    @Test
    def confirm05(): Unit \ Assert =
        let (res, _, _) = runWithLists("" :: Nil, () -> Console.confirm("Deploy?", default = false));
        assertEq(expected = false, res)

    @Test
    def confirm06(): Unit \ Assert =
        let (res, out, err) = runWithLists("maybe" :: "y" :: Nil, () -> Console.confirm("Deploy?", default = false));
        assertEq(expected = true, res);
        assertEq(expected = "Deploy? [y/N] " :: "Deploy? [y/N] " :: Nil, out);
        assertEq(expected = "Please answer 'y' or 'n'.\n" :: Nil, err)

    @Test
    def confirm07(): Unit \ Assert =
        let (res, _, _) = runWithLists(Nil, () -> Console.confirm("Deploy?", default = true));
        assertEq(expected = true, res)

    /////////////////////////////////////////////////////////////////////////////
    // Helpers                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// An enum with a derived `ToString` instance.
    ///
    enum Color with ToString {
        case Red
        case Green
    }

    ///
    /// Parses the string `s` as an `Int32`.
    ///
    def parseInt32(s: String): Result[String, Int32] = match Int32.fromString(s) {
        case Some(i) => Ok(i)
        case None    => Err("Not a number.")
    }

    ///
    /// Runs the `Console` effect of the given function `f` with the given `input` lines.
    ///
    /// Returns the result of `f` together with the strings written to stdout and to stderr.
    ///
    /// A `writeLine` or `writeErrLine` is recorded with a trailing newline.
    ///
    def runWithLists(input: List[String], f: Unit -> a \ ef): (a, List[String], List[String]) \ ef - Console = region rc {
        let inp = Ref.fresh(rc, input);
        let out = MutList.empty(rc);
        let err = MutList.empty(rc);
        let res = run {
            f()
        } with handler Console {
            def readln(k) = match Ref.get(inp) {
                case Nil     => k(None)
                case x :: xs => Ref.put(xs, inp); k(Some(x))
            }
            def write(s, k)        = { MutList.push(s, out); k() }
            def writeLine(s, k)    = { MutList.push(s + "\n", out); k() }
            def writeErr(s, k)     = { MutList.push(s, err); k() }
            def writeErrLine(s, k) = { MutList.push(s + "\n", err); k() }
        };
        (res, MutList.toList(out), MutList.toList(err))
    }

}
