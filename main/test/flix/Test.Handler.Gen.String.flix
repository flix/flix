mod Test.Handler.Gen.String {

    eff Gen {
        def gen(): String
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: String): List[String] =
        region rc {
            let counter = Ref.fresh(rc, "a");
            run {
                generator(); Nil
            } with handler Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (i == limit) Nil else i :: k(i)
            }
        }

    def next(c: String): String = match c {
        case "a" => "b"
        case "b" => "c"
        case "c" => "d"
        case "d" => "e"
        case "e" => "f"
        case "f" => "g"
        case "g" => "h"
        case "h" => "i"
        case "i" => "j"
        case "j" => "k"
        case "k" => "l"
        case "l" => "m"
        case "m" => "n"
        case "n" => "o"
        case "o" => "p"
        case "p" => "q"
        case "q" => "r"
        case "r" => "s"
        case "s" => "t"
        case "t" => "u"
        case "u" => "v"
        case "v" => "w"
        case "w" => "x"
        case "x" => "y"
        case "y" => "z"
        case "z" => "a"
        case _ => unreachable!()
    }

    def getAndInc(r: Ref[String, r]): String \ r =
        let c = Ref.get(r);
        Ref.put(next(c), r);
        c

    def range(a: String, b: String): List[String] =
        List.unfold(i -> if (i >= b) None else Some((i, next(i))), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample("a"))

    @Test
    def testSample02(): Bool =
        Assert.eq("a" :: Nil, sample("b"))

    @Test
    def testSample03(): Bool =
        Assert.eq("a" :: "b" :: Nil, sample("c"))

    @Test
    def testSample04(): Bool =
        Assert.eq("a" :: "b" :: "c" :: Nil, sample("d"))

    @Test
    def testSample05(): Bool =
        Assert.eq("a" :: "b" :: "c" :: "d" :: Nil, sample("e"))

    @Test
    def testSample06(): Bool =
        Assert.eq("a" :: "b" :: "c" :: "d" :: "e" :: Nil, sample("f"))

    @Test
    def testSample07(): Bool =
        Assert.eq("a" :: "b" :: "c" :: "d" :: "e" :: "f" :: Nil, sample("g"))

    @Test
    def testSample08(): Bool =
        Assert.eq(range("a", "k"), sample("k"))

    @Test
    def testSample09(): Bool =
        Assert.eq(range("a", "z"), sample("z"))

}
