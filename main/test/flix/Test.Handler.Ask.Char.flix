mod Test.Handler.Ask.Char {

    eff Ask {
        def ask(x: Char): Unit
    }

    def generator(x: Char): Unit \ Ask =
        Ask.ask(x); generator(next(x))

    def sample(limit: Char): List[Char] =
        try {
            generator('a'); Nil
        } with Ask {
            def ask(x, k) = if (x == limit) Nil else x :: k()
        }

    def next(c: Char): Char = match c {
        case 'a' => 'b'
        case 'b' => 'c'
        case 'c' => 'd'
        case 'd' => 'e'
        case 'e' => 'f'
        case 'f' => 'g'
        case 'g' => 'h'
        case 'h' => 'i'
        case 'i' => 'j'
        case 'j' => 'k'
        case 'k' => 'l'
        case 'l' => 'm'
        case 'm' => 'n'
        case 'n' => 'o'
        case 'o' => 'p'
        case 'p' => 'q'
        case 'q' => 'r'
        case 'r' => 's'
        case 's' => 't'
        case 't' => 'u'
        case 'u' => 'v'
        case 'v' => 'w'
        case 'w' => 'x'
        case 'x' => 'y'
        case 'y' => 'z'
        case 'z' => 'a'
        case _ => unreachable!()
    }

    def range(a: Char, b: Char): List[Char] =
        List.unfold(i -> if (i >= b) None else Some((i, next(i))), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample('a'))

    @Test
    def testSample02(): Bool =
        Assert.eq('a' :: Nil, sample('b'))

    @Test
    def testSample03(): Bool =
        Assert.eq('a' :: 'b' :: Nil, sample('c'))

    @Test
    def testSample04(): Bool =
        Assert.eq('a' :: 'b' :: 'c' :: Nil, sample('d'))

    @Test
    def testSample05(): Bool =
        Assert.eq('a' :: 'b' :: 'c' :: 'd' :: Nil, sample('e'))

    @Test
    def testSample06(): Bool =
        Assert.eq('a' :: 'b' :: 'c' :: 'd' :: 'e' :: Nil, sample('f'))

    @Test
    def testSample07(): Bool =
        Assert.eq('a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: Nil, sample('g'))

    @Test
    def testSample08(): Bool =
        Assert.eq(range('a', 'k'), sample('k'))

    @Test
    def testSample09(): Bool =
        Assert.eq(range('a', 'z'), sample('z'))

}
