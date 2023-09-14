mod Test.Exp.Match.Guard {

    @Test
    def testGuard01(): Bool =
        let r = match Some('a') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 1

    @Test
    def testGuard02(): Bool =
        let r = match Some('b') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 2

    @Test
    def testGuard03(): Bool =
        let r = match Some('c') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 3

    @Test
    def testGuard04(): Bool =
        let r = match Some('z') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 42

    @Test
    def testGuard05(): Bool =
        let r = match None {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 42

}
