mod Test.Exp.Match.Guard {

    use Assert.assertEq

    @Test
    def guard01(): Unit \ Assert =
        let r = match Some('a') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        assertEq(expected = 1, r)

    @Test
    def guard02(): Unit \ Assert =
        let r = match Some('b') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        assertEq(expected = 2, r)

    @Test
    def guard03(): Unit \ Assert =
        let r = match Some('c') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        assertEq(expected = 3, r)

    @Test
    def guard04(): Unit \ Assert =
        let r = match Some('z') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        assertEq(expected = 42, r)

    @Test
    def guard05(): Unit \ Assert =
        let r = match None {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        assertEq(expected = 42, r)

}
