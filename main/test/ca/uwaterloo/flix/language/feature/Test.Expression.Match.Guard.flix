namespace Test/Expression/Match/Guard {

    @test
    def guard01(): Bool =
        let r = match Some('a') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 1

    @test
    def guard02(): Bool =
        let r = match Some('b') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 2

    @test
    def guard03(): Bool =
        let r = match Some('c') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 3

    @test
    def guard04(): Bool =
        let r = match Some('z') {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 42

    @test
    def guard05(): Bool =
        let r = match None {
            case Some(x) if x == 'a' => 1
            case Some(x) if x == 'b' => 2
            case Some(x) if x == 'c' => 3
            case _                   => 42
        };
        r == 42

}