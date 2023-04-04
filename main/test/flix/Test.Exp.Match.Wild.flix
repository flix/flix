mod Test.Exp.Match.Wild {

    @test
    def testWild01(): Bool = match () {
        case _ => true
    }

    @test
    def testWild02(): Bool = match 123 {
        case 0 => false
        case 1 => false
        case _ => true
    }

    @test
    def testWild03(): Bool = match None {
        case Some(_) => false
        case _       => true
    }

    @test
    def testWild04(): Bool = match Some(Ok(123)) {
        case Some(Err(_)) => false
        case Some(Ok(_))  => true
        case _            => false
    }

}
