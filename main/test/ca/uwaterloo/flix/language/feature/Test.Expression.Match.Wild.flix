namespace Test/Expression/Match/Wild {

    @test
    def wild01(): Bool = match () {
        case _ => true
    }

    @test
    def wild02(): Bool = match 123 {
        case 0 => false
        case 1 => false
        case _ => true
    }

    @test
    def wild03(): Bool = match None {
        case Some(_) => false
        case _       => true
    }

    @test
    def wild04(): Bool = match Some(Ok(123)) {
        case Some(Err(_)) => false
        case Some(Ok(_))  => true
        case _            => false
    }

}
