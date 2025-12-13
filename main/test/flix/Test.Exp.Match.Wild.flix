mod Test.Exp.Match.Wild {

    use Assert.{success, fail}

    @Test
    def wild01(): Unit \ Assert = match () {
        case _ => success("matched wildcard")
    }

    @Test
    def wild02(): Unit \ Assert = match 123 {
        case 0 => fail("unexpected match on 0")
        case 1 => fail("unexpected match on 1")
        case _ => success("matched wildcard")
    }

    @Test
    def wild03(): Unit \ Assert = match None {
        case Some(_) => fail("unexpected match on Some")
        case _       => success("matched wildcard")
    }

    @Test
    def wild04(): Unit \ Assert = match Some(Ok(123)) {
        case Some(Err(_)) => fail("unexpected match on Some(Err(_))")
        case Some(Ok(_))  => success("matched Some(Ok(_))")
        case _            => fail("unexpected match on wildcard")
    }

}
