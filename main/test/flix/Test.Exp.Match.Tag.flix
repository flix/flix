mod Test.Exp.Match.Tag {

    use Assert.{success, fail}

    @Test
    def matchOption01(): Unit \ Assert = match None {
        case None    => success("matched None")
        case Some(_) => fail("unexpected match on Some")
    }

    @Test
    def matchOption02(): Unit \ Assert = match None {
        case Some(_) => fail("unexpected match on Some")
        case None    => success("matched None")
    }

    @Test
    def matchOption03(): Unit \ Assert = match Some(123) {
        case None    => fail("unexpected match on None")
        case Some(_) => success("matched Some")
    }

    @Test
    def matchOption04(): Unit \ Assert = match Some(123) {
        case Some(_) => success("matched Some")
        case None    => fail("unexpected match on None")
    }

    @Test
    def matchResult01(): Unit \ Assert = match Ok(123) {
        case Ok(_)  => success("matched Ok")
        case Err(_) => fail("unexpected match on Err")
    }

    @Test
    def matchResult02(): Unit \ Assert = match Ok(123) {
        case Err(_) => fail("unexpected match on Err")
        case Ok(_)  => success("matched Ok")
    }

    @Test
    def matchResult03(): Unit \ Assert = match Err(123) {
        case Ok(_)  => fail("unexpected match on Ok")
        case Err(_) => success("matched Err")
    }

    @Test
    def matchResult04(): Unit \ Assert = match Err(123) {
        case Err(_) => success("matched Err")
        case Ok(_)  => fail("unexpected match on Ok")
    }

    @Test
    def matchNested01(): Unit \ Assert = match None {
        case None           => success("matched None")
        case Some(None)     => fail("unexpected match on Some(None)")
        case Some(Some(_))  => fail("unexpected match on Some(Some(_))")
    }

    @Test
    def matchNested02(): Unit \ Assert = match Some(None) {
        case None           => fail("unexpected match on None")
        case Some(None)     => success("matched Some(None)")
        case Some(Some(_))  => fail("unexpected match on Some(Some(_))")
    }

    @Test
    def matchNested03(): Unit \ Assert = match Some(Some(123)) {
        case None           => fail("unexpected match on None")
        case Some(None)     => fail("unexpected match on Some(None)")
        case Some(Some(_))  => success("matched Some(Some(_))")
    }

    @Test
    def matchNested04(): Unit \ Assert = match None {
        case None         => success("matched None")
        case Some(Ok(_))  => fail("unexpected match on Some(Ok(_))")
        case Some(Err(_)) => fail("unexpected match on Some(Err(_))")
    }

    @Test
    def matchNested05(): Unit \ Assert = match Some(Ok(123)) {
        case None         => fail("unexpected match on None")
        case Some(Ok(_))  => success("matched Some(Ok(_))")
        case Some(Err(_)) => fail("unexpected match on Some(Err(_))")
    }

    @Test
    def matchNested06(): Unit \ Assert = match Some(Err(123)) {
        case None         => fail("unexpected match on None")
        case Some(Ok(_))  => fail("unexpected match on Some(Ok(_))")
        case Some(Err(_)) => success("matched Some(Err(_))")
    }

    @Test
    def matchTopLevel01(): Unit \ Assert = match (Option.Some(1)) {
        case Option.None    => fail("unexpected match on Option.None")
        case Option.Some(_) => success("matched Option.Some")
    }
}
