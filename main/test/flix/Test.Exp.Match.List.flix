mod Test.Exp.Match.List {

    use Assert.{assertEq, assertTrue, success, fail}

    // Define an enum with a `Cons` tag to ensure that it's not used in any of the following tests
    pub enum Foo {
        case Cons
    }

    @Test
    def matchList01(): Unit \ Assert = match Nil {
        case Nil => success("matched Nil")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchList02(): Unit \ Assert = match Nil {
        case Nil      => success("matched Nil")
        case 1 :: Nil => fail("unexpected match on 1 :: Nil")
        case _        => fail("unexpected match on wildcard")
    }

    @Test
    def matchList03(): Unit \ Assert = match Nil {
        case Nil           => success("matched Nil")
        case 1 :: Nil      => fail("unexpected match on 1 :: Nil")
        case 1 :: 2 :: Nil => fail("unexpected match on 1 :: 2 :: Nil")
        case _             => fail("unexpected match on wildcard")
    }

    @Test
    def matchList04(): Unit \ Assert = match 1 :: Nil {
        case Nil      => fail("unexpected match on Nil")
        case 1 :: Nil => success("matched 1 :: Nil")
        case _        => fail("unexpected match on wildcard")
    }

    @Test
    def matchList05(): Unit \ Assert = match 1 :: 2 :: Nil {
        case Nil      => fail("unexpected match on Nil")
        case _ :: _   => success("matched _ :: _")
    }

    @Test
    def matchList06(): Unit \ Assert = match 1 :: 2 :: Nil {
        case Nil            => fail("unexpected match on Nil")
        case 1 :: Nil       => fail("unexpected match on 1 :: Nil")
        case 1 :: 2 :: Nil  => success("matched 1 :: 2 :: Nil")
        case _              => fail("unexpected match on wildcard")
    }

    @Test
    def matchList07(): Unit \ Assert = match 1 :: 2 :: Nil {
        case x :: y :: Nil  => assertTrue(x + 1 == y)
        case _              => fail("unexpected match on wildcard")
    }

    @Test
    def matchList08(): Unit \ Assert = match 1 :: 2 :: 3 :: Nil {
        case x :: 2 :: z :: Nil => assertTrue(x + 2 == z)
        case _                  => fail("unexpected match on wildcard")
    }

    @Test
    def patternOption01(): Unit \ Assert = match Nil {
        case Nil => success("matched Nil")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def patternOption02(): Unit \ Assert = match None :: Nil {
        case Nil         => fail("unexpected match on Nil")
        case None :: Nil => success("matched None :: Nil")
        case _           => fail("unexpected match on wildcard")
    }

    @Test
    def patternOption03(): Unit \ Assert = match Some(123) :: Nil {
        case Nil              => fail("unexpected match on Nil")
        case Some(x) :: Nil   => assertEq(expected = 123, x)
        case _                => fail("unexpected match on wildcard")
    }

    @Test
    def listList01(): Unit \ Assert = match (Nil : List[List[Int32]]) {
        case Nil              => success("matched Nil")
        case _                => fail("unexpected match on wildcard")
    }

    @Test
    def listList02(): Unit \ Assert = match (1 :: Nil) :: Nil {
        case Nil                => fail("unexpected match on Nil")
        case (1 :: Nil) :: Nil  => success("matched (1 :: Nil) :: Nil")
        case _                  => fail("unexpected match on wildcard")
    }

    @Test
    def listList03(): Unit \ Assert = match (1 :: Nil) :: (2 :: Nil) :: Nil {
        case Nil                             => fail("unexpected match on Nil")
        case (x :: Nil) :: (y :: Nil) :: Nil => assertTrue(x + 1 == y)
        case _                               => fail("unexpected match on wildcard")
    }

}
