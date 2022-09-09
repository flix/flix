namespace TestWitherable {

    use Witherable.wither;

    /////////////////////////////////////////////////////////////////////////////
    // wither                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def wither01(): Bool =
        let xs: List[Int32] = Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(Nil)

    @test
    def wither02(): Bool =
        let xs: List[Int32] = Nil;
        wither(_ -> Some(None: Option[Int32]), xs) == Some(Nil)

    @test
    def wither03(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(2 :: Nil)

    @test
    def wither04(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        wither(_ -> Some(None: Option[Int32]), xs) == Some(Nil)


/*
    @test
    def wither02(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        wither(x -> None :: Nil, xs) == Some(None)

    @test
    def wither03(): Bool =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        wither(x -> Some(x+1), xs) == Some(2 :: 3 :: Nil)

    @test
    def wither04(): Bool =
        let xs: List[Int32] = Nil;
        wither(_ -> None: Option[Int32], xs) == Some(Nil)

    @test
    def wither05(): Bool =
        let xs: List[Int32] = 1 :: Nil;
        wither(_ -> None: Option[Int32], xs) == None

    @test
    def wither06(): Bool =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        wither(_ -> None: Option[Int32], xs) == None
*/

}
