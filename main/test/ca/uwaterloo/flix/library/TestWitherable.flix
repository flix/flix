namespace TestWitherable {

    use Witherable.wither;

    /////////////////////////////////////////////////////////////////////////////
    // wither                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def wither01(): Bool  =
        let xs = Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(Nil)

    @test
    def wither02(): Bool  =
        let xs = Nil;
        wither(_ -> Some(None: Option[Int32]), xs) == Some(Nil)

    @test
    def wither03(): Bool  =
        let xs = 1 :: Nil;
        wither(x -> Some(x + 1) |> Some, xs) == Some(2 :: Nil)

    @test
    def wither04(): Bool  =
        let xs = 1 :: Nil;
        wither(_ -> Some(None: Option[Int32]), xs) == Some(Nil)

    @test
    def wither05(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(x -> Some(if (x > 1) None else Some(x - 1)), xs) == Some(0 :: Nil)

    @test
    def wither100(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(_ -> None: Option[Option[Int32]], xs) == None


/*

    @test
    def wither03(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(x -> Some(x+1), xs) == Some(2 :: 3 :: Nil)

    @test
    def wither04(): Bool  =
        let xs = Nil;
        wither(_ -> None: Option[Int32], xs) == Some(Nil)

    @test
    def wither05(): Bool  =
        let xs = 1 :: Nil;
        wither(_ -> None: Option[Int32], xs) == None

    @test
    def wither06(): Bool  =
        let xs = 1 :: 2 :: Nil;
        wither(_ -> None: Option[Int32], xs) == None
*/

}
