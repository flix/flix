namespace Validation {

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getWithDefault01(): Bool = (Success(123)).getWithDefault(456) == 123

    @test
    def getWithDefault02(): Bool = (Failure(Nel(42, Nil))).getWithDefault(456) == 456

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = exists(x -> x == 123, (Success(123))) == true

    @test
    def exists02(): Bool = exists(x -> x != 123, (Success(123))) == false

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = exists(x -> x == 123, (Success(123))) == true

    @test
    def forall02(): Bool = exists(x -> x != 123, (Success(123))) == false

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = map(x -> x, Success(123)) == Success(123)

    @test
    def map02(): Bool = map(x -> x + 1, Success(123)) == Success(124)

    @test
    def map03(): Bool = map(x -> x, Failure(Nel(123, Nil))) == Failure(Nel(123, Nil))

    @test
    def map04(): Bool = Success(123) |> map(x -> x + 1) |> map(x -> x * 2) == Success(248)

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOption01(): Bool = (Success(123)).toOption() == Some(123)

    @test
    def toOption02(): Bool = (Failure(Nel(42, Nil))).toOption() == None

    /////////////////////////////////////////////////////////////////////////////
    // toResult                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toResult01(): Bool = (Success(123)).toResult() == Ok(123)

    @test
    def toResult02(): Bool = (Failure(Nel(42, Nil))).toResult() == Err(Nel(42, Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = (Success(123)).toList() == 123 :: Nil

    @test
    def toList02(): Bool = (Failure(Nel(42, Nil))).toList() == Nil

}