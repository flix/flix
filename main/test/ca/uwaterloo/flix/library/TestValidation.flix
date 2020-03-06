namespace Validation {

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