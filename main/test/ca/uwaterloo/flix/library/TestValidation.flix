namespace Validation {

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOption01(): Bool = (Success(123)).toOption() == Some(123)

    @test
    def toOption02(): Bool = (Failure(Nel(42, Nil))).toOption() == None

}