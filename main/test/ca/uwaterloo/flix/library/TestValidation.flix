namespace Validation {

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = ap(Success(x -> x + 1), Success(123)) == Success(124)

    @test
    def ap02(): Bool = ap(Success(x -> x + 1), Failure(Nel(42, Nil))) == Failure(Nel(42, Nil))

    @test
    def ap03(): Bool = ap(Failure(Nel(42, Nil)), Success(123)) == Failure(Nel(42, Nil))

    @test
    def ap04(): Bool = ap(Failure(Nel(123, Nil)), (Failure(Nel(456, Nil)))) == Failure(Nel(123, 456 :: Nil))

    @test
    def ap05(): Bool = ap(Failure(Nel(1, 2 :: Nil)), (Failure(Nel(3, Nil)))) == Failure(Nel(1, 2 :: 3 :: Nil))

    @test
    def ap06(): Bool = ap(Failure(Nel(1, Nil)), (Failure(Nel(2, 3 :: Nil)))) == Failure(Nel(1, 2 :: 3 :: Nil))

    @test
    def ap07(): Bool = ap(Failure(Nel(1, 2 :: Nil)), (Failure(Nel(3, 4 :: Nil)))) == Failure(Nel(1, 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getWithDefault01(): Bool = (Success(123)).getWithDefault(456) == 123

    @test
    def getWithDefault02(): Bool = (Failure(Nel(42, Nil))).getWithDefault(456) == 456

    /////////////////////////////////////////////////////////////////////////////
    // withDefault                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def withDefault01(): Bool = withDefault(Success(1), Success(2)) == Success(1)

    @test
    def withDefault02(): Bool = withDefault(Success(1), Failure(Nel(1, Nil))) == Success(1)

    @test
    def withDefault03(): Bool = withDefault(Failure(Nel(1, Nil)), Success(2)) == Success(2)

    @test
    def withDefault04(): Bool = withDefault(Failure(Nel(1, Nil)), Failure(Nel(2, Nil))) == Failure(Nel(2, Nil))

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
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool = sequence(Success(1) :: Nil) == Success(1 :: Nil)

    @test
    def sequence02(): Bool = sequence(Success(1) :: Success(2) :: Nil) == Success(1 :: 2 :: Nil)

    @test
    def sequence03(): Bool = sequence(Success(1) :: Success(2) :: Success(3) :: Nil) == Success(1 :: 2 :: 3 :: Nil)

    @test
    def sequence04(): Bool = sequence(Failure(Nel(1, Nil)) :: Nil) == Failure(Nel(1, Nil))

    @test
    def sequence05(): Bool = sequence(Failure(Nel(1, Nil)) :: Failure(Nel(2, Nil)) :: Nil) == Failure(Nel(1, 2 :: Nil))

    @test
    def sequence06(): Bool = sequence(Success(1) :: Failure(Nel(42, Nil)) :: Nil) == Failure(Nel(42, Nil))

    @test
    def sequence07(): Bool = sequence(Failure(Nel(42, Nil)) :: Success(1) :: Nil) == Failure(Nel(42, Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = traverse(x -> Success(x + 1), 1 :: Nil) == Success(2 :: Nil)

    @test
    def traverse02(): Bool = traverse(x -> Success(x + 1), 1 :: 2 :: Nil) == Success(2 :: 3 :: Nil)

    @test
    def traverse03(): Bool = traverse(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil) == Success(2 :: 3 :: 4 :: Nil)

    @test
    def traverse04(): Bool = traverse(x -> Failure(Nel(x, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(1, 2 :: 3 :: Nil))

    @test
    def traverse05(): Bool = traverse(x -> Failure(Nel(x, x :: Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(1, 1 :: 2 :: 2 :: 3 :: 3 :: Nil))

    @test
    def traverse06(): Bool = traverse(x -> if (x != 2) Success(x + 1) else Failure(Nel(42, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(42, Nil))

    @test
    def traverse07(): Bool = traverse(x -> if (x == 2) Success(x + 1) else Failure(Nel(x, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(1, 3 :: Nil))

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