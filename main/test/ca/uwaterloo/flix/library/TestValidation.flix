/*
 * Copyright 2020 Magnus Madsen, Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
namespace Validation {

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = ap(Success(x -> x + 1), Success(123)): Validation[_, Unit] == Success(124)

    @test
    def ap02(): Bool = ap(Success(x -> x + 1), Failure(Nel(42, Nil))) == Failure(Nel(42, Nil))

    @test
    def ap03(): Bool = ap(Failure(Nel(42, Nil)), Success(123)): Validation[Unit, _] == Failure(Nel(42, Nil))

    @test
    def ap04(): Bool = ap(Failure(Nel(123, Nil)), (Failure(Nel(456, Nil)))): Validation[Unit, _] == Failure(Nel(123, 456 :: Nil))

    @test
    def ap05(): Bool = ap(Failure(Nel(1, 2 :: Nil)), (Failure(Nel(3, Nil)))): Validation[Unit, _] == Failure(Nel(1, 2 :: 3 :: Nil))

    @test
    def ap06(): Bool = ap(Failure(Nel(1, Nil)), (Failure(Nel(2, 3 :: Nil)))): Validation[Unit, _] == Failure(Nel(1, 2 :: 3 :: Nil))

    @test
    def ap07(): Bool = ap(Failure(Nel(1, 2 :: Nil)), (Failure(Nel(3, 4 :: Nil)))): Validation[Unit, _] == Failure(Nel(1, 2 :: 3 :: 4 :: Nil))

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
    def withDefault01(): Bool = withDefault(Success(1), Success(2)): Validation[_, Unit] == Success(1)

    @test
    def withDefault02(): Bool = withDefault(Success(1), Failure(Nel(1, Nil))) == Success(1)

    @test
    def withDefault03(): Bool = withDefault(Failure(Nel(1, Nil)), Success(2)) == Success(2)

    @test
    def withDefault04(): Bool = withDefault(Failure(Nel(1, Nil)), Failure(Nel(2, Nil))): Validation[Unit, _] == Failure(Nel(2, Nil))

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
    def map01(): Bool = map(x -> x, Success(123)): Validation[_, Unit] == Success(123)

    @test
    def map02(): Bool = map(x -> x + 1, Success(123)): Validation[_, Unit] == Success(124)

    @test
    def map03(): Bool = map(x -> x, Failure(Nel(123, Nil))): Validation[Unit, _] == Failure(Nel(123, Nil))

    @test
    def map04(): Bool = (Success(123): Validation[_, Unit]) |> map(x -> x + 1) |> map(x -> x * 2)  == Success(248)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool = sequence(Success(1) :: Nil): Validation[_, Unit] == Success(1 :: Nil)

    @test
    def sequence02(): Bool = sequence(Success(1) :: Success(2) :: Nil): Validation[_, Unit] == Success(1 :: 2 :: Nil)

    @test
    def sequence03(): Bool = sequence(Success(1) :: Success(2) :: Success(3) :: Nil): Validation[_, Unit] == Success(1 :: 2 :: 3 :: Nil)

    @test
    def sequence04(): Bool = sequence(Failure(Nel(1, Nil)) :: Nil): Validation[List[Unit], _] == Failure(Nel(1, Nil))

    @test
    def sequence05(): Bool = sequence(Failure(Nel(1, Nil)) :: Failure(Nel(2, Nil)) :: Nil): Validation[List[Unit], _] == Failure(Nel(1, 2 :: Nil))

    @test
    def sequence06(): Bool = sequence(Success(1) :: Failure(Nel(42, Nil)) :: Nil) == Failure(Nel(42, Nil))

    @test
    def sequence07(): Bool = sequence(Failure(Nel(42, Nil)) :: Success(1) :: Nil) == Failure(Nel(42, Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = traverse(x -> Success(x + 1), 1 :: Nil): Validation[_, Unit] == Success(2 :: Nil)

    @test
    def traverse02(): Bool = traverse(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[_, Unit] == Success(2 :: 3 :: Nil)

    @test
    def traverse03(): Bool = traverse(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[_, Unit] == Success(2 :: 3 :: 4 :: Nil)

    @test
    def traverse04(): Bool = traverse(x -> Failure(Nel(x, Nil)), 1 :: 2 :: 3 :: Nil): Validation[List[Unit], _] == Failure(Nel(1, 2 :: 3 :: Nil))

    @test
    def traverse05(): Bool = traverse(x -> Failure(Nel(x, x :: Nil)), 1 :: 2 :: 3 :: Nil): Validation[List[Unit], _] == Failure(Nel(1, 1 :: 2 :: 2 :: 3 :: 3 :: Nil))

    @test
    def traverse06(): Bool = traverse(x -> if (x != 2) Success(x + 1) else Failure(Nel(42, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(42, Nil))

    @test
    def traverse07(): Bool = traverse(x -> if (x == 2) Success(x + 1) else Failure(Nel(x, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(1, 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverseX                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseX01(): Bool = traverseX(x -> Success(x + 1), 1 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX02(): Bool = traverseX(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX03(): Bool = traverseX(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX04(): Bool = traverseX(x -> Failure(Nel(x, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(1, 2 :: 3 :: Nil))

    @test
    def traverseX05(): Bool = traverseX(x -> Failure(Nel(x, x :: Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(1, 1 :: 2 :: 2 :: 3 :: 3 :: Nil))

    @test
    def traverseX06(): Bool = traverseX(x -> if (x != 2) Success(x + 1) else Failure(Nel(42, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(42, Nil))

    @test
    def traverseX07(): Bool = traverseX(x -> if (x == 2) Success(x + 1) else Failure(Nel(x, Nil)), 1 :: 2 :: 3 :: Nil) == Failure(Nel(1, 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOption01(): Bool = (Success(123)).toOption() == Some(123)

    @test
    def toOption02(): Bool = (Failure(Nel(42, Nil))).toOption(): Option[Int32] == None

    /////////////////////////////////////////////////////////////////////////////
    // toResult                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toResult01(): Bool = (Success(123)).toResult(): Result[_, Nel[Unit]] == Ok(123)

    @test
    def toResult02(): Bool = (Failure(Nel(42, Nil))).toResult(): Result[Unit, _] == Err(Nel(42, Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = (Success(123)).toList() == 123 :: Nil

    @test
    def toList02(): Bool = (Failure(Nel(42, Nil)): Validation[Unit, _]).toList() == Nil

/////////////////////////////////////////////////////////////////////////////
// lift2                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift201(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Success(123), Success(1)): Validation[_, Unit] == Success(124)

@test
def lift202(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Success(123), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift203(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Failure(Nel("e1", Nil)), Success(1)) == Failure(Nel("e1", Nil))

@test
def lift204(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift3                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift301(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Success(2)): Validation[_, Unit] == Success(126)

@test
def lift302(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift303(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(Nel("e1", Nil)), Success(2)) == Failure(Nel("e1", Nil))

@test
def lift304(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(Nel("e1", Nil)), Success(1), Success(2)) == Failure(Nel("e1", Nil))

@test
def lift305(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil))) == Failure(Nel("e1", "e2" :: Nil))

@test
def lift306(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(Nel("e1", Nil)), Success(1), Failure(Nel("e2", Nil))) == Failure(Nel("e1", "e2" :: Nil))

@test
def lift307(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift4                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift401(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Success(3)): Validation[_, Unit] == Success(129)

@test
def lift402(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift403(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Failure(Nel("e1", Nil)), Success(3)) == Failure(Nel("e1", Nil))

@test
def lift404(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Failure(Nel("e1", Nil)), Success(2), Success(3)) == Failure(Nel("e1", Nil))

@test
def lift405(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(Nel("e1", Nil)), Success(1), Success(2), Success(3)) == Failure(Nel("e1", Nil))

@test
def lift406(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)), Failure(Nel("e4", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: "e4" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift5                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift501(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Success(4)): Validation[_, Unit] == Success(133)

@test
def lift502(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift503(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Failure(Nel("e1", Nil)), Success(4)) == Failure(Nel("e1", Nil))

@test
def lift504(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Failure(Nel("e1", Nil)), Success(3), Success(4)) == Failure(Nel("e1", Nil))

@test
def lift505(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Failure(Nel("e1", Nil)), Success(2), Success(3), Success(4)) == Failure(Nel("e1", Nil))

@test
def lift506(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(Nel("e1", Nil)), Success(1), Success(2), Success(3), Success(4)) == Failure(Nel("e1", Nil))

@test
def lift507(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)), Failure(Nel("e4", Nil)), Failure(Nel("e5", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: "e4" :: "e5" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift6                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift601(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5)): Validation[_, Unit] == Success(138)

@test
def lift602(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift603(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Failure(Nel("e1", Nil)), Success(5)) == Failure(Nel("e1", Nil))

@test
def lift604(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Failure(Nel("e1", Nil)), Success(4), Success(5)) == Failure(Nel("e1", Nil))

@test
def lift605(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Failure(Nel("e1", Nil)), Success(3), Success(4), Success(5)) == Failure(Nel("e1", Nil))

@test
def lift606(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Failure(Nel("e1", Nil)), Success(2), Success(3), Success(4), Success(5)) == Failure(Nel("e1", Nil))

@test
def lift607(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(Nel("e1", Nil)), Success(1), Success(2), Success(3), Success(4), Success(5)) == Failure(Nel("e1", Nil))

@test
def lift608(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)), Failure(Nel("e4", Nil)), Failure(Nel("e5", Nil)), Failure(Nel("e6", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: "e4" :: "e5" :: "e6" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift7                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift701(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)): Validation[_, Unit] == Success(144)

@test
def lift702(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift703(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(Nel("e1", Nil)), Success(6)) == Failure(Nel("e1", Nil))

@test
def lift704(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Failure(Nel("e1", Nil)), Success(5), Success(6)) == Failure(Nel("e1", Nil))

@test
def lift705(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Failure(Nel("e1", Nil)), Success(4), Success(5), Success(6)) == Failure(Nel("e1", Nil))

@test
def lift706(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Failure(Nel("e1", Nil)), Success(3), Success(4), Success(5), Success(6)) == Failure(Nel("e1", Nil))

@test
def lift707(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Failure(Nel("e1", Nil)), Success(2), Success(3), Success(4), Success(5), Success(6)) == Failure(Nel("e1", Nil))

@test
def lift708(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(Nel("e1", Nil)), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)) == Failure(Nel("e1", Nil))

@test
def lift709(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)), Failure(Nel("e4", Nil)), Failure(Nel("e5", Nil)), Failure(Nel("e6", Nil)), Failure(Nel("e7", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: "e4" :: "e5" :: "e6" :: "e7" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift8                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift801(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)): Validation[_, Unit] == Success(151)

@test
def lift802(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift803(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(Nel("e1", Nil)), Success(7)) == Failure(Nel("e1", Nil))

@test
def lift804(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(Nel("e1", Nil)), Success(6), Success(7)) == Failure(Nel("e1", Nil))

@test
def lift805(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Failure(Nel("e1", Nil)), Success(5), Success(6), Success(7)) == Failure(Nel("e1", Nil))

@test
def lift806(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Failure(Nel("e1", Nil)), Success(4), Success(5), Success(6), Success(7)) == Failure(Nel("e1", Nil))

@test
def lift807(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Failure(Nel("e1", Nil)), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(Nel("e1", Nil))

@test
def lift808(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Failure(Nel("e1", Nil)), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(Nel("e1", Nil))

@test
def lift809(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(Nel("e1", Nil)), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(Nel("e1", Nil))

@test
def lift810(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)), Failure(Nel("e4", Nil)), Failure(Nel("e5", Nil)), Failure(Nel("e6", Nil)), Failure(Nel("e7", Nil)), Failure(Nel("e8", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: "e4" :: "e5" :: "e6" :: "e7" :: "e8" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift9                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift901(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)): Validation[_, Unit] == Success(159)

@test
def lift902(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift903(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(Nel("e1", Nil)), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift904(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(Nel("e1", Nil)), Success(7), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift905(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(Nel("e1", Nil)), Success(6), Success(7), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift906(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Failure(Nel("e1", Nil)), Success(5), Success(6), Success(7), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift907(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Failure(Nel("e1", Nil)), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift908(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Failure(Nel("e1", Nil)), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift909(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Failure(Nel("e1", Nil)), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift910(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(Nel("e1", Nil)), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(Nel("e1", Nil))

@test
def lift911(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)), Failure(Nel("e4", Nil)), Failure(Nel("e5", Nil)), Failure(Nel("e6", Nil)), Failure(Nel("e7", Nil)), Failure(Nel("e8", Nil)), Failure(Nel("e9", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: "e4" :: "e5" :: "e6" :: "e7" :: "e8" :: "e9" :: Nil))

/////////////////////////////////////////////////////////////////////////////
// lift10                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def lift1001(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)): Validation[_, Unit] == Success(168)

@test
def lift1002(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Failure(Nel("e1", Nil))) == Failure(Nel("e1", Nil))

@test
def lift1003(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(Nel("e1", Nil)), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1004(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(Nel("e1", Nil)), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1005(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(Nel("e1", Nil)), Success(7), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1006(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(Nel("e1", Nil)), Success(6), Success(7), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1007(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Failure(Nel("e1", Nil)), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1008(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Failure(Nel("e1", Nil)), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1009(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Failure(Nel("e1", Nil)), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1010(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Failure(Nel("e1", Nil)), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1011(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(Nel("e1", Nil)), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(Nel("e1", Nil))

@test
def lift1012(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(Nel("e1", Nil)), Failure(Nel("e2", Nil)), Failure(Nel("e3", Nil)), Failure(Nel("e4", Nil)), Failure(Nel("e5", Nil)), Failure(Nel("e6", Nil)), Failure(Nel("e7", Nil)), Failure(Nel("e8", Nil)), Failure(Nel("e9", Nil)), Failure(Nel("e10", Nil)): Validation[Int32, String]) == Failure(Nel("e1", "e2" :: "e3" :: "e4" :: "e5" :: "e6" :: "e7" :: "e8" :: "e9" :: "e10" :: Nil))

}