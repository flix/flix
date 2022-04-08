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

namespace TestValidation {

    use Hash.hash;
    use Nec.{cons, singleton};

    def necOf2(x: a, y: a): Nec[a] = cons(x, singleton(y))

    def necOf3(w: a, x: a, y: a): Nec[a] = cons(w, cons(x, singleton(y)))

    def necOf4(v: a, w: a, x: a, y: a): Nec[a] = cons(v, cons(w, cons(x, singleton(y))))

    def necOf5(u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(u, cons(v, cons(w, cons(x, singleton(y)))))

    def necOf6(t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))

    def necOf7(s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y)))))))

    def necOf8(r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(r, cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))))

    def necOf9(q: a, r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(q, cons(r, cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y)))))))))

    def necOf10(p: a, q: a, r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(p, cons(q, cons(r, cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))))))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = Validation.ap(Success(x -> x + 1), Success(123)): Validation[_, Unit] == Success(124)

    @test
    def ap02(): Bool = Validation.ap(Success(x -> x + 1), Failure(singleton(42))) == Failure(singleton(42))

    @test
    def ap03(): Bool = Validation.ap(Failure(singleton(42)), Success(123)): Validation[Unit, _] == Failure(singleton(42))

    @test
    def ap04(): Bool = Validation.ap(Failure(singleton(123)), (Failure(singleton(456)))): Validation[Unit, _] == Failure(necOf2(123, 456))

    @test
    def ap05(): Bool = Validation.ap(Failure(necOf2(1, 2)), (Failure(singleton(3)))): Validation[Unit, _] == Failure(necOf3(1, 2, 3))

    @test
    def ap06(): Bool = Validation.ap(Failure(singleton(1)), (Failure(necOf2(2, 3)))): Validation[Unit, _] == Failure(necOf3(1, 2, 3))

    @test
    def ap07(): Bool = Validation.ap(Failure(necOf2(1, 2)), (Failure(necOf2(3, 4)))): Validation[Unit, _] == Failure(necOf4(1, 2, 3, 4))

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool =
    let p = Validation.product(Failure(singleton(1)), (Failure(singleton(2))));
    match p {
        case Success(_) => false
        case Failure(v) => v == necOf2(1, 2)
    }

    @test
    def product02(): Bool =
        let p = Validation.product(Failure(singleton(1)), Success(2));
        match p {
            case Success(_) => false
            case Failure(v) => v == singleton(1)
        }

    @test
    def product03(): Bool =
        let p = Validation.product(Success(1), Failure(singleton(2)));
        match p {
            case Success(_) => false
            case Failure(v) => v == singleton(2)
        }

    @test
    def product04(): Bool =
        let p = Validation.product(Success(1), Success(2));
        match p {
            case Success(v) => v == (1, 2)
            case Failure(_) => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // product3                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product301(): Bool =
        let p = Validation.product3(Success(1), Success(2), Success(3));
        match p {
            case Success(v) => v == (1, 2, 3)
            case Failure(_) => false
        }

    @test
    def product302(): Bool =
        let p = Validation.product3(Success(1), Failure(singleton(2)), Success(3));
        match p {
            case Success(_) => false
            case Failure(v) => v == singleton(2)
        }

    @test
    def product303(): Bool =
        let p = Validation.product3(Failure(singleton(1)), Success(2), Failure(singleton(3)));
        match p {
            case Success(_) => false
            case Failure(v) => v == necOf2(1, 3)
        }

    @test
    def product304(): Bool =
        let p = Validation.product3(Failure(singleton(1)), Failure(singleton(2)), Failure(singleton(3)));
        match p {
            case Success(_) => false
            case Failure(v) => v == necOf3(1, 2, 3)
        }

    /////////////////////////////////////////////////////////////////////////////
    // product4                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product401(): Bool =
        let p = Validation.product4(Success(1), Success(2), Success(3), Success(4));
        match p {
            case Success(v) => v == (1, 2, 3, 4)
            case Failure(_) => false
        }

    @test
    def product402(): Bool =
        let p = Validation.product4(Success(1), Failure(singleton(2)), Success(3), Success(4));
        match p {
            case Success(_) => false
            case Failure(v) => v == singleton(2)
        }

    @test
    def product403(): Bool =
        let p = Validation.product4(Failure(singleton(1)), Success(2), Failure(singleton(3)), Success(4));
        match p {
            case Success(_) => false
            case Failure(v) => v == necOf2(1, 3)
        }

    @test
    def product404(): Bool =
        let p = Validation.product4(Failure(singleton(1)), Failure(singleton(2)), Failure(singleton(3)), Failure(singleton(4)));
        match p {
            case Success(_) => false
            case Failure(v) => v == necOf4(1, 2, 3, 4)
        }

    /////////////////////////////////////////////////////////////////////////////
    // product5                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product501(): Bool =
        let p = Validation.product5(Success(1), Success(2), Success(3), Success(4), Success(5));
        match p {
            case Success(v) => v == (1, 2, 3, 4, 5)
            case Failure(_) => false
        }

    @test
    def product502(): Bool =
        let p = Validation.product5(Success(1), Failure(singleton(2)), Success(3), Success(4), Success(5));
        match p {
            case Success(_) => false
            case Failure(v) => v == singleton(2)
        }

    @test
    def product503(): Bool =
        let p = Validation.product5(Failure(singleton(1)), Success(2), Failure(singleton(3)), Success(4), Failure(singleton(5)));
        match p {
            case Success(_) => false
            case Failure(v) => v == necOf3(1, 3, 5)
        }

    @test
    def product504(): Bool =
        let p = Validation.product5(Failure(singleton(1)), Failure(singleton(2)), Failure(singleton(3)), Failure(singleton(4)), Failure(singleton(5)));
        match p {
            case Success(_) => false
            case Failure(v) => v == necOf5(1, 2, 3, 4, 5)
        }

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getWithDefault01(): Bool = Validation.getWithDefault(456, Success(123)) == 123

    @test
    def getWithDefault02(): Bool = Validation.getWithDefault(456, Failure(singleton(42))) == 456

    /////////////////////////////////////////////////////////////////////////////
    // withDefault                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def withDefault01(): Bool = Validation.withDefault(default = Success(2), Success(1)): Validation[_, Unit] == Success(1)

    @test
    def withDefault02(): Bool = Validation.withDefault(default = Failure(singleton(1)), Success(1)) == Success(1)

    @test
    def withDefault03(): Bool = Validation.withDefault(default = Success(2), Failure(singleton(1))) == Success(2)

    @test
    def withDefault04(): Bool = Validation.withDefault(default = Failure(singleton(2)), Failure(singleton(1))): Validation[Unit, _] == Failure(singleton(2))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = Validation.exists(x -> x == 123, (Success(123))) == true

    @test
    def exists02(): Bool = Validation.exists(x -> x != 123, (Success(123))) == false

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = Validation.exists(x -> x == 123, (Success(123))) == true

    @test
    def forall02(): Bool = Validation.exists(x -> x != 123, (Success(123))) == false

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = Validation.map(x -> x, Success(123)): Validation[_, Unit] == Success(123)

    @test
    def map02(): Bool = Validation.map(x -> x + 1, Success(123)): Validation[_, Unit] == Success(124)

    @test
    def map03(): Bool = Validation.map(x -> x, Failure(singleton(123))): Validation[Unit, _] == Failure(singleton(123))

    @test
    def map04(): Bool = (Success(123): Validation[_, Unit]) |> Validation.map(x -> x + 1) |> Validation.map(x -> x * 2)  == Success(248)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool = Validation.sequence(Success(1) :: Nil): Validation[_, Unit] == Success(1 :: Nil)

    @test
    def sequence02(): Bool = Validation.sequence(Success(1) :: Success(2) :: Nil): Validation[_, Unit] == Success(1 :: 2 :: Nil)

    @test
    def sequence03(): Bool = Validation.sequence(Success(1) :: Success(2) :: Success(3) :: Nil): Validation[_, Unit] == Success(1 :: 2 :: 3 :: Nil)

    @test
    def sequence04(): Bool = Validation.sequence(Failure(singleton(1)) :: Nil): Validation[List[Unit], _] == Failure(singleton(1))

    @test
    def sequence05(): Bool = Validation.sequence(Failure(singleton(1)) :: Failure(singleton(2)) :: Nil): Validation[List[Unit], _] == Failure(necOf2(1, 2))

    @test
    def sequence06(): Bool = Validation.sequence(Success(1) :: Failure(singleton(42)) :: Nil) == Failure(singleton(42))

    @test
    def sequence07(): Bool = Validation.sequence(Failure(singleton(42)) :: Success(1) :: Nil) == Failure(singleton(42))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = Validation.traverse(x -> Success(x + 1), 1 :: Nil): Validation[_, Unit] == Success(2 :: Nil)

    @test
    def traverse02(): Bool = Validation.traverse(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[_, Unit] == Success(2 :: 3 :: Nil)

    @test
    def traverse03(): Bool = Validation.traverse(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[_, Unit] == Success(2 :: 3 :: 4 :: Nil)

    @test
    def traverse04(): Bool = Validation.traverse(x -> Failure(singleton(x)), 1 :: 2 :: 3 :: Nil): Validation[List[Unit], _] == Failure(necOf3(1, 2, 3))

    @test
    def traverse05(): Bool = Validation.traverse(x -> Failure(necOf2(x, x)), 1 :: 2 :: 3 :: Nil): Validation[List[Unit], _] == Failure(necOf6(1, 1, 2, 2, 3, 3))

    @test
    def traverse06(): Bool = Validation.traverse(x -> if (x != 2) Success(x + 1) else Failure(singleton(42)), 1 :: 2 :: 3 :: Nil) == Failure(singleton(42))

    @test
    def traverse07(): Bool = Validation.traverse(x -> if (x == 2) Success(x + 1) else Failure(singleton(x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf2(1, 3))

    /////////////////////////////////////////////////////////////////////////////
    // traverseX                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseX01(): Bool = Validation.traverseX(x -> Success(x + 1), 1 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX02(): Bool = Validation.traverseX(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX03(): Bool = Validation.traverseX(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[_, Unit] == Success()

    @test
    def traverseX04(): Bool = Validation.traverseX(x -> Failure(singleton(x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf3(1, 2, 3))

    @test
    def traverseX05(): Bool = Validation.traverseX(x -> Failure(necOf2(x, x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf6(1, 1, 2, 2, 3, 3))

    @test
    def traverseX06(): Bool = Validation.traverseX(x -> if (x != 2) Success(x + 1) else Failure(singleton(42)), 1 :: 2 :: 3 :: Nil) == Failure(singleton(42))

    @test
    def traverseX07(): Bool = Validation.traverseX(x -> if (x == 2) Success(x + 1) else Failure(singleton(x)), 1 :: 2 :: 3 :: Nil) == Failure(necOf2(1, 3))

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOption01(): Bool = (Success(123) |> Validation.toOption) == Some(123)

    @test
    def toOption02(): Bool = (Failure(singleton(42)) |> Validation.toOption): Option[Int32] == None

    /////////////////////////////////////////////////////////////////////////////
    // toResult                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toResult01(): Bool = (Success(123) |> Validation.toResult): Result[_, Nec[Unit]] == Ok(123)

    @test
    def toResult02(): Bool = (Failure(singleton(42)) |> Validation.toResult): Result[Unit, _] == Err(singleton(42))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = (Success(123) |> Validation.toList) == 123 :: Nil

    @test
    def toList02(): Bool = ((Failure(singleton(42)): Validation[Unit, _]) |> Validation.toList) == Nil

    /////////////////////////////////////////////////////////////////////////////
    // lift2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift201(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Success(123), Success(1)): Validation[_, Unit] == Success(124)

    @test
    def lift202(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Success(123), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift203(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Failure(singleton("e1")), Success(1)) == Failure(singleton("e1"))

    @test
    def lift204(): Bool = Validation.lift2((x1,x2) -> x1 + x2, Failure(singleton("e1")), Failure(singleton("e2")): Validation[Int32, String]) == Failure(necOf2("e1", "e2"))

    /////////////////////////////////////////////////////////////////////////////
    // lift3                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift301(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Success(2)): Validation[_, Unit] == Success(126)

    @test
    def lift302(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift303(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(singleton("e1")), Success(2)) == Failure(singleton("e1"))

    @test
    def lift304(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Success(1), Success(2)) == Failure(singleton("e1"))

    @test
    def lift305(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(singleton("e1")), Failure(singleton("e2"))) == Failure(necOf2("e1", "e2"))

    @test
    def lift306(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Success(1), Failure(singleton("e2"))) == Failure(necOf2("e1", "e2"))

    @test
    def lift307(): Bool = Validation.lift3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")): Validation[Int32, String]) == Failure(necOf3("e1", "e2", "e3"))

    /////////////////////////////////////////////////////////////////////////////
    // lift4                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift401(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Success(3)): Validation[_, Unit] == Success(129)

    @test
    def lift402(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift403(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Failure(singleton("e1")), Success(3)) == Failure(singleton("e1"))

    @test
    def lift404(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Failure(singleton("e1")), Success(2), Success(3)) == Failure(singleton("e1"))

    @test
    def lift405(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(singleton("e1")), Success(1), Success(2), Success(3)) == Failure(singleton("e1"))

    @test
    def lift406(): Bool = Validation.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")): Validation[Int32, String]) == Failure(necOf4("e1", "e2", "e3", "e4"))

    /////////////////////////////////////////////////////////////////////////////
    // lift5                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift501(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Success(4)): Validation[_, Unit] == Success(133)

    @test
    def lift502(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift503(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4)) == Failure(singleton("e1"))

    @test
    def lift504(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4)) == Failure(singleton("e1"))

    @test
    def lift505(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4)) == Failure(singleton("e1"))

    @test
    def lift506(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4)) == Failure(singleton("e1"))

    @test
    def lift507(): Bool = Validation.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")): Validation[Int32, String]) == Failure(necOf5("e1", "e2", "e3", "e4", "e5"))

    /////////////////////////////////////////////////////////////////////////////
    // lift6                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift601(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5)): Validation[_, Unit] == Success(138)

    @test
    def lift602(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift603(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5)) == Failure(singleton("e1"))

    @test
    def lift604(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift605(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift606(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift607(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5)) == Failure(singleton("e1"))

    @test
    def lift608(): Bool = Validation.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")): Validation[Int32, String]) == Failure(necOf6("e1", "e2", "e3", "e4", "e5", "e6"))

    /////////////////////////////////////////////////////////////////////////////
    // lift7                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift701(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)): Validation[_, Unit] == Success(144)

    @test
    def lift702(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift703(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6)) == Failure(singleton("e1"))

    @test
    def lift704(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift705(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift706(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift707(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift708(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)) == Failure(singleton("e1"))

    @test
    def lift709(): Bool = Validation.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")): Validation[Int32, String]) == Failure(necOf7("e1", "e2", "e3", "e4", "e5", "e6", "e7"))

    /////////////////////////////////////////////////////////////////////////////
    // lift8                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift801(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)): Validation[_, Unit] == Success(151)

    @test
    def lift802(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift803(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7)) == Failure(singleton("e1"))

    @test
    def lift804(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift805(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift806(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift807(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift808(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift809(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)) == Failure(singleton("e1"))

    @test
    def lift810(): Bool = Validation.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")): Validation[Int32, String]) == Failure(necOf8("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8"))

    /////////////////////////////////////////////////////////////////////////////
    // lift9                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift901(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)): Validation[_, Unit] == Success(159)

    @test
    def lift902(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift903(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1")), Success(8)) == Failure(singleton("e1"))

    @test
    def lift904(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift905(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift906(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift907(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift908(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift909(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift910(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)) == Failure(singleton("e1"))

    @test
    def lift911(): Bool = Validation.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")), Failure(singleton("e9")): Validation[Int32, String]) == Failure(necOf9("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9"))

    /////////////////////////////////////////////////////////////////////////////
    // lift10                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lift1001(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)): Validation[_, Unit] == Success(168)

    @test
    def lift1002(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Failure(singleton("e1"))) == Failure(singleton("e1"))

    @test
    def lift1003(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(singleton("e1")), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1004(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1")), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1005(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1006(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1007(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1008(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1009(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1010(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1011(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)) == Failure(singleton("e1"))

    @test
    def lift1012(): Bool = Validation.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")), Failure(singleton("e9")), Failure(singleton("e10")): Validation[Int32, String]) == Failure(necOf10("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10"))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash(Success(()): Validation[_, Unit]) == hash(Success(()): Validation[_, Unit])

    @test
    def hash02(): Bool = hash(Success(()): Validation[_, Unit]) != hash(Failure(necOf2((), ())): Validation[Unit, _])

    @test
    def hash03(): Bool = hash(Success(1, 2): Validation[_, Unit]) == hash(Success(1, 2): Validation[_, Unit])

    @test
    def hash04(): Bool = hash(Success(1, 2, 3): Validation[_, Unit]) != hash(Success(1, 3, 2): Validation[_, Unit])

    @test
    def hash05(): Bool = hash(Success(singleton(1)): Validation[_, Int32]) != hash(Failure(singleton(1)): Validation[Nec[Int32], _])

}
