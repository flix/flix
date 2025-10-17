mod TestValidation {

    use Assert.{assertEq, assertTrue, assertFalse, fail}
    use Hash.hash
    use Nec.{cons, singleton}
    use Validation.{Success, Failure}

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
    // Functor.map                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def functor_map01(): Unit \ Assert =
        assertEq(expected = Success(123), (Functor.map(x -> x, Success(123)): Validation[Int32, Int32]))

    @Test
    def functor_map02(): Unit \ Assert =
        assertEq(expected = Success(124), (Functor.map(x -> x + 1, Success(123)): Validation[Int32, Int32]))

    @Test
    def functor_map03(): Unit \ Assert =
        assertEq(expected = Failure(singleton(123)), (Functor.map(x -> x, Failure(singleton(123))): Validation[Int32, Int32]))

    @Test
    def functor_map04(): Unit \ Assert =
        assertEq(expected = Success(248), (Success(123): Validation[Int32, Int32]) |> Functor.map(x -> x + 1) |> Functor.map(x -> x * 2))

    /////////////////////////////////////////////////////////////////////////////
    // Applicative.ap                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def applicative_ap01(): Unit \ Assert =
        assertEq(expected = Success(124), (Applicative.ap(Success(x -> x + 1), Success(123)): Validation[Int32, Int32]))

    @Test
    def applicative_ap02(): Unit \ Assert =
        assertEq(expected = Failure(singleton(42)), Applicative.ap(Success(x -> x + 1), Failure(singleton(42))))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Unit \ Assert =
        assertEq(expected = Success(124), (Validation.ap(Success(x -> x + 1), Success(123)): Validation[Int32, Int32]))

    @Test
    def ap02(): Unit \ Assert =
        assertEq(expected = Failure(singleton(42)), Validation.ap(Success(x -> x + 1), Failure(singleton(42))))

    @Test
    def ap03(): Unit \ Assert =
        assertEq(expected = Failure(singleton(42)), (Validation.ap(Failure(singleton(42)), Success(123)): Validation[Int32, Int32]))

    @Test
    def ap04(): Unit \ Assert =
        assertEq(expected = Failure(necOf2(123, 456)), (Validation.ap(Failure(singleton(123)), (Failure(singleton(456)))): Validation[Int32, Int32]))

    @Test
    def ap05(): Unit \ Assert =
        assertEq(expected = Failure(necOf3(1, 2, 3)), (Validation.ap(Failure(necOf2(1, 2)), (Failure(singleton(3)))): Validation[Int32, Int32]))

    @Test
    def ap06(): Unit \ Assert =
        assertEq(expected = Failure(necOf3(1, 2, 3)), (Validation.ap(Failure(singleton(1)), (Failure(necOf2(2, 3)))): Validation[Int32, Int32]))

    @Test
    def ap07(): Unit \ Assert =
        assertEq(expected = Failure(necOf4(1, 2, 3, 4)), (Validation.ap(Failure(necOf2(1, 2)), (Failure(necOf2(3, 4)))): Validation[Int32, Int32]))

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product01(): Unit \ Assert =
        let p = Validation.product(Failure(singleton(1)), (Failure(singleton(2))));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = necOf2(1, 2), v)
        }

    @Test
    def product02(): Unit \ Assert =
        let p = Validation.product(Failure(singleton(1)), Success(2));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = singleton(1), v)
        }

    @Test
    def product03(): Unit \ Assert =
        let p = Validation.product(Success(1), Failure(singleton(2)));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = singleton(2), v)
        }

    @Test
    def product04(): Unit \ Assert =
        let p = Validation.product(Success(1), Success(2));
        match p {
            case Success(v) => assertEq(expected = (1, 2), v)
            case Failure(_) => fail("Expected Success but got Failure")
        }

    /////////////////////////////////////////////////////////////////////////////
    // product3                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product301(): Unit \ Assert =
        let p = Validation.product3(Success(1), Success(2), Success(3));
        match p {
            case Success(v) => assertEq(expected = (1, 2, 3), v)
            case Failure(_) => fail("Expected Success but got Failure")
        }

    @Test
    def product302(): Unit \ Assert =
        let p = Validation.product3(Success(1), Failure(singleton(2)), Success(3));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = singleton(2), v)
        }

    @Test
    def product303(): Unit \ Assert =
        let p = Validation.product3(Failure(singleton(1)), Success(2), Failure(singleton(3)));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = necOf2(1, 3), v)
        }

    @Test
    def product304(): Unit \ Assert =
        let p = Validation.product3(Failure(singleton(1)), Failure(singleton(2)), Failure(singleton(3)));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = necOf3(1, 2, 3), v)
        }

    /////////////////////////////////////////////////////////////////////////////
    // product4                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product401(): Unit \ Assert =
        let p = Validation.product4(Success(1), Success(2), Success(3), Success(4));
        match p {
            case Success(v) => assertEq(expected = (1, 2, 3, 4), v)
            case Failure(_) => fail("Expected Success but got Failure")
        }

    @Test
    def product402(): Unit \ Assert =
        let p = Validation.product4(Success(1), Failure(singleton(2)), Success(3), Success(4));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = singleton(2), v)
        }

    @Test
    def product403(): Unit \ Assert =
        let p = Validation.product4(Failure(singleton(1)), Success(2), Failure(singleton(3)), Success(4));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = necOf2(1, 3), v)
        }

    @Test
    def product404(): Unit \ Assert =
        let p = Validation.product4(Failure(singleton(1)), Failure(singleton(2)), Failure(singleton(3)), Failure(singleton(4)));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = necOf4(1, 2, 3, 4), v)
        }

    /////////////////////////////////////////////////////////////////////////////
    // product5                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def product501(): Unit \ Assert =
        let p = Validation.product5(Success(1), Success(2), Success(3), Success(4), Success(5));
        match p {
            case Success(v) => assertEq(expected = (1, 2, 3, 4, 5), v)
            case Failure(_) => fail("Expected Success but got Failure")
        }

    @Test
    def product502(): Unit \ Assert =
        let p = Validation.product5(Success(1), Failure(singleton(2)), Success(3), Success(4), Success(5));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = singleton(2), v)
        }

    @Test
    def product503(): Unit \ Assert =
        let p = Validation.product5(Failure(singleton(1)), Success(2), Failure(singleton(3)), Success(4), Failure(singleton(5)));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = necOf3(1, 3, 5), v)
        }

    @Test
    def product504(): Unit \ Assert =
        let p = Validation.product5(Failure(singleton(1)), Failure(singleton(2)), Failure(singleton(3)), Failure(singleton(4)), Failure(singleton(5)));
        match p {
            case Success(_) => fail("Expected Failure but got Success")
            case Failure(v) => assertEq(expected = necOf5(1, 2, 3, 4, 5), v)
        }

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getWithDefault01(): Unit \ Assert =
        assertEq(expected = 123, Validation.getWithDefault(456, Success(123)))

    @Test
    def getWithDefault02(): Unit \ Assert =
        assertEq(expected = 456, Validation.getWithDefault(456, Failure(singleton(42))))

    /////////////////////////////////////////////////////////////////////////////
    // withDefault                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def withDefault01(): Unit \ Assert =
        assertEq(expected = Success(1), (Validation.withDefault(default = Success(2), Success(1)): Validation[Int32, Int32]))

    @Test
    def withDefault02(): Unit \ Assert =
        assertEq(expected = Success(1), Validation.withDefault(default = Failure(singleton(1)), Success(1)))

    @Test
    def withDefault03(): Unit \ Assert =
        assertEq(expected = Success(2), Validation.withDefault(default = Success(2), Failure(singleton(1))))

    @Test
    def withDefault04(): Unit \ Assert =
        assertEq(expected = Failure(singleton(2)), (Validation.withDefault(default = Failure(singleton(2)), Failure(singleton(1))): Validation[Int32, Int32]))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert =
        assertTrue(Validation.exists(x -> x == 123, (Success(123))))

    @Test
    def exists02(): Unit \ Assert =
        assertFalse(Validation.exists(x -> x != 123, (Success(123))))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert =
        assertTrue(Validation.exists(x -> x == 123, (Success(123))))

    @Test
    def forAll02(): Unit \ Assert =
        assertFalse(Validation.exists(x -> x != 123, (Success(123))))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert =
        assertEq(expected = Success(123), (Validation.map(x -> x, Success(123)): Validation[Int32, Int32]))

    @Test
    def map02(): Unit \ Assert =
        assertEq(expected = Success(124), (Validation.map(x -> x + 1, Success(123)): Validation[Int32, Int32]))

    @Test
    def map03(): Unit \ Assert =
        assertEq(expected = Failure(singleton(123)), (Validation.map(x -> x, Failure(singleton(123))): Validation[Int32, Int32]))

    @Test
    def map04(): Unit \ Assert =
        assertEq(expected = Success(248), (Success(123): Validation[Int32, Int32]) |> Validation.map(x -> x + 1) |> Validation.map(x -> x * 2))

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Unit \ Assert =
        assertEq(expected = Success(1 :: Nil), (Validation.sequence(Success(1) :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def sequence02(): Unit \ Assert =
        assertEq(expected = Success(1 :: 2 :: Nil), (Validation.sequence(Success(1) :: Success(2) :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def sequence03(): Unit \ Assert =
        assertEq(expected = Success(1 :: 2 :: 3 :: Nil), (Validation.sequence(Success(1) :: Success(2) :: Success(3) :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def sequence04(): Unit \ Assert =
        assertEq(expected = Failure(singleton(1)), (Validation.sequence(Failure(singleton(1)) :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def sequence05(): Unit \ Assert =
        assertEq(expected = Failure(necOf2(1, 2)), (Validation.sequence(Failure(singleton(1)) :: Failure(singleton(2)) :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def sequence06(): Unit \ Assert =
        assertEq(expected = Failure(singleton(42)), Validation.sequence(Success(1) :: Failure(singleton(42)) :: Nil))

    @Test
    def sequence07(): Unit \ Assert =
        assertEq(expected = Failure(singleton(42)), Validation.sequence(Failure(singleton(42)) :: Success(1) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Unit \ Assert =
        assertEq(expected = Success(2 :: Nil), (Validation.traverse(x -> Success(x + 1), 1 :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def traverse02(): Unit \ Assert =
        assertEq(expected = Success(2 :: 3 :: Nil), (Validation.traverse(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def traverse03(): Unit \ Assert =
        assertEq(expected = Success(2 :: 3 :: 4 :: Nil), (Validation.traverse(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def traverse04(): Unit \ Assert =
        assertEq(expected = Failure(necOf3(1, 2, 3)), (Validation.traverse(x -> Failure(singleton(x)), 1 :: 2 :: 3 :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def traverse05(): Unit \ Assert =
        assertEq(expected = Failure(necOf6(1, 1, 2, 2, 3, 3)), (Validation.traverse(x -> Failure(necOf2(x, x)), 1 :: 2 :: 3 :: Nil): Validation[Int32, List[Int32]]))

    @Test
    def traverse06(): Unit \ Assert =
        assertEq(expected = Failure(singleton(42)), Validation.traverse(x -> if (x != 2) Success(x + 1) else Failure(singleton(42)), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse07(): Unit \ Assert =
        assertEq(expected = Failure(necOf2(1, 3)), Validation.traverse(x -> if (x == 2) Success(x + 1) else Failure(singleton(x)), 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverseX                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseX01(): Unit \ Assert =
        assertEq(expected = Success(), (Validation.traverseX(x -> Success(x + 1), 1 :: Nil): Validation[Int32, Unit]))

    @Test
    def traverseX02(): Unit \ Assert =
        assertEq(expected = Success(), (Validation.traverseX(x -> Success(x + 1), 1 :: 2 :: Nil): Validation[Int32, Unit]))

    @Test
    def traverseX03(): Unit \ Assert =
        assertEq(expected = Success(), (Validation.traverseX(x -> Success(x + 1), 1 :: 2 :: 3 :: Nil): Validation[Int32, Unit]))

    @Test
    def traverseX04(): Unit \ Assert =
        assertEq(expected = Failure(necOf3(1, 2, 3)), Validation.traverseX(x -> Failure(singleton(x)), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX05(): Unit \ Assert =
        assertEq(expected = Failure(necOf6(1, 1, 2, 2, 3, 3)), Validation.traverseX(x -> Failure(necOf2(x, x)), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX06(): Unit \ Assert =
        assertEq(expected = Failure(singleton(42)), Validation.traverseX(x -> if (x != 2) Success(x + 1) else Failure(singleton(42)), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX07(): Unit \ Assert =
        assertEq(expected = Failure(necOf2(1, 3)), Validation.traverseX(x -> if (x == 2) Success(x + 1) else Failure(singleton(x)), 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toOption01(): Unit \ Assert =
        assertEq(expected = Some(123), (Success(123) |> Validation.toOption))

    @Test
    def toOption02(): Unit \ Assert =
        assertEq(expected = None, ((Failure(singleton(42)) |> Validation.toOption): Option[Int32]))

    /////////////////////////////////////////////////////////////////////////////
    // toResult                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toResult01(): Unit \ Assert =
        assertEq(expected = Ok(123), ((Success(123) |> Validation.toResult): Result[Nec[Int32], Int32]))

    @Test
    def toResult02(): Unit \ Assert =
        assertEq(expected = Err(singleton(42)), ((Failure(singleton(42)) |> Validation.toResult): Result[Nec[Int32], Int32]))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert =
        assertEq(expected = 123 :: Nil, (Success(123) |> Validation.toList))

    @Test
    def toList02(): Unit \ Assert =
        assertEq(expected = Nil, ((Failure(singleton(42)): Validation[Int32, Int32]) |> Validation.toList))

    /////////////////////////////////////////////////////////////////////////////
    // map2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map201(): Unit \ Assert =
        assertEq(expected = Success(124), (Validation.map2((x1,x2) -> x1 + x2, Success(123), Success(1)): Validation[String, Int32]))

    @Test
    def map202(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map2((x1,x2) -> x1 + x2, Success(123), Failure(singleton("e1"))))

    @Test
    def map203(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map2((x1,x2) -> x1 + x2, Failure(singleton("e1")), Success(1)))

    @Test
    def map204(): Unit \ Assert =
        assertEq(expected = Failure(necOf2("e1", "e2")), Validation.map2((x1,x2) -> x1 + x2, Failure(singleton("e1")), (Failure(singleton("e2")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map3                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map301(): Unit \ Assert =
        assertEq(expected = Success(126), (Validation.map3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Success(2)): Validation[String, Int32]))

    @Test
    def map302(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Success(1), Failure(singleton("e1"))))

    @Test
    def map303(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(singleton("e1")), Success(2)))

    @Test
    def map304(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Success(1), Success(2)))

    @Test
    def map305(): Unit \ Assert =
        assertEq(expected = Failure(necOf2("e1", "e2")), Validation.map3((x1,x2,x3) -> x1 + x2 + x3, Success(123), Failure(singleton("e1")), Failure(singleton("e2"))))

    @Test
    def map306(): Unit \ Assert =
        assertEq(expected = Failure(necOf2("e1", "e2")), Validation.map3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Success(1), Failure(singleton("e2"))))

    @Test
    def map307(): Unit \ Assert =
        assertEq(expected = Failure(necOf3("e1", "e2", "e3")), Validation.map3((x1,x2,x3) -> x1 + x2 + x3, Failure(singleton("e1")), Failure(singleton("e2")), (Failure(singleton("e3")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map4                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map401(): Unit \ Assert =
        assertEq(expected = Success(129), (Validation.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Success(3)): Validation[String, Int32]))

    @Test
    def map402(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Success(2), Failure(singleton("e1"))))

    @Test
    def map403(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Success(1), Failure(singleton("e1")), Success(3)))

    @Test
    def map404(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Success(123), Failure(singleton("e1")), Success(2), Success(3)))

    @Test
    def map405(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(singleton("e1")), Success(1), Success(2), Success(3)))

    @Test
    def map406(): Unit \ Assert =
        assertEq(expected = Failure(necOf4("e1", "e2", "e3", "e4")), Validation.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), (Failure(singleton("e4")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map5                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map501(): Unit \ Assert =
        assertEq(expected = Success(133), (Validation.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Success(4)): Validation[String, Int32]))

    @Test
    def map502(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1"))))

    @Test
    def map503(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4)))

    @Test
    def map504(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4)))

    @Test
    def map505(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4)))

    @Test
    def map506(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4)))

    @Test
    def map507(): Unit \ Assert =
        assertEq(expected = Failure(necOf5("e1", "e2", "e3", "e4", "e5")), Validation.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), (Failure(singleton("e5")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map6                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map601(): Unit \ Assert =
        assertEq(expected = Success(138), (Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5)): Validation[String, Int32]))

    @Test
    def map602(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1"))))

    @Test
    def map603(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5)))

    @Test
    def map604(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5)))

    @Test
    def map605(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5)))

    @Test
    def map606(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5)))

    @Test
    def map607(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5)))

    @Test
    def map608(): Unit \ Assert =
        assertEq(expected = Failure(necOf6("e1", "e2", "e3", "e4", "e5", "e6")), Validation.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), (Failure(singleton("e6")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map7                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map701(): Unit \ Assert =
        assertEq(expected = Success(144), (Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)): Validation[String, Int32]))

    @Test
    def map702(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1"))))

    @Test
    def map703(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6)))

    @Test
    def map704(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6)))

    @Test
    def map705(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6)))

    @Test
    def map706(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6)))

    @Test
    def map707(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6)))

    @Test
    def map708(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6)))

    @Test
    def map709(): Unit \ Assert =
        assertEq(expected = Failure(necOf7("e1", "e2", "e3", "e4", "e5", "e6", "e7")), Validation.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), (Failure(singleton("e7")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map801(): Unit \ Assert =
        assertEq(expected = Success(151), (Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)): Validation[String, Int32]))

    @Test
    def map802(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1"))))

    @Test
    def map803(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7)))

    @Test
    def map804(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7)))

    @Test
    def map805(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7)))

    @Test
    def map806(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7)))

    @Test
    def map807(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7)))

    @Test
    def map808(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)))

    @Test
    def map809(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7)))

    @Test
    def map810(): Unit \ Assert =
        assertEq(expected = Failure(necOf8("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8")), Validation.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), (Failure(singleton("e8")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map9                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map901(): Unit \ Assert =
        assertEq(expected = Success(159), (Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)): Validation[String, Int32]))

    @Test
    def map902(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(singleton("e1"))))

    @Test
    def map903(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1")), Success(8)))

    @Test
    def map904(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7), Success(8)))

    @Test
    def map905(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7), Success(8)))

    @Test
    def map906(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7), Success(8)))

    @Test
    def map907(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7), Success(8)))

    @Test
    def map908(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)))

    @Test
    def map909(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)))

    @Test
    def map910(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8)))

    @Test
    def map911(): Unit \ Assert =
        assertEq(expected = Failure(necOf9("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9")), Validation.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")), (Failure(singleton("e9")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map10                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map1001(): Unit \ Assert =
        assertEq(expected = Success(168), (Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)): Validation[String, Int32]))

    @Test
    def map1002(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Failure(singleton("e1"))))

    @Test
    def map1003(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Failure(singleton("e1")), Success(9)))

    @Test
    def map1004(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Failure(singleton("e1")), Success(8), Success(9)))

    @Test
    def map1005(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Success(5), Failure(singleton("e1")), Success(7), Success(8), Success(9)))

    @Test
    def map1006(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Success(4), Failure(singleton("e1")), Success(6), Success(7), Success(8), Success(9)))

    @Test
    def map1007(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Success(3), Failure(singleton("e1")), Success(5), Success(6), Success(7), Success(8), Success(9)))

    @Test
    def map1008(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Success(2), Failure(singleton("e1")), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)))

    @Test
    def map1009(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Success(1), Failure(singleton("e1")), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)))

    @Test
    def map1010(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Success(123), Failure(singleton("e1")), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)))

    @Test
    def map1011(): Unit \ Assert =
        assertEq(expected = Failure(singleton("e1")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(singleton("e1")), Success(1), Success(2), Success(3), Success(4), Success(5), Success(6), Success(7), Success(8), Success(9)))

    @Test
    def map1012(): Unit \ Assert =
        assertEq(expected = Failure(necOf10("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10")), Validation.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Failure(singleton("e1")), Failure(singleton("e2")), Failure(singleton("e3")), Failure(singleton("e4")), Failure(singleton("e5")), Failure(singleton("e6")), Failure(singleton("e7")), Failure(singleton("e8")), Failure(singleton("e9")), (Failure(singleton("e10")): Validation[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert =
        assertEq(expected = hash((Success(()): Validation[Unit, Unit])), hash((Success(()): Validation[Unit, Unit])))

    @Test
    def hash02(): Unit \ Assert =
        assertTrue(hash((Success(()): Validation[Unit, Unit])) != hash((Failure(necOf2((), ())): Validation[Unit, Unit])))

    @Test
    def hash03(): Unit \ Assert =
        assertEq(expected = hash((Success((1, 2)): Validation[Unit, (Int32, Int32)])), hash((Success((1, 2)): Validation[Unit, (Int32, Int32)])))

    @Test
    def hash04(): Unit \ Assert =
        assertTrue(hash((Success((1, 2, 3)): Validation[Unit, (Int32, Int32, Int32)])) != hash((Success((1, 3, 2)): Validation[Unit, (Int32, Int32, Int32)])))

    @Test
    def hash05(): Unit \ Assert =
        assertTrue(hash((Success(singleton(1)): Validation[Int32, Nec[Int32]])) != hash((Failure(singleton(1)): Validation[Int32, Nec[Int32]])))

}
