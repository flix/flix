mod TestFoldable {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse};
    use Applicative.{point, *>}

    /// To test foldLeftM, foldRightM, foreachM we want a monad with
    /// "side-effects" so we can log direction of travel.

    enum Logger[i, a](Chain[i], a)

    def tell(x: i): Logger[i, Unit] = Logger.Logger(Chain.singleton(x), ())

    def runLogger(ma: Logger[i, a]): (List[i], a) =
        let Logger.Logger(w, a) = ma;
        (Chain.toList(w), a)


    instance Functor[Logger[i]] {
        pub def map(f: a -> b \ ef, ma: Logger[i, a]): Logger[i, b] \ ef =
            let Logger.Logger(w, a) = ma;
            Logger.Logger(w, f(a))
    }

    instance Applicative[Logger[i]] {
        pub def point(x: a): Logger[i, a] = Logger.Logger(Chain.empty(), x)

        pub def ap(mf: Logger[i, a -> b \ ef], ma: Logger[i, a]): Logger[i, b] \ ef =
            let Logger.Logger(w1, f) = mf;
            let Logger.Logger(w2, a) = ma;
            Logger.Logger(Chain.append(w1, w2), f(a))
        }


    instance Monad[Logger[i]] {
        pub def flatMap(f: a -> Logger[i, b] \ ef, ma: Logger[i, a]): Logger[i, b] \ ef =
            let Logger.Logger(w1, a) = ma;
            let Logger.Logger(w2, b) = f(a);
            Logger.Logger(Chain.append(w1, w2), b)
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert = assertEq(expected = None, Foldable.head((Nil: List[String])))

    @Test
    def head02(): Unit \ Assert = assertEq(expected = Some("a"), Foldable.head("a" :: Nil))

    @Test
    def head03(): Unit \ Assert = assertEq(expected = Some("a"), Foldable.head("a" :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert = assertEq(expected = None, Foldable.last((Nil: List[String])))

    @Test
    def last02(): Unit \ Assert = assertEq(expected = Some("a"), Foldable.last("a" :: Nil))

    @Test
    def last03(): Unit \ Assert = assertEq(expected = Some("b"), Foldable.last("a" :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = assertEq(expected = None, Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, (Nil: List[Int32])))

    @Test
    def findLeft02(): Unit \ Assert = assertEq(expected = None, Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, 1 :: Nil))

    @Test
    def findLeft03(): Unit \ Assert = assertEq(expected = Some(2), Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, 1 :: 2 :: Nil))

    @Test
    def findLeft04(): Unit \ Assert = assertEq(expected = Some(2), Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, 1 :: 2 :: 3 :: 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = assertEq(expected = 0, Foldable.foldMap(x -> 2 * x, Nil))

    @Test
    def foldMap02(): Unit \ Assert = assertEq(expected = 6, Foldable.foldMap(x -> 2 * x, 1 :: 2 :: Nil))

    @Test
    def foldMap03(): Unit \ Assert = assertEq(expected = "b", Foldable.foldMap(x -> if (x == "a") "b" else x, "a" :: Nil))

    @Test
    def foldMap04(): Unit \ Assert = assertEq(expected = "abb", Foldable.foldMap(x -> if (x == "c") "b" else x, "a" :: "b" :: "c":: Nil))

    @Test
    def foldMap05(): Unit \ Assert = assertEq(expected = "123", Foldable.foldMap(Int32.toString, 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert = assertEq(expected = "", Foldable.fold((Nil: List[String])))

    @Test
    def fold02(): Unit \ Assert = assertEq(expected = "a", Foldable.fold("a" :: Nil))

    @Test
    def fold03(): Unit \ Assert = assertEq(expected = "ab", Foldable.fold("a" :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftM                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def foldLeftStep(acc: Int32, a: Int32): Logger[Int32, Int32] =
        tell(a) *> point(acc+a)

    @Test
    def foldLeftM01(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = (Nil, 0), runLogger(Foldable.foldLeftM(foldLeftStep, 0, xs)))

    @Test
    def foldLeftM02(): Unit \ Assert =
        let xs = 1 :: 2 :: Nil;
        assertEq(expected = (1 :: 2 :: Nil, 3), runLogger(Foldable.foldLeftM(foldLeftStep, 0, xs)))

    @Test
    def foldLeftM03(): Unit \ Assert =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        assertEq(expected = (1 :: 2 :: 3 :: 4 :: 5 :: Nil, 15), runLogger(Foldable.foldLeftM(foldLeftStep, 0, xs)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightM                                                              //
    /////////////////////////////////////////////////////////////////////////////

    def foldRightStep(a: Int32, acc: Int32): Logger[Int32, Int32] =
        tell(a) *> point(acc+a)

    @Test
    def foldRightM01(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = (Nil, 0), runLogger(Foldable.foldRightM(foldRightStep, 0, xs)))

    @Test
    def foldRightM02(): Unit \ Assert =
        let xs = 1 :: 2 :: Nil;
        assertEq(expected = (2 :: 1 :: Nil, 3), runLogger(Foldable.foldRightM(foldRightStep, 0, xs)))

    @Test
    def foldRightM03(): Unit \ Assert =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        assertEq(expected = (5 :: 4 :: 3 :: 2 :: 1 :: Nil, 15), runLogger(Foldable.foldRightM(foldRightStep, 0, xs)))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert =
        assertEq(expected = 0, Foldable.length((Nil: List[String])))

    @Test
    def length02(): Unit \ Assert =
        assertEq(expected = 2, Foldable.length("a" :: "b" :: Nil))

    @Test
    def length03(): Unit \ Assert =
        assertEq(expected = 4, Foldable.length(Some(5) :: None :: Some(18) :: None :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 0, Foldable.size((Nil: List[String])))

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 2, Foldable.size("a" :: "b" :: Nil))

    @Test
    def size03(): Unit \ Assert =
        assertEq(expected = 4, Foldable.size(Some(5) :: None :: Some(18) :: None :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert =
        let f = x -> x == "a";
        assertEq(expected = 0, Foldable.count(f, Nil))

    @Test
    def count02(): Unit \ Assert =
        let xs = "a" :: "b" :: "a" :: "c" :: "a" :: Nil;
        let f = x -> x == "a";
        assertEq(expected = 3, Foldable.count(f, xs))

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert =
        assertEq(expected = 0, Foldable.sum(None))

    @Test
    def sum02(): Unit \ Assert =
        let num = Some(8);
        assertEq(expected = 8, Foldable.sum(num))

    @Test
    def sum03(): Unit \ Assert =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        assertEq(expected = 15, Foldable.sum(xs))

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        let f = x -> x + 1;
        assertEq(expected = 0, Foldable.sumWith(f, Nil))

    @Test
    def sumWith02(): Unit \ Assert =
        let f = x -> x + 1;
        assertEq(expected = 14, Foldable.sumWith(f, 1 :: 2 :: 3 :: 4 :: Nil))

    @Test
    def sumWith03(): Unit \ Assert =
        let f = x -> match x {
            case None    => 0
            case Some(v) => 2 * v
        };
        assertEq(expected = 10, Foldable.sumWith(f, Some(Some(5))))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert =
        let f = x -> x == 3;
        assertFalse(Foldable.exists(f, Nil))

    @Test
    def exists02(): Unit \ Assert =
        let f = x -> x == "d";
        assertTrue(Foldable.exists(f, "a" :: "b" :: "c" :: "d" :: "e" :: Nil))

    @Test
    def exists03(): Unit \ Assert =
        let f = x -> x == "f";
        assertFalse(Foldable.exists(f, "a" :: "b" :: "c" :: "d" :: "e" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert =
        let f = x -> x == 3;
        assertTrue(Foldable.forAll(f, Nil))

    @Test
    def forAll02(): Unit \ Assert =
        let f = x -> x == "a";
        assertTrue(Foldable.forAll(f, "a" :: "a" :: "a" :: "a" :: "a" :: Nil))

    @Test
    def forAll03(): Unit \ Assert =
        let f = x -> x == "a";
        assertFalse(Foldable.forAll(f, "a" :: "a" :: "a" :: "a" :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // forEachM                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachM01(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = (Nil, ()), runLogger(Foldable.forEachM(tell, xs)))

    @Test
    def forEachM02(): Unit \ Assert =
        let xs = 1 :: 2 :: Nil;
        assertEq(expected = (1 :: 2 :: Nil, ()), runLogger(Foldable.forEachM(tell, xs)))

    @Test
    def forEachM03(): Unit \ Assert =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        assertEq(expected = (1 :: 2 :: 3 :: 4 :: 5 :: Nil, ()), runLogger(Foldable.forEachM(tell, xs)))

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Unit \ Assert =
        assertEq(expected = (Chain.empty(): Chain[Int32]), Foldable.toChain((Nil: List[Int32])))

    @Test
    def toChain02(): Unit \ Assert =
        assertEq(expected = List.toChain(1 :: 2 :: Nil), Foldable.toChain(1 :: 2 :: Nil))

    @Test
    def toChain03(): Unit \ Assert =
        assertEq(expected = List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: Nil), Foldable.toChain(Set#{0, 1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Foldable.toArray(rc, Nel.Nel(1, Nil)), Array#{1} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Foldable.toArray(rc, Nel.Nel(1, 2 :: 3 :: Nil)), Array#{1, 2, 3} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Foldable.toArray(rc, Nel.Nel(1, 1 :: 2 :: 3 :: Nil)), Array#{1, 1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        assertEq(expected = Vector#{1}, Foldable.toVector(Nel.Nel(1, Nil)))

    @Test
    def toVector02(): Unit \ Assert =
        assertEq(expected = Vector#{1, 2, 3}, Foldable.toVector(Nel.Nel(1, 2 :: 3 :: Nil)))

    @Test
    def toVector03(): Unit \ Assert =
        assertEq(expected = Vector#{1, 1, 2, 3}, Foldable.toVector(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith01(): Unit \ Assert =
        assertEq(expected = Map#{}, Foldable.toMapWith(x -> 0 * x, List.toChain(Nil)))

    @Test
    def toMapWith02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 0, 2 => 0}, Foldable.toMapWith(x -> 0 * x, List.toChain(1 :: 2 :: Nil)))

    @Test
    def toMapWith03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 2, 2 => 4}, Foldable.toMapWith(x -> 2 * x, List.toChain(1 :: 2 :: Nil)))

    @Test
    def toMapWith04(): Unit \ Assert =
        assertEq(expected = Map#{1 => 11, 2 => 12}, Foldable.toMapWith(x -> x + 10, List.toChain(1 :: 2 :: Nil)))

    @Test
    def toMapWith05(): Unit \ Assert =
        assertEq(expected = Map#{"a" => "ab", "aa" => "aab"}, Foldable.toMapWith(x -> x + "b", List.toChain("aa" :: "a" :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Unit \ Assert =
        assertEq(expected = None, Foldable.toNec((Nil: List[Int32])))

    @Test
    def toNec02(): Unit \ Assert =
        assertEq(expected = Some(Nec.cons(1, Nec.singleton(2))), Foldable.toNec(1 :: 2 :: Nil))

    @Test
    def toNec03(): Unit \ Assert =
        assertEq(expected = List.toNec(0 :: 1 :: 2 :: 3 :: 4 :: Nil), Foldable.toNec(Set#{0, 1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Unit \ Assert =
        assertEq(expected = None, Foldable.toNel((Nil: List[Int32])))

    @Test
    def toNel02(): Unit \ Assert =
        assertEq(expected = Some(Nel.cons(1, Nel.singleton(2))), Foldable.toNel(1 :: 2 :: Nil))

    @Test
    def toNel03(): Unit \ Assert =
        assertEq(expected = List.toNel(0 :: 1 :: 2 :: 3 :: 4 :: Nil), Foldable.toNel(Set#{0, 1, 2, 3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert =
        assertEq(expected = "", Foldable.join(",", (Nil: List[Int32])))

    @Test
    def join02(): Unit \ Assert =
        assertEq(expected = "1", Foldable.join(",", 1 :: Nil))

    @Test
    def join03(): Unit \ Assert =
        assertEq(expected = "1,2,3", Foldable.join(",", 1 :: 2 :: 3 :: Nil))

    @Test
    def join04(): Unit \ Assert =
        assertEq(expected = "1,2,3", Foldable.join(",", "1" :: "2" :: "3" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert =
        assertEq(expected = "", Foldable.joinWith(x -> "${x + 1}", ",", (Nil: List[Int32])))

    @Test
    def joinWith02(): Unit \ Assert =
        assertEq(expected = "2", Foldable.joinWith(x -> "${x + 1}", ",", 1 :: Nil))

    @Test
    def joinWith03(): Unit \ Assert =
        assertEq(expected = "2,3,4", Foldable.joinWith(x -> "${x + 1}", ",", 1 :: 2 :: 3 :: Nil))

    @Test
    def joinWith04(): Unit \ Assert =
        assertEq(expected = "11,22,33", Foldable.joinWith(x -> x + x, ",", "1" :: "2" :: "3" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert =
        assertEq(expected = (Nil: List[Int32]), Foldable.filter(_ -> true, (Nil: List[Int32])))

    @Test
    def filter02(): Unit \ Assert =
        assertEq(expected = Nil, Foldable.filter(_ -> false, List#{1, 2}))

    @Test
    def filter03(): Unit \ Assert =
        assertEq(expected = List#{8, 3}, Foldable.filter(x -> x > 2, List#{1, 8, 2, 3}))

    @Test
    def filter04(): Unit \ Assert =
        assertEq(expected = List#{1, 2}, Foldable.filter(x -> x <= 2, List#{1, 8, 2, 3}))

    eff Capture {
        def capture(x: Int32): Int32
    }

    @Test
    def filter05(): Unit \ Assert = region rc {
        let xs = Vector#{1, 2, 3, 4};
        let effects = MutList.empty(rc);
        def f(x) = if (x > 2) {
            discard Capture.capture(x);
            false
        } else {
            true
        };
        let filtered = run {
            Foldable.filter(f, xs)
        } with handler Capture {
            def capture(x, resume) = {
                MutList.push(x, effects);
                resume(x)
            }
        };
        assertEq(expected = List#{1, 2}, filtered);
        assertEq(expected = List#{3, 4}, MutList.toList(effects))
    }
}
