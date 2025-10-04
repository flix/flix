mod TestNel {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse};
    use Hash.hash
    use Nel.Nel

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "Nel(1, Nil)", Nel.toString(Nel(1, Nil)))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "Nel(1, 2 :: 3 :: 4 :: Nil)", Nel.toString(Nel(1, 2 :: 3 :: 4 :: Nil)))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "Nel(a, b :: Nil)", Nel.toString(Nel('a', 'b' :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert = assertEq(expected = Nel(1, Nil), Nel.singleton(1))

    @Test
    def singleton02(): Unit \ Assert = assertEq(expected = Nel(2, Nil), Nel.singleton(2))

    @Test
    def singleton03(): Unit \ Assert = assertEq(expected = Nel('a', Nil), Nel.singleton('a'))

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def cons01(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: Nil), Nel.cons(1, Nel.singleton(2)))

    @Test
    def cons02(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: Nil), Nel.cons(1, Nel.cons(2, Nel.singleton(3))))

    @Test
    def cons03(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: 4 :: Nil), Nel.cons(1, Nel.cons(2, Nel.cons(3, Nel.singleton(4)))))


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert = assertEq(expected = 123, Nel.head(Nel(123, Nil)))

    @Test
    def head02(): Unit \ Assert = assertEq(expected = 123, Nel.head(Nel(123, 1 :: Nil)))

    @Test
    def head03(): Unit \ Assert = assertEq(expected = 123, Nel.head(Nel(123, 1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert = assertEq(expected = 123, Nel.last(Nel(123, Nil)))

    @Test
    def last02(): Unit \ Assert = assertEq(expected = 1, Nel.last(Nel(123, 1 :: Nil)))

    @Test
    def last03(): Unit \ Assert = assertEq(expected = 2, Nel.last(Nel(123, 1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Unit \ Assert = assertEq(expected = Nil, Nel.init(Nel(1, Nil)))

    @Test
    def init02(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nel.init(Nel(1, 2 :: Nil)))

    @Test
    def init03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nel.init(Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tail01(): Unit \ Assert = assertEq(expected = Nil, Nel.tail(Nel(1, Nil)))

    @Test
    def tail02(): Unit \ Assert = assertEq(expected = 2 :: Nil, Nel.tail(Nel(1, 2 :: Nil)))

    @Test
    def tail03(): Unit \ Assert = assertEq(expected = 2 :: 3 :: Nil, Nel.tail(Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert = assertEq(expected = 1, Nel.length(Nel(1, Nil)))

    @Test
    def length02(): Unit \ Assert = assertEq(expected = 2, Nel.length(Nel(1, 2 :: Nil)))

    @Test
    def length03(): Unit \ Assert = assertEq(expected = 3, Nel.length(Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = assertEq(expected = 1, Nel.size(Nel(1, Nil)))

    @Test
    def size02(): Unit \ Assert = assertEq(expected = 2, Nel.size(Nel(1, 2 :: Nil)))

    @Test
    def size03(): Unit \ Assert = assertEq(expected = 3, Nel.size(Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: Nil), Nel(1, Nil) `Nel.append` Nel(2, Nil))

    @Test
    def append02(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: Nil), Nel(1, 2 :: Nil) `Nel.append` Nel(3, Nil))

    @Test
    def append03(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: Nil), Nel(1, Nil) `Nel.append` Nel(2, 3 :: Nil))

    @Test
    def append04(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: 4 :: Nil), Nel(1, 2 :: Nil) `Nel.append` Nel(3, 4 :: Nil))

    @Test
    def append05(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil), Nel(1, 2 :: 3 :: Nil) `Nel.append` Nel(4, 5 :: 6 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = assertFalse(Nel.memberOf(0, Nel(1, Nil)))

    @Test
    def memberOf02(): Unit \ Assert = assertFalse(Nel.memberOf(0, Nel(1, 2 :: Nil)))

    @Test
    def memberOf03(): Unit \ Assert = assertFalse(Nel.memberOf(0, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def memberOf04(): Unit \ Assert = assertTrue(Nel.memberOf(1, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def memberOf05(): Unit \ Assert = assertTrue(Nel.memberOf(2, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def memberOf06(): Unit \ Assert = assertTrue(Nel.memberOf(3, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = assertEq(expected = None, Nel.findLeft(x -> x == 0, Nel(1, Nil)))

    @Test
    def findLeft02(): Unit \ Assert = assertEq(expected = None, Nel.findLeft(x -> x == 0, Nel(1, 2 :: Nil)))

    @Test
    def findLeft03(): Unit \ Assert = assertEq(expected = None, Nel.findLeft(x -> x == 0, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findLeft04(): Unit \ Assert = assertEq(expected = Some(1), Nel.findLeft(x -> x == 1, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findLeft05(): Unit \ Assert = assertEq(expected = Some(2), Nel.findLeft(x -> x == 2, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findLeft06(): Unit \ Assert = assertEq(expected = Some(3), Nel.findLeft(x -> x == 3, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = assertEq(expected = None, Nel.findRight(x -> x == 0, Nel(1, Nil)))

    @Test
    def findRight02(): Unit \ Assert = assertEq(expected = None, Nel.findRight(x -> x == 0, Nel(1, 2 :: Nil)))

    @Test
    def findRight03(): Unit \ Assert = assertEq(expected = None, Nel.findRight(x -> x == 0, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findRight04(): Unit \ Assert = assertEq(expected = Some(1), Nel.findRight(x -> x == 1, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findRight05(): Unit \ Assert = assertEq(expected = Some(2), Nel.findRight(x -> x == 2, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def findRight06(): Unit \ Assert = assertEq(expected = Some(3), Nel.findRight(x -> x == 3, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = assertEq(expected = Nel(2, Nil), Nel.map(x -> x + 1, Nel(1, Nil)))

    @Test
    def map02(): Unit \ Assert = assertEq(expected = Nel(2, 3 :: Nil), Nel.map(x -> x + 1, Nel(1, 2 :: Nil)))

    @Test
    def map03(): Unit \ Assert = assertEq(expected = Nel(2, 3 :: 4 :: Nil), Nel.map(x -> x + 1, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert = assertEq(expected = Nel(0, Nil), Nel.mapWithIndex((i, _) -> i, Nel(21, Nil)))

    @Test
    def mapWithIndex02(): Unit \ Assert = assertEq(expected = Nel(0, 1 :: Nil), Nel.mapWithIndex((i, _) -> i, Nel(21, 42 :: Nil)))

    @Test
    def mapWithIndex03(): Unit \ Assert = assertEq(expected = Nel(0, 1 :: 2 :: Nil), Nel.mapWithIndex((i, _) -> i, Nel(21, 42 :: 84 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Unit \ Assert = assertEq(expected = Nel(6, Nil), Nel.ap(Nel((x -> x+1), Nil), Nel(5, Nil)))

    @Test
    def ap02(): Unit \ Assert = assertEq(expected = Nel(1, 6 :: Nil), Nel.ap(Nel((x -> x+1), Nil), Nel(0, 5 :: Nil)))

    @Test
    def ap03(): Unit \ Assert = assertEq(expected = Nel(1, 5 :: 0 :: 8 :: Nil), Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: Nil)))

    @Test
    def ap04(): Unit \ Assert = assertEq(expected = Nel(1, 5 :: 9 :: 0 :: 8 :: 16 :: Nil), Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: 8 :: Nil)))

    @Test
    def ap05(): Unit \ Assert = assertEq(expected = Nel(1, 5 :: 9 :: 17 :: 0 :: 8 :: 16 :: 32 :: Nil), Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: 8 :: 16 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = assertEq(expected = Nel(2, Nil), Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, Nil)))

    @Test
    def flatMap02(): Unit \ Assert = assertEq(expected = Nel(2, 3 :: Nil), Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: Nil)))

    @Test
    def flatMap03(): Unit \ Assert = assertEq(expected = Nel(2, 3 :: 4 :: Nil), Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: 3 :: Nil)))

    @Test
    def flatMap04(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: Nil), Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, Nil)))

    @Test
    def flatMap05(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 4 :: 5 :: Nil), Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, 4 :: Nil)))

    @Test
    def flatMap06(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil), Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, 3 :: 5 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = Nel(1, Nil), Nel.reverse(Nel(1, Nil)))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = Nel(2, 1 :: Nil), Nel.reverse(Nel(1, 2 :: Nil)))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = Nel(3, 2 :: 1 :: Nil), Nel.reverse(Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = assertEq(expected = Nel(42, Nil), Nel.replace(src = 1, dst = 42, Nel(1, Nil)))

    @Test
    def replace02(): Unit \ Assert = assertEq(expected = Nel(1, Nil), Nel.replace(src = 7, dst = 42, Nel(1, Nil)))

    @Test
    def replace03(): Unit \ Assert = assertEq(expected = Nel(1, 42 :: Nil), Nel.replace(src = 2, dst = 42, Nel(1, 2 :: Nil)))

    @Test
    def replace04(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: Nil), Nel.replace(src = 7, dst = 42, Nel(1, 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // permutations                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def permutations01(): Unit \ Assert = assertEq(expected = Nel(1 :: Nil, Nil), Nel.permutations(Nel(1, Nil)))

    @Test
    def permutations02(): Unit \ Assert = assertEq(expected = Nel(1 :: 2 :: Nil, (2 :: 1 :: Nil) :: Nil), Nel.permutations(Nel(1, 2 :: Nil)))

    @Test
    def permutations03(): Unit \ Assert = assertEq(expected = Nel(1 :: 2 :: 3 :: Nil,
                                                                           (1 :: 3 :: 2 :: Nil) ::
                                                                           (2 :: 1 :: 3 :: Nil) ::
                                                                           (2 :: 3 :: 1 :: Nil) ::
                                                                           (3 :: 1 :: 2 :: Nil) ::
                                                                           (3 :: 2 :: 1 :: Nil) :: Nil), Nel.permutations(Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // subsequences                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def subsequences01(): Unit \ Assert = assertEq(expected = Nel(1 :: Nil, (Nil) :: Nil), Nel.subsequences(Nel(1, Nil)))

    @Test
    def subsequences02(): Unit \ Assert = assertEq(expected = Nel(1 :: 2 :: Nil, (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil), Nel.subsequences(Nel(1, 2 :: Nil)))

    @Test
    def subsequences03(): Unit \ Assert = assertEq(expected = Nel(1 :: 2 :: 3 :: Nil,
                                                                           (1 :: 2 :: Nil) ::
                                                                           (1 :: 3 :: Nil) ::
                                                                           (1 :: Nil) ::
                                                                           (2 :: 3 :: Nil) ::
                                                                           (2 :: Nil) ::
                                                                           (3 :: Nil) ::
                                                                           (Nil) :: Nil), Nel.subsequences(Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Unit \ Assert = assertEq(expected = Nel(1, Nil), Nel.intersperse(7, Nel(1, Nil)))

    @Test
    def intersperse02(): Unit \ Assert = assertEq(expected = Nel(1, 7 :: 2 :: Nil), Nel.intersperse(7, Nel(1, 2 :: Nil)))

    @Test
    def intersperse03(): Unit \ Assert = assertEq(expected = Nel(1, 7 :: 2 :: 7 :: 3 :: Nil), Nel.intersperse(7, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert = assertEq(expected = "a", Nel.fold(Nel("a", Nil)))

    @Test
    def fold02(): Unit \ Assert = assertEq(expected = (), Nel.fold(Nel((), Nil)))

    @Test
    def fold03(): Unit \ Assert = assertEq(expected = "abc", Nel.fold(Nel("a", "b" :: "c" :: Nil)))

    @Test
    def fold04(): Unit \ Assert = assertEq(expected = ("abc", ()), Nel.fold(Nel(("a", ()), ("b", ()) :: ("c", ()) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = assertEq(expected = -1, Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, Nil)))

    @Test
    def foldLeft02(): Unit \ Assert = assertEq(expected = -3, Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: Nil)))

    @Test
    def foldLeft03(): Unit \ Assert = assertEq(expected = -6, Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = assertEq(expected = -1, Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, Nil)))

    @Test
    def foldRight02(): Unit \ Assert = assertEq(expected = -3, Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: Nil)))

    @Test
    def foldRight03(): Unit \ Assert = assertEq(expected = -6, Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = assertEq(expected = -1, Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, Nil)))

    @Test
    def foldRightWithCont02(): Unit \ Assert = assertEq(expected = -3, Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, 2 :: Nil)))

    @Test
    def foldRightWithCont03(): Unit \ Assert = assertEq(expected = -6, Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
        assertEq(expected = 6, Nel.foldMap(x -> 2 * x, Nel(1, 2 :: Nil)))

    @Test
    def foldMap02(): Unit \ Assert =
        assertEq(expected = "b", Nel.foldMap(x -> if (x == "a") "b" else x, Nel("a", Nil)))

    @Test
    def foldMap03(): Unit \ Assert =
        assertEq(expected = "abb", Nel.foldMap(x -> if (x == "c") "b" else x, Nel("a", "b" :: "c" :: Nil)))

    @Test
    def foldMap04(): Unit \ Assert =
        assertEq(expected = "123", Nel.foldMap(Int32.toString, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduce01(): Unit \ Assert = assertEq(expected = "a", Nel.reduce(Nel("a", Nil)))

    @Test
    def reduce02(): Unit \ Assert = assertEq(expected = (), Nel.reduce(Nel((), Nil)))

    @Test
    def reduce03(): Unit \ Assert = assertEq(expected = "abc", Nel.reduce(Nel("a", "b" :: "c" :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert = assertEq(expected = 1, Nel.reduceLeft((acc, x) -> acc - x, Nel(1, Nil)))

    @Test
    def reduceLeft02(): Unit \ Assert = assertEq(expected = -1, Nel.reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: Nil)))

    @Test
    def reduceLeft03(): Unit \ Assert = assertEq(expected = -4, Nel.reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert = assertEq(expected = 1, Nel.reduceRight((x, acc) -> acc - x, Nel(1, Nil)))

    @Test
    def reduceRight02(): Unit \ Assert = assertEq(expected = 1, Nel.reduceRight((x, acc) -> acc - x, Nel(1, 2 :: Nil)))

    @Test
    def reduceRight03(): Unit \ Assert = assertEq(expected = 0, Nel.reduceRight((x, acc) -> acc - x, Nel(1, 2 :: 3 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = assertEq(expected = 1, Nel.count(x -> x == 1, Nel(1, Nil)))

    @Test
    def count02(): Unit \ Assert = assertEq(expected = 0, Nel.count(x -> x == 2, Nel(1, Nil)))

    @Test
    def count03(): Unit \ Assert = assertEq(expected = 2, Nel.count(x -> x == 2, Nel(1, 2 :: 1 :: 2 :: Nil)))

    @Test
    def count04(): Unit \ Assert = assertEq(expected = 3, Nel.count(x -> x == 2, Nel(2, 2 :: 1 :: 2 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert =
        assertEq(expected = 1, Nel.sum(Nel(1, Nil)))

    @Test
    def sum02(): Unit \ Assert =
        assertEq(expected = 6, Nel.sum(Nel(1, 2 :: 3 :: Nil)))

    @Test
    def sum03(): Unit \ Assert =
        assertEq(expected = 3, Nel.sum(Nel(1, 2 :: 3 :: -3 :: Nil)))

    @Test
    def sum04(): Unit \ Assert =
        assertEq(expected = -10, Nel.sum(Nel(-1, -2 :: -3 :: -4 :: Nil)))

    @Test
    def sum05(): Unit \ Assert =
        assertEq(expected = 0, Nel.sum(Nel(10, -10 :: Nil)))

    @Test
    def sum06(): Unit \ Assert = match List.range(1, 101) {
        case x :: xs => assertEq(expected = 5050, Nel.sum(Nel(x, xs)))
        case _       => unreachable!()
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 2, Nel.sumWith(x -> x + 1, Nel(1, Nil)))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 9, Nel.sumWith(x -> x + 1, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 7, Nel.sumWith(x -> x + 1, Nel(1, 2 :: 3 :: -3 :: Nil)))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = -6, Nel.sumWith(x -> x + 1, Nel(-1, -2 :: -3 :: -4 :: Nil)))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = 2, Nel.sumWith(x -> x + 1, Nel(10, -10 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = assertEq(expected = Nel(1, Nil), Nel.flatten(Nel(Nel(1, Nil), Nil)))

    @Test
    def flatten02(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: Nil), Nel.flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nil)))

    @Test
    def flatten03(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: Nil), Nel.flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nel(3, Nil) :: Nil)))

    @Test
    def flatten04(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: 4 :: 5 :: Nil), Nel.flatten(Nel(Nel(1, Nil), Nel(2, 3 :: Nil) :: Nel(4, 5 :: Nil) :: Nil)))

    @Test
    def flatten05(): Unit \ Assert = assertEq(expected = Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil), Nel.flatten(Nel(Nel(1, 2 :: 3 :: Nil), Nel(4, 5 :: Nil) :: Nel(6, 7 :: Nil) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = assertTrue(Nel.exists(x -> x == 1, Nel(1, Nil)))

    @Test
    def exists02(): Unit \ Assert = assertFalse(Nel.exists(x -> x == 2, Nel(1, Nil)))

    @Test
    def exists03(): Unit \ Assert = assertTrue(Nel.exists(x -> x == 1, Nel(1, 2 :: Nil)))

    @Test
    def exists04(): Unit \ Assert = assertTrue(Nel.exists(x -> x == 2, Nel(1, 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = assertTrue(Nel.forAll(x -> x == 1, Nel(1, Nil)))

    @Test
    def forAll02(): Unit \ Assert = assertFalse(Nel.forAll(x -> x == 2, Nel(1, Nil)))

    @Test
    def forAll03(): Unit \ Assert = assertTrue(Nel.forAll(x -> x == 1, Nel(1, 1 :: Nil)))

    @Test
    def forAll04(): Unit \ Assert = assertFalse(Nel.forAll(x -> x == 1, Nel(1, 2 :: Nil)))

    @Test
    def forAll05(): Unit \ Assert = assertFalse(Nel.forAll(x -> x == 1, Nel(2, 1 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nel.filter(x -> x > 0, Nel(1, Nil)))

    @Test
    def filter02(): Unit \ Assert = assertEq(expected = Nil, Nel.filter(x -> x > 1, Nel(1, Nil)))

    @Test
    def filter03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nel.filter(x -> x > 0, Nel(1, 2 :: Nil)))

    @Test
    def filter04(): Unit \ Assert = assertEq(expected = 2 :: Nil, Nel.filter(x -> x > 1, Nel(1, 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert = assertEq(expected = Nel((1, 1), Nil), Nel.zip(Nel(1, Nil), Nel(1, Nil)))

    @Test
    def zip02(): Unit \ Assert = assertEq(expected = Nel((1, 1), Nil), Nel.zip(Nel(1, 2 :: Nil), Nel(1, Nil)))

    @Test
    def zip03(): Unit \ Assert = assertEq(expected = Nel((1, 1), Nil), Nel.zip(Nel(1, Nil), Nel(1, 2 :: Nil)))

    @Test
    def zip04(): Unit \ Assert = assertEq(expected = Nel((1, 1), (2, 2) :: Nil), Nel.zip(Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Unit \ Assert = assertEq(expected = Nel(2, Nil), Nel.zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, Nil)))

    @Test
    def zipWith02(): Unit \ Assert = assertEq(expected = Nel(2, Nil), Nel.zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, Nil)))

    @Test
    def zipWith03(): Unit \ Assert = assertEq(expected = Nel(2, Nil), Nel.zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, 2 :: Nil)))

    @Test
    def zipWith04(): Unit \ Assert = assertEq(expected = Nel(2, 4 :: Nil), Nel.zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Unit \ Assert = assertEq(expected = (Nel(1, Nil), Nel(1, Nil)), Nel.unzip(Nel((1, 1), Nil)))

    @Test
    def unzip02(): Unit \ Assert = assertEq(expected = (Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)), Nel.unzip(Nel((1, 1), (2, 2) :: Nil)))

    @Test
    def unzip03(): Unit \ Assert = assertEq(expected = (Nel(1, 2 :: 3 :: Nil), Nel(1, 2 :: 3 :: Nil)), Nel.unzip(Nel((1, 1), (2, 2) :: (3, 3) :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Unit \ Assert = assertEq(expected = Nel((0, 1), Nil), Nel.zipWithIndex(Nel(1, Nil)))

    @Test
    def zipWithIndex02(): Unit \ Assert = assertEq(expected = Nel((0, 1), (1, 2) :: Nil), Nel.zipWithIndex(Nel(1, 2 :: Nil)))

    @Test
    def zipWithIndex03(): Unit \ Assert = assertEq(expected = Nel((0, 2), (1, 1) :: Nil), Nel.zipWithIndex(Nel(2, 1 :: Nil)))

    @Test
    def zipWithIndex04(): Unit \ Assert = assertEq(expected = Nel((0, 1), (1, 2) :: (2, 3) :: Nil), Nel.zipWithIndex(Nel(1, 2 :: 3 :: Nil)))

    @Test
    def zipWithIndex05(): Unit \ Assert =
        assertEq(expected = Nel((0, 1), (1, 2) :: (2, 3) :: (3, 4) :: (4, 5) :: Nil), Nel.zipWithIndex(Nel(1, 2 :: 3 :: 4 :: 5 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithA01(): Unit \ Assert = assertEq(expected = Identity.Identity(Nel(2, Nil)), Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, Nil), Nel(1, Nil)))

    @Test
    def zipWithA02(): Unit \ Assert = assertEq(expected = Identity.Identity(Nel(2, Nil)), Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, 2 :: Nil), Nel(1, Nil)))

    @Test
    def zipWithA03(): Unit \ Assert = assertEq(expected = Identity.Identity(Nel(2, Nil)), Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, Nil), Nel(1, 2 :: Nil)))

    @Test
    def zipWithA04(): Unit \ Assert = assertEq(expected = Identity.Identity(Nel(2, 4 :: Nil)), Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nel.toList(Nel(1, Nil)))

    @Test
    def toList02(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nel.toList(Nel(1, 2 :: Nil)))

    @Test
    def toList03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, Nel.toList(Nel(1, 2 :: 3 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let a = Nel.toArray(rc, Nel.singleton(1));
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let a = Nel.toArray(rc, Nel(1, List#{2}));
        assertTrue(Array.sameElements(a, Array#{1, 2} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let a = Nel.toArray(rc, Nel(1, List#{2, 3}));
        assertTrue(Array.sameElements(a, Array#{1, 2, 3} @ rc))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let a = Nel.toArray(rc, Nel(3, List#{2, 3}));
        assertTrue(Array.sameElements(a, Array#{3, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        let l = Nel(1, List#{});
        assertEq(expected = Vector#{1}, Nel.toVector(l))

    @Test
    def toVector02(): Unit \ Assert =
        let l = Nel(1, List#{2});
        assertEq(expected = Vector#{1, 2}, Nel.toVector(l))

    @Test
    def toVector03(): Unit \ Assert =
        let l = Nel(1, List#{2, 3});
        assertEq(expected = Vector#{1, 2, 3}, Nel.toVector(l))

    @Test
    def toVector04(): Unit \ Assert =
        let l = Nel(3, List#{2, 3});
        assertEq(expected = Vector#{3, 2, 3}, Nel.toVector(l))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEach(x -> Ref.put(x, ri), Nel.singleton(1));
        assertEq(expected = 1, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEach(x -> Ref.put(x, ri), Nel(1, 2 :: 3 :: Nil));
        assertEq(expected = 3, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEachWithIndex((i, _) -> Ref.put(i, ri), Nel.singleton(1));
        assertEq(expected = 0, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEachWithIndex((i, _) -> Ref.put(i, ri), Nel(1, List#{2, 3}));
        assertEq(expected = 2, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert = assertEq(expected = hash(Nel(1, Nil)), hash(Nel(1, Nil)))

    @Test
    def hash02(): Unit \ Assert = assertEq(expected = hash(Nel(1, 2 :: 3 :: Nil)), hash(Nel(1, 2 :: 3 :: Nil)))

    @Test
    def hash03(): Unit \ Assert = assertNeq(unexpected = hash(Nel(1, 3 :: 2 :: Nil)), hash(Nel(1, 2 :: 3 :: Nil)))

    @Test
    def hash04(): Unit \ Assert = assertNeq(unexpected = hash(Nel(1, Nil)), hash(Nel(2, Nil)))

    @Test
    def hash05(): Unit \ Assert = assertNeq(unexpected = hash(Nel("c", "b" :: "a" :: Nil)), hash(Nel("c", "a" :: "b" :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert = assertEq(expected = 1, Nel.minimumBy((x, y) -> x <=> y, Nel(1, Nil)))

    @Test
    def minimumBy02(): Unit \ Assert = assertEq(expected = 1, Nel.minimumBy((x, y) -> x <=> y, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def minimumBy03(): Unit \ Assert = assertEq(expected = 0, Nel.minimumBy((x, y) -> x <=> y, Nel(1, 2 :: 0 :: 3 :: Nil)))

    @Test
    def minimumBy04(): Unit \ Assert = assertEq(expected = 3, Nel.minimumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)))

    @Test
    def minimumBy05(): Unit \ Assert = assertEq(expected = 2, Nel.minimumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert = assertEq(expected = 1, Nel.maximumBy((x, y) -> x <=> y, Nel(1, Nil)))

    @Test
    def maximumBy02(): Unit \ Assert = assertEq(expected = 3, Nel.maximumBy((x, y) -> x <=> y, Nel(1, 2 :: 3 :: Nil)))

    @Test
    def maximumBy03(): Unit \ Assert = assertEq(expected = 3, Nel.maximumBy((x, y) -> x <=> y, Nel(1, 2 :: 0 :: 3 :: Nil)))

    @Test
    def maximumBy04(): Unit \ Assert = assertEq(expected = 9, Nel.maximumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)))

    @Test
    def maximumBy05(): Unit \ Assert = assertEq(expected = 10, Nel.maximumBy((x, y) -> x <=> y, Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // The following tests for sort functions correspond to those in TestList  //
    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith02(): Unit \ Assert =
        assertEq(expected = Nel(0, Nil), Nel.sortWith(cmp, Nel(0, Nil)))

    @Test
    def sortWith03(): Unit \ Assert =
        assertEq(expected = Nel(0, 1 :: Nil), Nel.sortWith(cmp, Nel(0, 1 :: Nil)))

    @Test
    def sortWith04(): Unit \ Assert =
        assertEq(expected = Nel(0, 1 :: Nil), Nel.sortWith(cmp, Nel(1, 0 :: Nil)))

    @Test
    def sortWith05(): Unit \ Assert =
        assertEq(expected = Nel(1, 1 :: Nil), Nel.sortWith(cmp, Nel(1, 1 :: Nil)))

    @Test
    def sortWith06(): Unit \ Assert =
        assertEq(expected = Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil), Nel.sortWith(cmp, Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)))

    @Test
    def sortWith07(): Unit \ Assert =
        assertEq(expected = Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil), Nel.sortWith(cmp, Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)))

    @Test
    def sortWith08(): Unit \ Assert =
        assertEq(expected = Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil), Nel.sortWith(cmp, Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)))

    @Test
    def sortWith09(): Unit \ Assert =
        assertEq(expected = Nel(0, 1 :: 2 :: 2 :: 3 :: 4 :: Nil), Nel.sortWith(cmp, Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)))

    @Test
    def sortWith10(): Unit \ Assert =
        assertEq(expected = Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil), Nel.sortWith(flip(cmp), Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)))

    @Test
    def sortWith11(): Unit \ Assert =
        assertEq(expected = Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil), Nel.sortWith(flip(cmp), Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)))

    @Test
    def sortWith12(): Unit \ Assert =
        assertEq(expected = Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil), Nel.sortWith(flip(cmp), Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)))

    @Test
    def sortWith13(): Unit \ Assert =
        assertEq(expected = Nel(4, 3 :: 2 :: 2 :: 1 :: 0 :: Nil), Nel.sortWith(flip(cmp), Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(xs: Nel[Int32]) : Unit \ Assert =
        assertEq(expected = Nel.sortWith(cmp, xs), Nel.sort(xs))

    @Test
    def sort02(): Unit \ Assert = testSortVsSortWith(Nel(0, Nil))

    @Test
    def sort03(): Unit \ Assert = testSortVsSortWith(Nel(0, 1 :: Nil))

    @Test
    def sort04(): Unit \ Assert = testSortVsSortWith(Nel(1, 0 :: Nil))

    @Test
    def sort05(): Unit \ Assert = testSortVsSortWith(Nel(1, 1 :: Nil))

    @Test
    def sort06(): Unit \ Assert = testSortVsSortWith(Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sort07(): Unit \ Assert = testSortVsSortWith(Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sort08(): Unit \ Assert = testSortVsSortWith(Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sort09(): Unit \ Assert = testSortVsSortWith(Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Nel[Int32]) : Unit \ Assert = {
        assertEq(expected = Nel.sort(a), Nel.sortBy(identity, a));
        assertEq(expected = Nel.sort(a), Nel.sortBy(x -> 4 * x + 7, a));
        assertEq(expected = Nel.sortWith(flip(cmp),a), Nel.sortBy(x -> -x, a))
    }

    @Test
    def sortBy02(): Unit \ Assert = testSortByVsSort(Nel(0, Nil))

    @Test
    def sortBy03(): Unit \ Assert = testSortByVsSort(Nel(0, 1 :: Nil))

    @Test
    def sortBy04(): Unit \ Assert = testSortByVsSort(Nel(1, 0 :: Nil))

    @Test
    def sortBy05(): Unit \ Assert = testSortByVsSort(Nel(1, 1 :: Nil))

    @Test
    def sortBy06(): Unit \ Assert = testSortByVsSort(Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sortBy07(): Unit \ Assert = testSortByVsSort(Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sortBy08(): Unit \ Assert = testSortByVsSort(Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortBy09(): Unit \ Assert = testSortByVsSort(Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Some(Nel(1, 2 :: 3 :: Nil)), Nel(1, 2 :: 3 :: Nil) |> Nel.iterator(rc) |> Iterator.toNel)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = List.range(-100, 100), Nel(-100, List.range(-99, 100)) |> Nel.iterator(rc) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Unit \ Assert =
        let l = Nel.singleton(Identity.Identity(1));
        assertEq(expected = Identity.Identity(Nel.singleton(1)), Nel.sequence(l))

    @Test
    def sequence02(): Unit \ Assert =
        let l = Nel.cons(Identity.Identity(1), Nel.singleton(Identity.Identity(2)));
        assertEq(expected = Identity.Identity(Nel.cons(1, Nel.singleton(2))), Nel.sequence(l))

    @Test
    def sequence03(): Unit \ Assert =
        let l = Nel.cons(Identity.Identity(1), Nel.cons(Identity.Identity(2), Nel.singleton(Identity.Identity(3))));
        assertEq(expected = Identity.Identity(Nel.cons(1, Nel.cons(2, Nel.singleton(3)))), Nel.sequence(l))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nel.singleton(1);
        let ans = Nel.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(Nel.singleton(1)), ans);
        assertEq(expected = 1, Ref.get(st))
    }

    @Test
    def traverse02(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nel.cons(1, Nel.singleton(2));
        let ans = Nel.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(Nel.cons(1, Nel.singleton(2))), ans);
        assertEq(expected = 2, Ref.get(st))
    }

    @Test
    def traverse03(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nel.cons(1, Nel.cons(2, Nel.singleton(3)));
        let ans = Nel.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(Nel.cons(1, Nel.cons(2, Nel.singleton(3)))), ans);
        assertEq(expected = 3, Ref.get(st))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith01(): Unit \ Assert =
        assertEq(expected = Map#{1 => 0, 3 => 0, 6 => 0, 7 => 0}, Nel.toMapWith(x -> 0 * x, Nel(7, 1 :: 3 :: 6 :: Nil)))

    @Test
    def toMapWith02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 2, 3 => 6, 6 => 12, 7 => 14}, Nel.toMapWith(x -> 2 * x, Nel(7, 1 :: 3 :: 6 :: Nil)))

    @Test
    def toMapWith03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}, Nel.toMapWith(x -> x + 10, Nel(7, 1 :: 3 :: 6 :: Nil)))

    @Test
    def toMapWith04(): Unit \ Assert =
        assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}, Nel.toMapWith(x -> x + 10, Nel(7, 3 :: 6 :: 1 :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert =
        assertEq(expected = "1", Nel.join(",", Nel(1, Nil)))

    @Test
    def join02(): Unit \ Assert =
        assertEq(expected = "1,2,3", Nel.join(",", Nel(1, 2 :: 3 :: Nil)))

    @Test
    def join03(): Unit \ Assert =
        assertEq(expected = "1,2,3", Nel.join(",", Nel("1", "2" :: "3" :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert =
        assertEq(expected = "2", Nel.joinWith(x -> "${x + 1}", ",", Nel(1, Nil)))

    @Test
    def joinWith02(): Unit \ Assert =
        assertEq(expected = "2,3,4", Nel.joinWith(x -> "${x + 1}", ",", Nel(1, 2 :: 3 :: Nil)))

    @Test
    def joinWith03(): Unit \ Assert =
        assertEq(expected = "11,22,33", Nel.joinWith(x -> x + x, ",", Nel("1", "2" :: "3" :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ {Assert, NonDet} =
        run {
            let l = Nel.singleton(1);
            match Nel.shuffle(l) {
                case Some(ll) => {
                    assertEq(expected = 1, Nel.length(ll));
                    assertEq(expected = Set#{1}, ll |> Nel.toList |> List.toSet)
                }
                case _        => assertEq(expected = (), unreachable!())
            }
        } with Shuffle.runWithIO

    @Test
    def shuffle02(): Unit \ {Assert, NonDet} =
        run {
            let l = Nel.cons(1, Nel.cons(2, Nel.singleton(3)));
            match Nel.shuffle(l) {
                case Some(ll) => {
                    assertEq(expected = 3, Nel.length(ll));
                    assertEq(expected = Set#{1, 2, 3}, ll |> Nel.toList |> List.toSet)
                }
                case _        => assertEq(expected = (), unreachable!())
            }
        } with Shuffle.runWithIO

    @Test
    def shuffle03(): Unit \ {Assert, NonDet} =
        run {
            let l = Nel.cons(1, Nel.cons(2, Nel.cons(3, Nel.cons(4, Nel.cons(5, Nel.cons(6, Nel.cons(7, Nel.singleton(8))))))));
            match Nel.shuffle(l) {
                case Some(ll) => {
                    assertEq(expected = 8, Nel.length(ll));
                    assertEq(expected = Set#{1, 2, 3, 4, 5, 6, 7, 8}, ll |> Nel.toList |> List.toSet)
                }
                case _        => assertEq(expected = (), unreachable!())
            }
        } with Shuffle.runWithIO

}
