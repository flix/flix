mod TestNel {

    use Hash.hash
    use Nel.Nel

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool = Nel.toString(Nel(1, Nil)) == "Nel(1, Nil)"

    @Test
    def toString02(): Bool = Nel.toString(Nel(1, 2 :: 3 :: 4 :: Nil)) == "Nel(1, 2 :: 3 :: 4 :: Nil)"

    @Test
    def toString03(): Bool = Nel.toString(Nel('a', 'b' :: Nil)) == "Nel(a, b :: Nil)"

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Bool = Nel.singleton(1) == Nel(1, Nil)

    @Test
    def singleton02(): Bool = Nel.singleton(2) == Nel(2, Nil)

    @Test
    def singleton03(): Bool = Nel.singleton('a') == Nel('a', Nil)

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def cons01(): Bool = Nel.cons(1, Nel.singleton(2)) == Nel(1, 2 :: Nil)

    @Test
    def cons02(): Bool = Nel.cons(1, Nel.cons(2, Nel.singleton(3))) == Nel(1, 2 :: 3 :: Nil)

    @Test
    def cons03(): Bool = Nel.cons(1, Nel.cons(2, Nel.cons(3, Nel.singleton(4)))) == Nel(1, 2 :: 3 :: 4 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Bool = Nel.head(Nel(123, Nil)) == 123

    @Test
    def head02(): Bool = Nel.head(Nel(123, 1 :: Nil)) == 123

    @Test
    def head03(): Bool = Nel.head(Nel(123, 1 :: 2 :: Nil)) == 123

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Bool = Nel.last(Nel(123, Nil)) == 123

    @Test
    def last02(): Bool = Nel.last(Nel(123, 1 :: Nil)) == 1

    @Test
    def last03(): Bool = Nel.last(Nel(123, 1 :: 2 :: Nil)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Bool = Nel.init(Nel(1, Nil)) == Nil

    @Test
    def init02(): Bool = Nel.init(Nel(1, 2 :: Nil)) == 1 :: Nil

    @Test
    def init03(): Bool = Nel.init(Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tail01(): Bool = Nel.tail(Nel(1, Nil)) == Nil

    @Test
    def tail02(): Bool = Nel.tail(Nel(1, 2 :: Nil)) == 2 :: Nil

    @Test
    def tail03(): Bool = Nel.tail(Nel(1, 2 :: 3 :: Nil)) == 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Bool = Nel.length(Nel(1, Nil)) == 1

    @Test
    def length02(): Bool = Nel.length(Nel(1, 2 :: Nil)) == 2

    @Test
    def length03(): Bool = Nel.length(Nel(1, 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Bool = Nel(1, Nil) `Nel.append` Nel(2, Nil) == Nel(1, 2 :: Nil)

    @Test
    def append02(): Bool = Nel(1, 2 :: Nil) `Nel.append` Nel(3, Nil) == Nel(1, 2 :: 3 :: Nil)

    @Test
    def append03(): Bool = Nel(1, Nil) `Nel.append` Nel(2, 3 :: Nil) == Nel(1, 2 :: 3 :: Nil)

    @Test
    def append04(): Bool = Nel(1, 2 :: Nil) `Nel.append` Nel(3, 4 :: Nil) == Nel(1, 2 :: 3 :: 4 :: Nil)

    @Test
    def append05(): Bool = Nel(1, 2 :: 3 :: Nil) `Nel.append` Nel(4, 5 :: 6 :: Nil) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool = Nel.memberOf(0, Nel(1, Nil)) == false

    @Test
    def memberOf02(): Bool = Nel.memberOf(0, Nel(1, 2 :: Nil)) == false

    @Test
    def memberOf03(): Bool = Nel.memberOf(0, Nel(1, 2 :: 3 :: Nil)) == false

    @Test
    def memberOf04(): Bool = Nel.memberOf(1, Nel(1, 2 :: 3 :: Nil))

    @Test
    def memberOf05(): Bool = Nel.memberOf(2, Nel(1, 2 :: 3 :: Nil))

    @Test
    def memberOf06(): Bool = Nel.memberOf(3, Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool = Nel.findLeft(x -> x == 0, Nel(1, Nil)) == None

    @Test
    def findLeft02(): Bool = Nel.findLeft(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @Test
    def findLeft03(): Bool = Nel.findLeft(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @Test
    def findLeft04(): Bool = Nel.findLeft(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @Test
    def findLeft05(): Bool = Nel.findLeft(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @Test
    def findLeft06(): Bool = Nel.findLeft(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool = Nel.findRight(x -> x == 0, Nel(1, Nil)) == None

    @Test
    def findRight02(): Bool = Nel.findRight(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @Test
    def findRight03(): Bool = Nel.findRight(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @Test
    def findRight04(): Bool = Nel.findRight(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @Test
    def findRight05(): Bool = Nel.findRight(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @Test
    def findRight06(): Bool = Nel.findRight(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool = Nel.map(x -> x + 1, Nel(1, Nil)) == Nel(2, Nil)

    @Test
    def map02(): Bool = Nel.map(x -> x + 1, Nel(1, 2 :: Nil)) == Nel(2, 3 :: Nil)

    @Test
    def map03(): Bool = Nel.map(x -> x + 1, Nel(1, 2 :: 3 :: Nil)) == Nel(2, 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Bool = Nel.mapWithIndex((i, _) -> i, Nel(21, Nil)) == Nel(0, Nil)

    @Test
    def mapWithIndex02(): Bool = Nel.mapWithIndex((i, _) -> i, Nel(21, 42 :: Nil)) == Nel(0, 1 :: Nil)

    @Test
    def mapWithIndex03(): Bool = Nel.mapWithIndex((i, _) -> i, Nel(21, 42 :: 84 :: Nil)) == Nel(0, 1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Bool = Nel.ap(Nel((x -> x+1), Nil), Nel(5, Nil)) == Nel(6, Nil)

    @Test
    def ap02(): Bool = Nel.ap(Nel((x -> x+1), Nil), Nel(0, 5 :: Nil)) == Nel(1, 6 :: Nil)

    @Test
    def ap03(): Bool = Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: Nil)) == Nel(1, 5 :: 0 :: 8 :: Nil)

    @Test
    def ap04(): Bool = Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: 8 :: Nil)) == Nel(1, 5 :: 9 :: 0 :: 8 :: 16 :: Nil)

    @Test
    def ap05(): Bool = Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: 8 :: 16 :: Nil)) == Nel(1, 5 :: 9 :: 17 :: 0 :: 8 :: 16 :: 32 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Bool = Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, Nil)) == Nel(2, Nil)

    @Test
    def flatMap02(): Bool = Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: Nil)) == Nel(2, 3 :: Nil)

    @Test
    def flatMap03(): Bool = Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: 3 :: Nil)) == Nel(2, 3 :: 4 :: Nil)

    @Test
    def flatMap04(): Bool = Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, Nil)) == Nel(1, 2 :: Nil)

    @Test
    def flatMap05(): Bool = Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, 4 :: Nil)) == Nel(1, 2 :: 4 :: 5 :: Nil)

    @Test
    def flatMap06(): Bool = Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, 3 :: 5 :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Bool = Nel.reverse(Nel(1, Nil)) == Nel(1, Nil)

    @Test
    def reverse02(): Bool = Nel.reverse(Nel(1, 2 :: Nil)) == Nel(2, 1 :: Nil)

    @Test
    def reverse03(): Bool = Nel.reverse(Nel(1, 2 :: 3 :: Nil)) == Nel(3, 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Bool = Nel.replace(from = 1, to = 42, Nel(1, Nil)) == Nel(42, Nil)

    @Test
    def replace02(): Bool = Nel.replace(from = 7, to = 42, Nel(1, Nil)) == Nel(1, Nil)

    @Test
    def replace03(): Bool = Nel.replace(from = 2, to = 42, Nel(1, 2 :: Nil)) == Nel(1, 42 :: Nil)

    @Test
    def replace04(): Bool = Nel.replace(from = 7, to = 42, Nel(1, 2 :: Nil)) == Nel(1, 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // permutations                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def permutations01(): Bool = Nel.permutations(Nel(1, Nil)) == Nel(1 :: Nil, Nil)

    @Test
    def permutations02(): Bool = Nel.permutations(Nel(1, 2 :: Nil)) == Nel(1 :: 2 :: Nil, (2 :: 1 :: Nil) :: Nil)

    @Test
    def permutations03(): Bool = Nel.permutations(Nel(1, 2 :: 3 :: Nil)) == Nel(1 :: 2 :: 3 :: Nil,
                                                                           (1 :: 3 :: 2 :: Nil) ::
                                                                           (2 :: 1 :: 3 :: Nil) ::
                                                                           (2 :: 3 :: 1 :: Nil) ::
                                                                           (3 :: 1 :: 2 :: Nil) ::
                                                                           (3 :: 2 :: 1 :: Nil) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // subsequences                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def subsequences01(): Bool = Nel.subsequences(Nel(1, Nil)) == Nel(1 :: Nil, (Nil) :: Nil)

    @Test
    def subsequences02(): Bool = Nel.subsequences(Nel(1, 2 :: Nil)) == Nel(1 :: 2 :: Nil, (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil)

    @Test
    def subsequences03(): Bool = Nel.subsequences(Nel(1, 2 :: 3 :: Nil)) == Nel(1 :: 2 :: 3 :: Nil,
                                                                           (1 :: 2 :: Nil) ::
                                                                           (1 :: 3 :: Nil) ::
                                                                           (1 :: Nil) ::
                                                                           (2 :: 3 :: Nil) ::
                                                                           (2 :: Nil) ::
                                                                           (3 :: Nil) ::
                                                                           (Nil) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Bool = Nel.intersperse(7, Nel(1, Nil)) == Nel(1, Nil)

    @Test
    def intersperse02(): Bool = Nel.intersperse(7, Nel(1, 2 :: Nil)) == Nel(1, 7 :: 2 :: Nil)

    @Test
    def intersperse03(): Bool = Nel.intersperse(7, Nel(1, 2 :: 3 :: Nil)) == Nel(1, 7 :: 2 :: 7 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Bool = Nel.fold(Nel("a", Nil)) == "a"

    @Test
    def fold02(): Bool = Nel.fold(Nel((), Nil)) == ()

    @Test
    def fold03(): Bool = Nel.fold(Nel("a", "b" :: "c" :: Nil)) == "abc"

    @Test
    def fold04(): Bool = Nel.fold(Nel(("a", ()), ("b", ()) :: ("c", ()) :: Nil)) == ("abc", ())

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool = Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, Nil)) == -1

    @Test
    def foldLeft02(): Bool = Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: Nil)) == -3

    @Test
    def foldLeft03(): Bool = Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool = Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, Nil)) == -1

    @Test
    def foldRight02(): Bool = Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: Nil)) == -3

    @Test
    def foldRight03(): Bool = Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool = Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, Nil)) == -1

    @Test
    def foldRightWithCont02(): Bool = Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, 2 :: Nil)) == -3

    @Test
    def foldRightWithCont03(): Bool = Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool =
        Nel.foldMap(x -> 2 * x, Nel(1, 2 :: Nil)) == 6

    @Test
    def foldMap02(): Bool =
        Nel.foldMap(x -> if (x == "a") "b" else x, Nel("a", Nil)) == "b"

    @Test
    def foldMap03(): Bool =
        Nel.foldMap(x -> if (x == "c") "b" else x, Nel("a", "b" :: "c" :: Nil)) == "abb"

    @Test
    def foldMap04(): Bool =
        Nel.foldMap(Int32.toString, Nel(1, 2 :: 3 :: Nil)) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduce01(): Bool = Nel.reduce(Nel("a", Nil)) == "a"

    @Test
    def reduce02(): Bool = Nel.reduce(Nel((), Nil)) == ()

    @Test
    def reduce03(): Bool = Nel.reduce(Nel("a", "b" :: "c" :: Nil)) == "abc"

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Bool = Nel.reduceLeft((acc, x) -> acc - x, Nel(1, Nil)) == 1

    @Test
    def reduceLeft02(): Bool = Nel.reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: Nil)) == -1

    @Test
    def reduceLeft03(): Bool = Nel.reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: 3 :: Nil)) == -4

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Bool = Nel.reduceRight((x, acc) -> acc - x, Nel(1, Nil)) == 1

    @Test
    def reduceRight02(): Bool = Nel.reduceRight((x, acc) -> acc - x, Nel(1, 2 :: Nil)) == 1

    @Test
    def reduceRight03(): Bool = Nel.reduceRight((x, acc) -> acc - x, Nel(1, 2 :: 3 :: Nil)) == 0

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool = Nel.count(x -> x == 1, Nel(1, Nil)) == 1

    @Test
    def count02(): Bool = Nel.count(x -> x == 2, Nel(1, Nil)) == 0

    @Test
    def count03(): Bool = Nel.count(x -> x == 2, Nel(1, 2 :: 1 :: 2 :: Nil)) == 2

    @Test
    def count04(): Bool = Nel.count(x -> x == 2, Nel(2, 2 :: 1 :: 2 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool =
        Nel.sum(Nel(1, Nil)) == 1

    @Test
    def sum02(): Bool =
        Nel.sum(Nel(1, 2 :: 3 :: Nil)) == 6

    @Test
    def sum03(): Bool =
        Nel.sum(Nel(1, 2 :: 3 :: -3 :: Nil)) == 3

    @Test
    def sum04(): Bool =
        Nel.sum(Nel(-1, -2 :: -3 :: -4 :: Nil)) == -10

    @Test
    def sum05(): Bool =
        Nel.sum(Nel(10, -10 :: Nil)) == 0

    @Test
    def sum06(): Bool = match List.range(1, 101) {
        case x :: xs => Nel.sum(Nel(x, xs)) == 5050
        case _       => unreachable!()
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        Nel.sumWith(x -> x + 1, Nel(1, Nil)) == 2

    @Test
    def sumWith02(): Bool =
        Nel.sumWith(x -> x + 1, Nel(1, 2 :: 3 :: Nil)) == 9

    @Test
    def sumWith03(): Bool =
        Nel.sumWith(x -> x + 1, Nel(1, 2 :: 3 :: -3 :: Nil)) == 7

    @Test
    def sumWith04(): Bool =
        Nel.sumWith(x -> x + 1, Nel(-1, -2 :: -3 :: -4 :: Nil)) == -6

    @Test
    def sumWith05(): Bool =
        Nel.sumWith(x -> x + 1, Nel(10, -10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nil)) == Nel(1, Nil)

    @Test
    def flatten02(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nil)) == Nel(1, 2 :: Nil)

    @Test
    def flatten03(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nel(3, Nil) :: Nil)) == Nel(1, 2 :: 3 :: Nil)

    @Test
    def flatten04(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nel(2, 3 :: Nil) :: Nel(4, 5 :: Nil) :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def flatten05(): Bool = Nel.flatten(Nel(Nel(1, 2 :: 3 :: Nil), Nel(4, 5 :: Nil) :: Nel(6, 7 :: Nil) :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool = Nel.exists(x -> x == 1, Nel(1, Nil)) == true

    @Test
    def exists02(): Bool = Nel.exists(x -> x == 2, Nel(1, Nil)) == false

    @Test
    def exists03(): Bool = Nel.exists(x -> x == 1, Nel(1, 2 :: Nil)) == true

    @Test
    def exists04(): Bool = Nel.exists(x -> x == 2, Nel(1, 2 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool = Nel.forAll(x -> x == 1, Nel(1, Nil)) == true

    @Test
    def forAll02(): Bool = Nel.forAll(x -> x == 2, Nel(1, Nil)) == false

    @Test
    def forAll03(): Bool = Nel.forAll(x -> x == 1, Nel(1, 1 :: Nil)) == true

    @Test
    def forAll04(): Bool = Nel.forAll(x -> x == 1, Nel(1, 2 :: Nil)) == false

    @Test
    def forAll05(): Bool = Nel.forAll(x -> x == 1, Nel(2, 1 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Bool = Nel.filter(x -> x > 0, Nel(1, Nil)) == 1 :: Nil

    @Test
    def filter02(): Bool = Nel.filter(x -> x > 1, Nel(1, Nil)) == Nil

    @Test
    def filter03(): Bool = Nel.filter(x -> x > 0, Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @Test
    def filter04(): Bool = Nel.filter(x -> x > 1, Nel(1, 2 :: Nil)) == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Bool = Nel.zip(Nel(1, Nil), Nel(1, Nil)) == Nel((1, 1), Nil)

    @Test
    def zip02(): Bool = Nel.zip(Nel(1, 2 :: Nil), Nel(1, Nil)) == Nel((1, 1), Nil)

    @Test
    def zip03(): Bool = Nel.zip(Nel(1, Nil), Nel(1, 2 :: Nil)) == Nel((1, 1), Nil)

    @Test
    def zip04(): Bool = Nel.zip(Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Nel((1, 1), (2, 2) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, Nil)) == Nel(2, Nil)

    @Test
    def zipWith02(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, Nil)) == Nel(2, Nil)

    @Test
    def zipWith03(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, 2 :: Nil)) == Nel(2, Nil)

    @Test
    def zipWith04(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Nel(2, 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Bool = Nel.unzip(Nel((1, 1), Nil)) == (Nel(1, Nil), Nel(1, Nil))

    @Test
    def unzip02(): Bool = Nel.unzip(Nel((1, 1), (2, 2) :: Nil)) == (Nel(1, 2 :: Nil), Nel(1, 2 :: Nil))

    @Test
    def unzip03(): Bool = Nel.unzip(Nel((1, 1), (2, 2) :: (3, 3) :: Nil)) == (Nel(1, 2 :: 3 :: Nil), Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Bool = Nel.zipWithIndex(Nel(1, Nil)) == Nel((0, 1), Nil)

    @Test
    def zipWithIndex02(): Bool = Nel.zipWithIndex(Nel(1, 2 :: Nil)) == Nel((0, 1), (1, 2) :: Nil)

    @Test
    def zipWithIndex03(): Bool = Nel.zipWithIndex(Nel(2, 1 :: Nil)) == Nel((0, 2), (1, 1) :: Nil)

    @Test
    def zipWithIndex04(): Bool = Nel.zipWithIndex(Nel(1, 2 :: 3 :: Nil)) == Nel((0, 1), (1, 2) :: (2, 3) :: Nil)

    @Test
    def zipWithIndex05(): Bool =
        Nel.zipWithIndex(Nel(1, 2 :: 3 :: 4 :: 5 :: Nil)) == Nel((0, 1), (1, 2) :: (2, 3) :: (3, 4) :: (4, 5) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithA01(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, Nil), Nel(1, Nil)) == Identity.Identity(Nel(2, Nil))

    @Test
    def zipWithA02(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, 2 :: Nil), Nel(1, Nil)) == Identity.Identity(Nel(2, Nil))

    @Test
    def zipWithA03(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, Nil), Nel(1, 2 :: Nil)) == Identity.Identity(Nel(2, Nil))

    @Test
    def zipWithA04(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Identity.Identity(Nel(2, 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool = Nel.toList(Nel(1, Nil)) == 1 :: Nil

    @Test
    def toList02(): Bool = Nel.toList(Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @Test
    def toList03(): Bool = Nel.toList(Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
        let a = Nel.toArray(rc, Nel.singleton(1));
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def toArray02(): Bool = region rc {
        let a = Nel.toArray(rc, Nel(1, List#{2}));
        Array.sameElements(a, Array#{1, 2} @ rc)
    }

    @Test
    def toArray03(): Bool = region rc {
        let a = Nel.toArray(rc, Nel(1, List#{2, 3}));
        Array.sameElements(a, Array#{1, 2, 3} @ rc)
    }

    @Test
    def toArray04(): Bool = region rc {
        let a = Nel.toArray(rc, Nel(3, List#{2, 3}));
        Array.sameElements(a, Array#{3, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool =
        let l = Nel(1, List#{});
        Nel.toVector(l) == Vector#{1}

    @Test
    def toVector02(): Bool =
        let l = Nel(1, List#{2});
        Nel.toVector(l) == Vector#{1, 2}

    @Test
    def toVector03(): Bool =
        let l = Nel(1, List#{2, 3});
        Nel.toVector(l) == Vector#{1, 2, 3}

    @Test
    def toVector04(): Bool =
        let l = Nel(3, List#{2, 3});
        Nel.toVector(l) == Vector#{3, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let l = Nel(1, 2 :: Nil);
        let d1 = Nel.toMutDeque(rc, l);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack(1, d2);
        MutDeque.pushBack(2, d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let l = Nel(3, 1 :: 2 :: Nil);
        let d1 = Nel.toMutDeque(rc, l);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack(1, d2);
        MutDeque.pushBack(2, d2);
        MutDeque.pushFront(3, d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let l = Nel('a', 'b' :: 'c' :: Nil);
        let d1 = Nel.toMutDeque(rc, l);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront('c', d2);
        MutDeque.pushFront('b', d2);
        MutDeque.pushFront('a', d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let ri = ref 21 @ rc;
        Nel.forEach(x -> ri := x, Nel.singleton(1));
        1 == deref ri
    }

    @Test
    def forEach02(): Bool = region rc {
        let ri = ref 21 @ rc;
        Nel.forEach(x -> ri := x, Nel(1, 2 :: 3 :: Nil));
        3 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let ri = ref 21 @ rc;
        Nel.forEachWithIndex((i, _) -> ri := i, Nel.singleton(1));
        0 == deref ri
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let ri = ref 21 @ rc;
        Nel.forEachWithIndex((i, _) -> ri := i, Nel(1, List#{2, 3}));
        2 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Bool = hash(Nel(1, Nil)) == hash(Nel(1, Nil))

    @Test
    def hash02(): Bool = hash(Nel(1, 2 :: 3 :: Nil)) == hash(Nel(1, 2 :: 3 :: Nil))

    @Test
    def hash03(): Bool = hash(Nel(1, 3 :: 2 :: Nil)) != hash(Nel(1, 2 :: 3 :: Nil))

    @Test
    def hash04(): Bool = hash(Nel(1, Nil)) != hash(Nel(2, Nil))

    @Test
    def hash05(): Bool = hash(Nel("c", "b" :: "a" :: Nil)) != hash(Nel("c", "a" :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(1, Nil)) == 1

    @Test
    def minimumBy02(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(1, 2 :: 3 :: Nil)) == 1

    @Test
    def minimumBy03(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(1, 2 :: 0 :: 3 :: Nil)) == 0

    @Test
    def minimumBy04(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 3

    @Test
    def minimumBy05(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(1, Nil)) == 1

    @Test
    def maximumBy02(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(1, 2 :: 3 :: Nil)) == 3

    @Test
    def maximumBy03(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(1, 2 :: 0 :: 3 :: Nil)) == 3

    @Test
    def maximumBy04(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 9

    @Test
    def maximumBy05(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)) == 10

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
    def sortWith02(): Bool =
        Nel.sortWith(cmp, Nel(0, Nil)) == Nel(0, Nil)

    @Test
    def sortWith03(): Bool =
        Nel.sortWith(cmp, Nel(0, 1 :: Nil)) == Nel(0, 1 :: Nil)

    @Test
    def sortWith04(): Bool =
        Nel.sortWith(cmp, Nel(1, 0 :: Nil)) == Nel(0, 1 :: Nil)

    @Test
    def sortWith05(): Bool =
        Nel.sortWith(cmp, Nel(1, 1 :: Nil)) == Nel(1, 1 :: Nil)

    @Test
    def sortWith06(): Bool =
        Nel.sortWith(cmp, Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith07(): Bool =
        Nel.sortWith(cmp, Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith08(): Bool =
        Nel.sortWith(cmp, Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith09(): Bool =
        Nel.sortWith(cmp, Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(0, 1 :: 2 :: 2 :: 3 :: 4 :: Nil)

    @Test
    def sortWith10(): Bool =
        Nel.sortWith(flip(cmp), Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith11(): Bool =
        Nel.sortWith(flip(cmp), Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith12(): Bool =
        Nel.sortWith(flip(cmp), Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith13(): Bool =
        Nel.sortWith(flip(cmp), Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(4, 3 :: 2 :: 2 :: 1 :: 0 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(xs: Nel[Int32]) : Bool =
        Nel.sort(xs) == Nel.sortWith(cmp, xs)

    @Test
    def sort02(): Bool = testSortVsSortWith(Nel(0, Nil))

    @Test
    def sort03(): Bool = testSortVsSortWith(Nel(0, 1 :: Nil))

    @Test
    def sort04(): Bool = testSortVsSortWith(Nel(1, 0 :: Nil))

    @Test
    def sort05(): Bool = testSortVsSortWith(Nel(1, 1 :: Nil))

    @Test
    def sort06(): Bool = testSortVsSortWith(Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sort07(): Bool = testSortVsSortWith(Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sort08(): Bool = testSortVsSortWith(Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sort09(): Bool = testSortVsSortWith(Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Nel[Int32]) : Bool =
        (Nel.sortBy(identity, a) == Nel.sort(a)) and
        (Nel.sortBy(x -> 4 * x + 7, a) == Nel.sort(a)) and
        (Nel.sortBy(x -> -x, a) == Nel.sortWith(flip(cmp),a))

    @Test
    def sortBy02(): Bool = testSortByVsSort(Nel(0, Nil))

    @Test
    def sortBy03(): Bool = testSortByVsSort(Nel(0, 1 :: Nil))

    @Test
    def sortBy04(): Bool = testSortByVsSort(Nel(1, 0 :: Nil))

    @Test
    def sortBy05(): Bool = testSortByVsSort(Nel(1, 1 :: Nil))

    @Test
    def sortBy06(): Bool = testSortByVsSort(Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sortBy07(): Bool = testSortByVsSort(Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sortBy08(): Bool = testSortByVsSort(Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortBy09(): Bool = testSortByVsSort(Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        Nel(1, 2 :: 3 :: Nil) |> Nel.iterator(rc) |> Iterator.toNel == Some(Nel(1, 2 :: 3 :: Nil))
    }

    @Test
    def iterator02(): Bool = region rc {
        Nel(-100, List.range(-99, 100)) |> Nel.iterator(rc) |> Iterator.toList == List.range(-100, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def enumerator01(): Bool = region rc {
        Nel(1, 2 :: 3 :: Nil) |> Nel.enumerator(rc) |> Iterator.toList == (0, 1) :: (1, 2) :: (2, 3) :: Nil
    }

    @Test
    def enumerator02(): Bool = region rc {
        Nel(-100, List.range(-99, 100)) |> Nel.enumerator(rc) |> Iterator.toList == List.mapWithIndex((ix, i) -> (ix, i), (List.range(-100, 100)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Bool =
        let l = Nel.singleton(Identity.Identity(1));
        Nel.sequence(l) == Identity.Identity(Nel.singleton(1))

    @Test
    def sequence02(): Bool =
        let l = Nel.cons(Identity.Identity(1), Nel.singleton(Identity.Identity(2)));
        Nel.sequence(l) == Identity.Identity(Nel.cons(1, Nel.singleton(2)))

    @Test
    def sequence03(): Bool =
        let l = Nel.cons(Identity.Identity(1), Nel.cons(Identity.Identity(2), Nel.singleton(Identity.Identity(3))));
        Nel.sequence(l) == Identity.Identity(Nel.cons(1, Nel.cons(2, Nel.singleton(3))))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = Nel.singleton(1);
        let ans = Nel.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(Nel.singleton(1)) and deref st == 1
    }

    @Test
    def traverse02(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = Nel.cons(1, Nel.singleton(2));
        let ans = Nel.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(Nel.cons(1, Nel.singleton(2))) and deref st == 2
    }

    @Test
    def traverse03(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = Nel.cons(1, Nel.cons(2, Nel.singleton(3)));
        let ans = Nel.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(Nel.cons(1, Nel.cons(2, Nel.singleton(3)))) and deref st == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith01(): Bool =
        Nel.toMapWith(x -> 0 * x, Nel(7, 1 :: 3 :: 6 :: Nil)) == Map#{1 => 0, 3 => 0, 6 => 0, 7 => 0}

    @Test
    def toMapWith02(): Bool =
        Nel.toMapWith(x -> 2 * x, Nel(7, 1 :: 3 :: 6 :: Nil)) == Map#{1 => 2, 3 => 6, 6 => 12, 7 => 14}

    @Test
    def toMapWith03(): Bool =
        Nel.toMapWith(x -> x + 10, Nel(7, 1 :: 3 :: 6 :: Nil)) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

    @Test
    def toMapWith04(): Bool =
        Nel.toMapWith(x -> x + 10, Nel(7, 3 :: 6 :: 1 :: Nil)) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool =
        Nel.join(",", Nel(1, Nil)) == "1"

    @Test
    def join02(): Bool =
        Nel.join(",", Nel(1, 2 :: 3 :: Nil)) == "1,2,3"

    @Test
    def join03(): Bool =
        Nel.join(",", Nel("1", "2" :: "3" :: Nil)) == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        Nel.joinWith(x -> "${x + 1}", ",", Nel(1, Nil)) == "2"

    @Test
    def joinWith02(): Bool =
        Nel.joinWith(x -> "${x + 1}", ",", Nel(1, 2 :: 3 :: Nil)) == "2,3,4"

    @Test
    def joinWith03(): Bool =
        Nel.joinWith(x -> x + x, ",", Nel("1", "2" :: "3" :: Nil)) == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet =
        let l = Nel.singleton(1);
        match Nel.shuffle(Random.new(), l) {
            case Some(ll) => Nel.length(ll) == 1 and (ll |> Nel.toList |> List.toSet) == Set#{1}
            case _        => false
        }

    @Test
    def shuffle02(): Bool \ NonDet =
        let l = Nel.cons(1, Nel.cons(2, Nel.singleton(3)));
        match Nel.shuffle(Random.new(), l) {
            case Some(ll) => Nel.length(ll) == 3 and (ll |> Nel.toList |> List.toSet) == Set#{1, 2, 3}
            case _        => false
        }

    @Test
    def shuffle03(): Bool \ NonDet =
        let l = Nel.cons(1, Nel.cons(2, Nel.cons(3, Nel.cons(4, Nel.cons(5, Nel.cons(6, Nel.cons(7, Nel.singleton(8))))))));
        match Nel.shuffle(Random.new(), l) {
            case Some(ll) => Nel.length(ll) == 8 and
                                (ll |> Nel.toList |> List.toSet) == Set#{1, 2, 3, 4, 5, 6, 7, 8}
            case _        => false
        }

}
