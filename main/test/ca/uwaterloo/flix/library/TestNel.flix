mod TestNel {

    use Hash.hash
    use Nel.Nel

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool = Nel.toString(Nel(1, Nil)) == "Nel(1, Nil)"

    @test
    def toString02(): Bool = Nel.toString(Nel(1, 2 :: 3 :: 4 :: Nil)) == "Nel(1, 2 :: 3 :: 4 :: Nil)"

    @test
    def toString03(): Bool = Nel.toString(Nel('a', 'b' :: Nil)) == "Nel(a, b :: Nil)"

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool = Nel.singleton(1) == Nel(1, Nil)

    @test
    def singleton02(): Bool = Nel.singleton(2) == Nel(2, Nil)

    @test
    def singleton03(): Bool = Nel.singleton('a') == Nel('a', Nil)

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def cons01(): Bool = Nel.cons(1, Nel.singleton(2)) == Nel(1, 2 :: Nil)

    @test
    def cons02(): Bool = Nel.cons(1, Nel.cons(2, Nel.singleton(3))) == Nel(1, 2 :: 3 :: Nil)

    @test
    def cons03(): Bool = Nel.cons(1, Nel.cons(2, Nel.cons(3, Nel.singleton(4)))) == Nel(1, 2 :: 3 :: 4 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = Nel.head(Nel(123, Nil)) == 123

    @test
    def head02(): Bool = Nel.head(Nel(123, 1 :: Nil)) == 123

    @test
    def head03(): Bool = Nel.head(Nel(123, 1 :: 2 :: Nil)) == 123

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = Nel.last(Nel(123, Nil)) == 123

    @test
    def last02(): Bool = Nel.last(Nel(123, 1 :: Nil)) == 1

    @test
    def last03(): Bool = Nel.last(Nel(123, 1 :: 2 :: Nil)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = Nel.init(Nel(1, Nil)) == Nil

    @test
    def init02(): Bool = Nel.init(Nel(1, 2 :: Nil)) == 1 :: Nil

    @test
    def init03(): Bool = Nel.init(Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool = Nel.tail(Nel(1, Nil)) == Nil

    @test
    def tail02(): Bool = Nel.tail(Nel(1, 2 :: Nil)) == 2 :: Nil

    @test
    def tail03(): Bool = Nel.tail(Nel(1, 2 :: 3 :: Nil)) == 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = Nel.length(Nel(1, Nil)) == 1

    @test
    def length02(): Bool = Nel.length(Nel(1, 2 :: Nil)) == 2

    @test
    def length03(): Bool = Nel.length(Nel(1, 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = Nel(1, Nil) `Nel.append` Nel(2, Nil) == Nel(1, 2 :: Nil)

    @test
    def append02(): Bool = Nel(1, 2 :: Nil) `Nel.append` Nel(3, Nil) == Nel(1, 2 :: 3 :: Nil)

    @test
    def append03(): Bool = Nel(1, Nil) `Nel.append` Nel(2, 3 :: Nil) == Nel(1, 2 :: 3 :: Nil)

    @test
    def append04(): Bool = Nel(1, 2 :: Nil) `Nel.append` Nel(3, 4 :: Nil) == Nel(1, 2 :: 3 :: 4 :: Nil)

    @test
    def append05(): Bool = Nel(1, 2 :: 3 :: Nil) `Nel.append` Nel(4, 5 :: 6 :: Nil) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = Nel.memberOf(0, Nel(1, Nil)) == false

    @test
    def memberOf02(): Bool = Nel.memberOf(0, Nel(1, 2 :: Nil)) == false

    @test
    def memberOf03(): Bool = Nel.memberOf(0, Nel(1, 2 :: 3 :: Nil)) == false

    @test
    def memberOf04(): Bool = Nel.memberOf(1, Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf05(): Bool = Nel.memberOf(2, Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf06(): Bool = Nel.memberOf(3, Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = Nel.findLeft(x -> x == 0, Nel(1, Nil)) == None

    @test
    def findLeft02(): Bool = Nel.findLeft(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @test
    def findLeft03(): Bool = Nel.findLeft(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findLeft04(): Bool = Nel.findLeft(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findLeft05(): Bool = Nel.findLeft(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findLeft06(): Bool = Nel.findLeft(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = Nel.findRight(x -> x == 0, Nel(1, Nil)) == None

    @test
    def findRight02(): Bool = Nel.findRight(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @test
    def findRight03(): Bool = Nel.findRight(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findRight04(): Bool = Nel.findRight(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findRight05(): Bool = Nel.findRight(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findRight06(): Bool = Nel.findRight(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = Nel.map(x -> x + 1, Nel(1, Nil)) == Nel(2, Nil)

    @test
    def map02(): Bool = Nel.map(x -> x + 1, Nel(1, 2 :: Nil)) == Nel(2, 3 :: Nil)

    @test
    def map03(): Bool = Nel.map(x -> x + 1, Nel(1, 2 :: 3 :: Nil)) == Nel(2, 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = Nel.mapWithIndex((i, _) -> i, Nel(21, Nil)) == Nel(0, Nil)

    @test
    def mapWithIndex02(): Bool = Nel.mapWithIndex((i, _) -> i, Nel(21, 42 :: Nil)) == Nel(0, 1 :: Nil)

    @test
    def mapWithIndex03(): Bool = Nel.mapWithIndex((i, _) -> i, Nel(21, 42 :: 84 :: Nil)) == Nel(0, 1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = Nel.ap(Nel((x -> x+1), Nil), Nel(5, Nil)) == Nel(6, Nil)

    @test
    def ap02(): Bool = Nel.ap(Nel((x -> x+1), Nil), Nel(0, 5 :: Nil)) == Nel(1, 6 :: Nil)

    @test
    def ap03(): Bool = Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: Nil)) == Nel(1, 5 :: 0 :: 8 :: Nil)

    @test
    def ap04(): Bool = Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: 8 :: Nil)) == Nel(1, 5 :: 9 :: 0 :: 8 :: 16 :: Nil)

    @test
    def ap05(): Bool = Nel.ap(Nel((x -> x+1), (x -> x*2) :: Nil), Nel(0, 4 :: 8 :: 16 :: Nil)) == Nel(1, 5 :: 9 :: 17 :: 0 :: 8 :: 16 :: 32 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, Nil)) == Nel(2, Nil)

    @test
    def flatMap02(): Bool = Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: Nil)) == Nel(2, 3 :: Nil)

    @test
    def flatMap03(): Bool = Nel.flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: 3 :: Nil)) == Nel(2, 3 :: 4 :: Nil)

    @test
    def flatMap04(): Bool = Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, Nil)) == Nel(1, 2 :: Nil)

    @test
    def flatMap05(): Bool = Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, 4 :: Nil)) == Nel(1, 2 :: 4 :: 5 :: Nil)

    @test
    def flatMap06(): Bool = Nel.flatMap(x -> Nel(x, (x + 1) :: Nil), Nel(1, 3 :: 5 :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = Nel.reverse(Nel(1, Nil)) == Nel(1, Nil)

    @test
    def reverse02(): Bool = Nel.reverse(Nel(1, 2 :: Nil)) == Nel(2, 1 :: Nil)

    @test
    def reverse03(): Bool = Nel.reverse(Nel(1, 2 :: 3 :: Nil)) == Nel(3, 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = Nel.replace(src = 1, dst = 42, Nel(1, Nil)) == Nel(42, Nil)

    @test
    def replace02(): Bool = Nel.replace(src = 7, dst = 42, Nel(1, Nil)) == Nel(1, Nil)

    @test
    def replace03(): Bool = Nel.replace(src = 2, dst = 42, Nel(1, 2 :: Nil)) == Nel(1, 42 :: Nil)

    @test
    def replace04(): Bool = Nel.replace(src = 7, dst = 42, Nel(1, 2 :: Nil)) == Nel(1, 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // permutations                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def permutations01(): Bool = Nel.permutations(Nel(1, Nil)) == Nel(1 :: Nil, Nil)

    @test
    def permutations02(): Bool = Nel.permutations(Nel(1, 2 :: Nil)) == Nel(1 :: 2 :: Nil, (2 :: 1 :: Nil) :: Nil)

    @test
    def permutations03(): Bool = Nel.permutations(Nel(1, 2 :: 3 :: Nil)) == Nel(1 :: 2 :: 3 :: Nil,
                                                                           (1 :: 3 :: 2 :: Nil) ::
                                                                           (2 :: 1 :: 3 :: Nil) ::
                                                                           (2 :: 3 :: 1 :: Nil) ::
                                                                           (3 :: 1 :: 2 :: Nil) ::
                                                                           (3 :: 2 :: 1 :: Nil) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // subsequences                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def subsequences01(): Bool = Nel.subsequences(Nel(1, Nil)) == Nel(1 :: Nil, (Nil) :: Nil)

    @test
    def subsequences02(): Bool = Nel.subsequences(Nel(1, 2 :: Nil)) == Nel(1 :: 2 :: Nil, (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil)

    @test
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

    @test
    def intersperse01(): Bool = Nel.intersperse(7, Nel(1, Nil)) == Nel(1, Nil)

    @test
    def intersperse02(): Bool = Nel.intersperse(7, Nel(1, 2 :: Nil)) == Nel(1, 7 :: 2 :: Nil)

    @test
    def intersperse03(): Bool = Nel.intersperse(7, Nel(1, 2 :: 3 :: Nil)) == Nel(1, 7 :: 2 :: 7 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool = Nel.fold(Nel("a", Nil)) == "a"

    @test
    def fold02(): Bool = Nel.fold(Nel((), Nil)) == ()

    @test
    def fold03(): Bool = Nel.fold(Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def fold04(): Bool = Nel.fold(Nel(("a", ()), ("b", ()) :: ("c", ()) :: Nil)) == ("abc", ())

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, Nil)) == -1

    @test
    def foldLeft02(): Bool = Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: Nil)) == -3

    @test
    def foldLeft03(): Bool = Nel.foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, Nil)) == -1

    @test
    def foldRight02(): Bool = Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: Nil)) == -3

    @test
    def foldRight03(): Bool = Nel.foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, Nil)) == -1

    @test
    def foldRightWithCont02(): Bool = Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, 2 :: Nil)) == -3

    @test
    def foldRightWithCont03(): Bool = Nel.foldRightWithCont((x, k) -> k() - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool =
        Nel.foldMap(x -> 2 * x, Nel(1, 2 :: Nil)) == 6

    @test
    def foldMap02(): Bool =
        Nel.foldMap(x -> if (x == "a") "b" else x, Nel("a", Nil)) == "b"

    @test
    def foldMap03(): Bool =
        Nel.foldMap(x -> if (x == "c") "b" else x, Nel("a", "b" :: "c" :: Nil)) == "abb"

    @test
    def foldMap04(): Bool =
        Nel.foldMap(Int32.toString, Nel(1, 2 :: 3 :: Nil)) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduce01(): Bool = Nel.reduce(Nel("a", Nil)) == "a"

    @test
    def reduce02(): Bool = Nel.reduce(Nel((), Nil)) == ()

    @test
    def reduce03(): Bool = Nel.reduce(Nel("a", "b" :: "c" :: Nil)) == "abc"

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = Nel.reduceLeft((acc, x) -> acc - x, Nel(1, Nil)) == 1

    @test
    def reduceLeft02(): Bool = Nel.reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: Nil)) == -1

    @test
    def reduceLeft03(): Bool = Nel.reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: 3 :: Nil)) == -4

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = Nel.reduceRight((x, acc) -> acc - x, Nel(1, Nil)) == 1

    @test
    def reduceRight02(): Bool = Nel.reduceRight((x, acc) -> acc - x, Nel(1, 2 :: Nil)) == 1

    @test
    def reduceRight03(): Bool = Nel.reduceRight((x, acc) -> acc - x, Nel(1, 2 :: 3 :: Nil)) == 0

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = Nel.count(x -> x == 1, Nel(1, Nil)) == 1

    @test
    def count02(): Bool = Nel.count(x -> x == 2, Nel(1, Nil)) == 0

    @test
    def count03(): Bool = Nel.count(x -> x == 2, Nel(1, 2 :: 1 :: 2 :: Nil)) == 2

    @test
    def count04(): Bool = Nel.count(x -> x == 2, Nel(2, 2 :: 1 :: 2 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        Nel.sum(Nel(1, Nil)) == 1

    @test
    def sum02(): Bool =
        Nel.sum(Nel(1, 2 :: 3 :: Nil)) == 6

    @test
    def sum03(): Bool =
        Nel.sum(Nel(1, 2 :: 3 :: -3 :: Nil)) == 3

    @test
    def sum04(): Bool =
        Nel.sum(Nel(-1, -2 :: -3 :: -4 :: Nil)) == -10

    @test
    def sum05(): Bool =
        Nel.sum(Nel(10, -10 :: Nil)) == 0

    @test
    def sum06(): Bool = match List.range(1, 101) {
        case x :: xs => Nel.sum(Nel(x, xs)) == 5050
        case _       => unreachable!()
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        Nel.sumWith(x -> x + 1, Nel(1, Nil)) == 2

    @test
    def sumWith02(): Bool =
        Nel.sumWith(x -> x + 1, Nel(1, 2 :: 3 :: Nil)) == 9

    @test
    def sumWith03(): Bool =
        Nel.sumWith(x -> x + 1, Nel(1, 2 :: 3 :: -3 :: Nil)) == 7

    @test
    def sumWith04(): Bool =
        Nel.sumWith(x -> x + 1, Nel(-1, -2 :: -3 :: -4 :: Nil)) == -6

    @test
    def sumWith05(): Bool =
        Nel.sumWith(x -> x + 1, Nel(10, -10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nil)) == Nel(1, Nil)

    @test
    def flatten02(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nil)) == Nel(1, 2 :: Nil)

    @test
    def flatten03(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nel(3, Nil) :: Nil)) == Nel(1, 2 :: 3 :: Nil)

    @test
    def flatten04(): Bool = Nel.flatten(Nel(Nel(1, Nil), Nel(2, 3 :: Nil) :: Nel(4, 5 :: Nil) :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def flatten05(): Bool = Nel.flatten(Nel(Nel(1, 2 :: 3 :: Nil), Nel(4, 5 :: Nil) :: Nel(6, 7 :: Nil) :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = Nel.exists(x -> x == 1, Nel(1, Nil)) == true

    @test
    def exists02(): Bool = Nel.exists(x -> x == 2, Nel(1, Nil)) == false

    @test
    def exists03(): Bool = Nel.exists(x -> x == 1, Nel(1, 2 :: Nil)) == true

    @test
    def exists04(): Bool = Nel.exists(x -> x == 2, Nel(1, 2 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool = Nel.forAll(x -> x == 1, Nel(1, Nil)) == true

    @test
    def forAll02(): Bool = Nel.forAll(x -> x == 2, Nel(1, Nil)) == false

    @test
    def forAll03(): Bool = Nel.forAll(x -> x == 1, Nel(1, 1 :: Nil)) == true

    @test
    def forAll04(): Bool = Nel.forAll(x -> x == 1, Nel(1, 2 :: Nil)) == false

    @test
    def forAll05(): Bool = Nel.forAll(x -> x == 1, Nel(2, 1 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = Nel.filter(x -> x > 0, Nel(1, Nil)) == 1 :: Nil

    @test
    def filter02(): Bool = Nel.filter(x -> x > 1, Nel(1, Nil)) == Nil

    @test
    def filter03(): Bool = Nel.filter(x -> x > 0, Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @test
    def filter04(): Bool = Nel.filter(x -> x > 1, Nel(1, 2 :: Nil)) == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = Nel.zip(Nel(1, Nil), Nel(1, Nil)) == Nel((1, 1), Nil)

    @test
    def zip02(): Bool = Nel.zip(Nel(1, 2 :: Nil), Nel(1, Nil)) == Nel((1, 1), Nil)

    @test
    def zip03(): Bool = Nel.zip(Nel(1, Nil), Nel(1, 2 :: Nil)) == Nel((1, 1), Nil)

    @test
    def zip04(): Bool = Nel.zip(Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Nel((1, 1), (2, 2) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, Nil)) == Nel(2, Nil)

    @test
    def zipWith02(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, Nil)) == Nel(2, Nil)

    @test
    def zipWith03(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, 2 :: Nil)) == Nel(2, Nil)

    @test
    def zipWith04(): Bool = Nel.zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Nel(2, 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = Nel.unzip(Nel((1, 1), Nil)) == (Nel(1, Nil), Nel(1, Nil))

    @test
    def unzip02(): Bool = Nel.unzip(Nel((1, 1), (2, 2) :: Nil)) == (Nel(1, 2 :: Nil), Nel(1, 2 :: Nil))

    @test
    def unzip03(): Bool = Nel.unzip(Nel((1, 1), (2, 2) :: (3, 3) :: Nil)) == (Nel(1, 2 :: 3 :: Nil), Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIndex01(): Bool = Nel.zipWithIndex(Nel(1, Nil)) == Nel((0, 1), Nil)

    @test
    def zipWithIndex02(): Bool = Nel.zipWithIndex(Nel(1, 2 :: Nil)) == Nel((0, 1), (1, 2) :: Nil)

    @test
    def zipWithIndex03(): Bool = Nel.zipWithIndex(Nel(2, 1 :: Nil)) == Nel((0, 2), (1, 1) :: Nil)

    @test
    def zipWithIndex04(): Bool = Nel.zipWithIndex(Nel(1, 2 :: 3 :: Nil)) == Nel((0, 1), (1, 2) :: (2, 3) :: Nil)

    @test
    def zipWithIndex05(): Bool =
        Nel.zipWithIndex(Nel(1, 2 :: 3 :: 4 :: 5 :: Nil)) == Nel((0, 1), (1, 2) :: (2, 3) :: (3, 4) :: (4, 5) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithA01(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, Nil), Nel(1, Nil)) == Identity.Identity(Nel(2, Nil))

    @test
    def zipWithA02(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, 2 :: Nil), Nel(1, Nil)) == Identity.Identity(Nel(2, Nil))

    @test
    def zipWithA03(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, Nil), Nel(1, 2 :: Nil)) == Identity.Identity(Nel(2, Nil))

    @test
    def zipWithA04(): Bool = Nel.zipWithA((x, y) -> Identity.Identity(x + y), Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Identity.Identity(Nel(2, 4 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = Nel.toList(Nel(1, Nil)) == 1 :: Nil

    @test
    def toList02(): Bool = Nel.toList(Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @test
    def toList03(): Bool = Nel.toList(Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
        let a = Nel.toArray(rc, Nel.singleton(1));
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def toArray02(): Bool = region rc {
        let a = Nel.toArray(rc, Nel(1, List#{2}));
        Array.sameElements(a, Array#{1, 2} @ rc)
    }

    @test
    def toArray03(): Bool = region rc {
        let a = Nel.toArray(rc, Nel(1, List#{2, 3}));
        Array.sameElements(a, Array#{1, 2, 3} @ rc)
    }

    @test
    def toArray04(): Bool = region rc {
        let a = Nel.toArray(rc, Nel(3, List#{2, 3}));
        Array.sameElements(a, Array#{3, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool =
        let l = Nel(1, List#{});
        Nel.toVector(l) == Vector#{1}

    @test
    def toVector02(): Bool =
        let l = Nel(1, List#{2});
        Nel.toVector(l) == Vector#{1, 2}

    @test
    def toVector03(): Bool =
        let l = Nel(1, List#{2, 3});
        Nel.toVector(l) == Vector#{1, 2, 3}

    @test
    def toVector04(): Bool =
        let l = Nel(3, List#{2, 3});
        Nel.toVector(l) == Vector#{3, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let l = Nel(1, 2 :: Nil);
        let d1 = Nel.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);
        MutDeque.pushBack(2, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let l = Nel(3, 1 :: 2 :: Nil);
        let d1 = Nel.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);
        MutDeque.pushBack(2, d2);
        MutDeque.pushFront(3, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let l = Nel('a', 'b' :: 'c' :: Nil);
        let d1 = Nel.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront('c', d2);
        MutDeque.pushFront('b', d2);
        MutDeque.pushFront('a', d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEach(x -> Ref.put(x, ri), Nel.singleton(1));
        1 == deref ri
    }

    @test
    def forEach02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEach(x -> Ref.put(x, ri), Nel(1, 2 :: 3 :: Nil));
        3 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEachWithIndex((i, _) -> Ref.put(i, ri), Nel.singleton(1));
        0 == deref ri
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nel.forEachWithIndex((i, _) -> Ref.put(i, ri), Nel(1, List#{2, 3}));
        2 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash(Nel(1, Nil)) == hash(Nel(1, Nil))

    @test
    def hash02(): Bool = hash(Nel(1, 2 :: 3 :: Nil)) == hash(Nel(1, 2 :: 3 :: Nil))

    @test
    def hash03(): Bool = hash(Nel(1, 3 :: 2 :: Nil)) != hash(Nel(1, 2 :: 3 :: Nil))

    @test
    def hash04(): Bool = hash(Nel(1, Nil)) != hash(Nel(2, Nil))

    @test
    def hash05(): Bool = hash(Nel("c", "b" :: "a" :: Nil)) != hash(Nel("c", "a" :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(1, Nil)) == 1

    @test
    def minimumBy02(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(1, 2 :: 3 :: Nil)) == 1

    @test
    def minimumBy03(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(1, 2 :: 0 :: 3 :: Nil)) == 0

    @test
    def minimumBy04(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 3

    @test
    def minimumBy05(): Bool = Nel.minimumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(1, Nil)) == 1

    @test
    def maximumBy02(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(1, 2 :: 3 :: Nil)) == 3

    @test
    def maximumBy03(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(1, 2 :: 0 :: 3 :: Nil)) == 3

    @test
    def maximumBy04(): Bool = Nel.maximumBy((x, y) -> x <=> y, Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 9

    @test
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

    @test
    def sortWith02(): Bool =
        Nel.sortWith(cmp, Nel(0, Nil)) == Nel(0, Nil)

    @test
    def sortWith03(): Bool =
        Nel.sortWith(cmp, Nel(0, 1 :: Nil)) == Nel(0, 1 :: Nil)

    @test
    def sortWith04(): Bool =
        Nel.sortWith(cmp, Nel(1, 0 :: Nil)) == Nel(0, 1 :: Nil)

    @test
    def sortWith05(): Bool =
        Nel.sortWith(cmp, Nel(1, 1 :: Nil)) == Nel(1, 1 :: Nil)

    @test
    def sortWith06(): Bool =
        Nel.sortWith(cmp, Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith07(): Bool =
        Nel.sortWith(cmp, Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith08(): Bool =
        Nel.sortWith(cmp, Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith09(): Bool =
        Nel.sortWith(cmp, Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(0, 1 :: 2 :: 2 :: 3 :: 4 :: Nil)

    @test
    def sortWith10(): Bool =
        Nel.sortWith(flip(cmp), Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith11(): Bool =
        Nel.sortWith(flip(cmp), Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith12(): Bool =
        Nel.sortWith(flip(cmp), Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith13(): Bool =
        Nel.sortWith(flip(cmp), Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == Nel(4, 3 :: 2 :: 2 :: 1 :: 0 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(xs: Nel[Int32]) : Bool =
        Nel.sort(xs) == Nel.sortWith(cmp, xs)

    @test
    def sort02(): Bool = testSortVsSortWith(Nel(0, Nil))

    @test
    def sort03(): Bool = testSortVsSortWith(Nel(0, 1 :: Nil))

    @test
    def sort04(): Bool = testSortVsSortWith(Nel(1, 0 :: Nil))

    @test
    def sort05(): Bool = testSortVsSortWith(Nel(1, 1 :: Nil))

    @test
    def sort06(): Bool = testSortVsSortWith(Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @test
    def sort07(): Bool = testSortVsSortWith(Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def sort08(): Bool = testSortVsSortWith(Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @test
    def sort09(): Bool = testSortVsSortWith(Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Nel[Int32]) : Bool =
        (Nel.sortBy(identity, a) == Nel.sort(a)) and
        (Nel.sortBy(x -> 4 * x + 7, a) == Nel.sort(a)) and
        (Nel.sortBy(x -> -x, a) == Nel.sortWith(flip(cmp),a))

    @test
    def sortBy02(): Bool = testSortByVsSort(Nel(0, Nil))

    @test
    def sortBy03(): Bool = testSortByVsSort(Nel(0, 1 :: Nil))

    @test
    def sortBy04(): Bool = testSortByVsSort(Nel(1, 0 :: Nil))

    @test
    def sortBy05(): Bool = testSortByVsSort(Nel(1, 1 :: Nil))

    @test
    def sortBy06(): Bool = testSortByVsSort(Nel(0, 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @test
    def sortBy07(): Bool = testSortByVsSort(Nel(5, 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def sortBy08(): Bool = testSortByVsSort(Nel(5, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @test
    def sortBy09(): Bool = testSortByVsSort(Nel(2, 3 :: 0 :: 4 :: 1 :: 2 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        Nel(1, 2 :: 3 :: Nil) |> Nel.iterator(rc) |> Iterator.toNel == Some(Nel(1, 2 :: 3 :: Nil))
    }

    @test
    def iterator02(): Bool = region rc {
        Nel(-100, List.range(-99, 100)) |> Nel.iterator(rc) |> Iterator.toList == List.range(-100, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def enumerator01(): Bool = region rc {
        Nel(1, 2 :: 3 :: Nil) |> Nel.enumerator(rc) |> Iterator.toList == (0, 1) :: (1, 2) :: (2, 3) :: Nil
    }

    @test
    def enumerator02(): Bool = region rc {
        Nel(-100, List.range(-99, 100)) |> Nel.enumerator(rc) |> Iterator.toList == List.mapWithIndex((ix, i) -> (ix, i), (List.range(-100, 100)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool =
        let l = Nel.singleton(Identity.Identity(1));
        Nel.sequence(l) == Identity.Identity(Nel.singleton(1))

    @test
    def sequence02(): Bool =
        let l = Nel.cons(Identity.Identity(1), Nel.singleton(Identity.Identity(2)));
        Nel.sequence(l) == Identity.Identity(Nel.cons(1, Nel.singleton(2)))

    @test
    def sequence03(): Bool =
        let l = Nel.cons(Identity.Identity(1), Nel.cons(Identity.Identity(2), Nel.singleton(Identity.Identity(3))));
        Nel.sequence(l) == Identity.Identity(Nel.cons(1, Nel.cons(2, Nel.singleton(3))))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nel.singleton(1);
        let ans = Nel.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(Nel.singleton(1)) and deref st == 1
    }

    @test
    def traverse02(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nel.cons(1, Nel.singleton(2));
        let ans = Nel.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(Nel.cons(1, Nel.singleton(2))) and deref st == 2
    }

    @test
    def traverse03(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nel.cons(1, Nel.cons(2, Nel.singleton(3)));
        let ans = Nel.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(Nel.cons(1, Nel.cons(2, Nel.singleton(3)))) and deref st == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMapWith01(): Bool =
        Nel.toMapWith(x -> 0 * x, Nel(7, 1 :: 3 :: 6 :: Nil)) == Map#{1 => 0, 3 => 0, 6 => 0, 7 => 0}

    @test
    def toMapWith02(): Bool =
        Nel.toMapWith(x -> 2 * x, Nel(7, 1 :: 3 :: 6 :: Nil)) == Map#{1 => 2, 3 => 6, 6 => 12, 7 => 14}

    @test
    def toMapWith03(): Bool =
        Nel.toMapWith(x -> x + 10, Nel(7, 1 :: 3 :: 6 :: Nil)) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

    @test
    def toMapWith04(): Bool =
        Nel.toMapWith(x -> x + 10, Nel(7, 3 :: 6 :: 1 :: Nil)) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Nel.join(",", Nel(1, Nil)) == "1"

    @test
    def join02(): Bool =
        Nel.join(",", Nel(1, 2 :: 3 :: Nil)) == "1,2,3"

    @test
    def join03(): Bool =
        Nel.join(",", Nel("1", "2" :: "3" :: Nil)) == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        Nel.joinWith(x -> "${x + 1}", ",", Nel(1, Nil)) == "2"

    @test
    def joinWith02(): Bool =
        Nel.joinWith(x -> "${x + 1}", ",", Nel(1, 2 :: 3 :: Nil)) == "2,3,4"

    @test
    def joinWith03(): Bool =
        Nel.joinWith(x -> x + x, ",", Nel("1", "2" :: "3" :: Nil)) == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ IO =
        let l = Nel.singleton(1);
        match Nel.shuffle(Random.fresh(), l) {
            case Some(ll) => Nel.length(ll) == 1 and (ll |> Nel.toList |> List.toSet) == Set#{1}
            case _        => false
        }

    @test
    def shuffle02(): Bool \ IO =
        let l = Nel.cons(1, Nel.cons(2, Nel.singleton(3)));
        match Nel.shuffle(Random.fresh(), l) {
            case Some(ll) => Nel.length(ll) == 3 and (ll |> Nel.toList |> List.toSet) == Set#{1, 2, 3}
            case _        => false
        }

    @test
    def shuffle03(): Bool \ IO =
        let l = Nel.cons(1, Nel.cons(2, Nel.cons(3, Nel.cons(4, Nel.cons(5, Nel.cons(6, Nel.cons(7, Nel.singleton(8))))))));
        match Nel.shuffle(Random.fresh(), l) {
            case Some(ll) => Nel.length(ll) == 8 and
                                (ll |> Nel.toList |> List.toSet) == Set#{1, 2, 3, 4, 5, 6, 7, 8}
            case _        => false
        }

}
