namespace Nel {

    use Hash.hash;

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool = singleton(1) == Nel(1, Nil)

    @test
    def singleton02(): Bool = singleton(2) == Nel(2, Nil)

    @test
    def singleton03(): Bool = singleton('a') == Nel('a', Nil)

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def cons01(): Bool = cons(1, singleton(2)) == Nel(1, 2 :: Nil)

    @test
    def cons02(): Bool = cons(1, cons(2, singleton(3))) == Nel(1, 2 :: 3 :: Nil)

    @test
    def cons03(): Bool = cons(1, cons(2, cons(3, singleton(4)))) == Nel(1, 2 :: 3 :: 4 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = head(Nel(123, Nil)) == 123

    @test
    def head02(): Bool = head(Nel(123, 1 :: Nil)) == 123

    @test
    def head03(): Bool = head(Nel(123, 1 :: 2 :: Nil)) == 123

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = last(Nel(123, Nil)) == 123

    @test
    def last02(): Bool = last(Nel(123, 1 :: Nil)) == 1

    @test
    def last03(): Bool = last(Nel(123, 1 :: 2 :: Nil)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = init(Nel(1, Nil)) == Nil

    @test
    def init02(): Bool = init(Nel(1, 2 :: Nil)) == 1 :: Nil

    @test
    def init03(): Bool = init(Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool = tail(Nel(1, Nil)) == Nil

    @test
    def tail02(): Bool = tail(Nel(1, 2 :: Nil)) == 2 :: Nil

    @test
    def tail03(): Bool = tail(Nel(1, 2 :: 3 :: Nil)) == 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = length(Nel(1, Nil)) == 1

    @test
    def length02(): Bool = length(Nel(1, 2 :: Nil)) == 2

    @test
    def length03(): Bool = length(Nel(1, 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = Nel(1, Nil) `append` Nel(2, Nil) == Nel(1, 2 :: Nil)

    @test
    def append02(): Bool = Nel(1, 2 :: Nil) `append` Nel(3, Nil) == Nel(1, 2 :: 3 :: Nil)

    @test
    def append03(): Bool = Nel(1, Nil) `append` Nel(2, 3 :: Nil) == Nel(1, 2 :: 3 :: Nil)

    @test
    def append04(): Bool = Nel(1, 2 :: Nil) `append` Nel(3, 4 :: Nil) == Nel(1, 2 :: 3 :: 4 :: Nil)

    @test
    def append05(): Bool = Nel(1, 2 :: 3 :: Nil) `append` Nel(4, 5 :: 6 :: Nil) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = memberOf(0, Nel(1, Nil)) == false

    @test
    def memberOf02(): Bool = memberOf(0, Nel(1, 2 :: Nil)) == false

    @test
    def memberOf03(): Bool = memberOf(0, Nel(1, 2 :: 3 :: Nil)) == false

    @test
    def memberOf04(): Bool = memberOf(1, Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf05(): Bool = memberOf(2, Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf06(): Bool = memberOf(3, Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = findLeft(x -> x == 0, Nel(1, Nil)) == None

    @test
    def findLeft02(): Bool = findLeft(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @test
    def findLeft03(): Bool = findLeft(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findLeft04(): Bool = findLeft(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findLeft05(): Bool = findLeft(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findLeft06(): Bool = findLeft(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = findRight(x -> x == 0, Nel(1, Nil)) == None

    @test
    def findRight02(): Bool = findRight(x -> x == 0, Nel(1, 2 :: Nil)) == None

    @test
    def findRight03(): Bool = findRight(x -> x == 0, Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findRight04(): Bool = findRight(x -> x == 1, Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findRight05(): Bool = findRight(x -> x == 2, Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findRight06(): Bool = findRight(x -> x == 3, Nel(1, 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = map(x -> x + 1, Nel(1, Nil)) == Nel(2, Nil)

    @test
    def map02(): Bool = map(x -> x + 1, Nel(1, 2 :: Nil)) == Nel(2, 3 :: Nil)

    @test
    def map03(): Bool = map(x -> x + 1, Nel(1, 2 :: 3 :: Nil)) == Nel(2, 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = mapWithIndex((_, i) -> i, Nel(21, Nil)) == Nel(0, Nil)

    @test
    def mapWithIndex02(): Bool = mapWithIndex((_, i) -> i, Nel(21, 42 :: Nil)) == Nel(0, 1 :: Nil)

    @test
    def mapWithIndex03(): Bool = mapWithIndex((_, i) -> i, Nel(21, 42 :: 84 :: Nil)) == Nel(0, 1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = flatMap(x -> Nel(x + 1, Nil), Nel(1, Nil)) == Nel(2, Nil)

    @test
    def flatMap02(): Bool = flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: Nil)) == Nel(2, 3 :: Nil)

    @test
    def flatMap03(): Bool = flatMap(x -> Nel(x + 1, Nil), Nel(1, 2 :: 3 :: Nil)) == Nel(2, 3 :: 4 :: Nil)

    @test
    def flatMap04(): Bool = flatMap(x -> Nel(x, x :: Nil), Nel(1, Nil)) == Nel(1, 1 :: Nil)

    @test
    def flatMap05(): Bool = flatMap(x -> Nel(x, x :: Nil), Nel(1, 2 :: Nil)) == Nel(1, 1 :: 2 :: 2 :: Nil)

    @test
    def flatMap06(): Bool = flatMap(x -> Nel(x, x :: Nil), Nel(1, 2 :: 3 :: Nil)) == Nel(1, 1 :: 2 :: 2 :: 3 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = reverse(Nel(1, Nil)) == Nel(1, Nil)

    @test
    def reverse02(): Bool = reverse(Nel(1, 2 :: Nil)) == Nel(2, 1 :: Nil)

    @test
    def reverse03(): Bool = reverse(Nel(1, 2 :: 3 :: Nil)) == Nel(3, 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = replace(1, 42, Nel(1, Nil)) == Nel(42, Nil)

    @test
    def replace02(): Bool = replace(7, 42, Nel(1, Nil)) == Nel(1, Nil)

    @test
    def replace03(): Bool = replace(2, 42, Nel(1, 2 :: Nil)) == Nel(1, 42 :: Nil)

    @test
    def replace04(): Bool = replace(7, 42, Nel(1, 2 :: Nil)) == Nel(1, 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // permutations                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def permutations01(): Bool = permutations(Nel(1, Nil)) == Nel(1 :: Nil, Nil)

    @test
    def permutations02(): Bool = permutations(Nel(1, 2 :: Nil)) == Nel(1 :: 2 :: Nil, (2 :: 1 :: Nil) :: Nil)

    @test
    def permutations03(): Bool = permutations(Nel(1, 2 :: 3 :: Nil)) == Nel(1 :: 2 :: 3 :: Nil,
                                                                           (1 :: 3 :: 2 :: Nil) ::
                                                                           (2 :: 1 :: 3 :: Nil) ::
                                                                           (2 :: 3 :: 1 :: Nil) ::
                                                                           (3 :: 1 :: 2 :: Nil) ::
                                                                           (3 :: 2 :: 1 :: Nil) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // subsequences                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def subsequences01(): Bool = subsequences(Nel(1, Nil)) == Nel(1 :: Nil, (Nil) :: Nil)

    @test
    def subsequences02(): Bool = subsequences(Nel(1, 2 :: Nil)) == Nel(1 :: 2 :: Nil, (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil)

    @test
    def subsequences03(): Bool = subsequences(Nel(1, 2 :: 3 :: Nil)) == Nel(1 :: 2 :: 3 :: Nil,
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
    def intersperse01(): Bool = intersperse(7, Nel(1, Nil)) == Nel(1, Nil)

    @test
    def intersperse02(): Bool = intersperse(7, Nel(1, 2 :: Nil)) == Nel(1, 7 :: 2 :: Nil)

    @test
    def intersperse03(): Bool = intersperse(7, Nel(1, 2 :: 3 :: Nil)) == Nel(1, 7 :: 2 :: 7 :: 3 :: Nil)

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
    def foldLeft01(): Bool = foldLeft((acc, x) -> acc - x, 0, Nel(1, Nil)) == -1

    @test
    def foldLeft02(): Bool = foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: Nil)) == -3

    @test
    def foldLeft03(): Bool = foldLeft((acc, x) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = foldRight((x, acc) -> acc - x, 0, Nel(1, Nil)) == -1

    @test
    def foldRight02(): Bool = foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: Nil)) == -3

    @test
    def foldRight03(): Bool = foldRight((x, acc) -> acc - x, 0, Nel(1, 2 :: 3 :: Nil)) == -6

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
    def reduceLeft01(): Bool = reduceLeft((acc, x) -> acc - x, Nel(1, Nil)) == 1

    @test
    def reduceLeft02(): Bool = reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: Nil)) == -1

    @test
    def reduceLeft03(): Bool = reduceLeft((acc, x) -> acc - x, Nel(1, 2 :: 3 :: Nil)) == -4

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = reduceRight((x, acc) -> acc - x, Nel(1, Nil)) == 1

    @test
    def reduceRight02(): Bool = reduceRight((x, acc) -> acc - x, Nel(1, 2 :: Nil)) == 1

    @test
    def reduceRight03(): Bool = reduceRight((x, acc) -> acc - x, Nel(1, 2 :: 3 :: Nil)) == 0

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = count(x -> x == 1, Nel(1, Nil)) == 1

    @test
    def count02(): Bool = count(x -> x == 2, Nel(1, Nil)) == 0

    @test
    def count03(): Bool = count(x -> x == 2, Nel(1, 2 :: 1 :: 2 :: Nil)) == 2

    @test
    def count04(): Bool = count(x -> x == 2, Nel(2, 2 :: 1 :: 2 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = flatten(Nel(Nel(1, Nil), Nil)) == Nel(1, Nil)

    @test
    def flatten02(): Bool = flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nil)) == Nel(1, 2 :: Nil)

    @test
    def flatten03(): Bool = flatten(Nel(Nel(1, Nil), Nel(2, Nil) :: Nel(3, Nil) :: Nil)) == Nel(1, 2 :: 3 :: Nil)

    @test
    def flatten04(): Bool = flatten(Nel(Nel(1, Nil), Nel(2, 3 :: Nil) :: Nel(4, 5 :: Nil) :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def flatten05(): Bool = flatten(Nel(Nel(1, 2 :: 3 :: Nil), Nel(4, 5 :: Nil) :: Nel(6, 7 :: Nil) :: Nil)) == Nel(1, 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = exists(x -> x == 1, Nel(1, Nil)) == true

    @test
    def exists02(): Bool = exists(x -> x == 2, Nel(1, Nil)) == false

    @test
    def exists03(): Bool = exists(x -> x == 1, Nel(1, 2 :: Nil)) == true

    @test
    def exists04(): Bool = exists(x -> x == 2, Nel(1, 2 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = forall(x -> x == 1, Nel(1, Nil)) == true

    @test
    def forall02(): Bool = forall(x -> x == 2, Nel(1, Nil)) == false

    @test
    def forall03(): Bool = forall(x -> x == 1, Nel(1, 1 :: Nil)) == true

    @test
    def forall04(): Bool = forall(x -> x == 1, Nel(1, 2 :: Nil)) == false

    @test
    def forall05(): Bool = forall(x -> x == 1, Nel(2, 1 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = filter(x -> x > 0, Nel(1, Nil)) == 1 :: Nil

    @test
    def filter02(): Bool = filter(x -> x > 1, Nel(1, Nil)) == Nil

    @test
    def filter03(): Bool = filter(x -> x > 0, Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @test
    def filter04(): Bool = filter(x -> x > 1, Nel(1, 2 :: Nil)) == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = zip(Nel(1, Nil), Nel(1, Nil)) == Nel((1, 1), Nil)

    @test
    def zip02(): Bool = zip(Nel(1, 2 :: Nil), Nel(1, Nil)) == Nel((1, 1), Nil)

    @test
    def zip03(): Bool = zip(Nel(1, Nil), Nel(1, 2 :: Nil)) == Nel((1, 1), Nil)

    @test
    def zip04(): Bool = zip(Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Nel((1, 1), (2, 2) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, Nil)) == Nel(2, Nil)

    @test
    def zipWith02(): Bool = zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, Nil)) == Nel(2, Nil)

    @test
    def zipWith03(): Bool = zipWith((x, y) -> x + y, Nel(1, Nil), Nel(1, 2 :: Nil)) == Nel(2, Nil)

    @test
    def zipWith04(): Bool = zipWith((x, y) -> x + y, Nel(1, 2 :: Nil), Nel(1, 2 :: Nil)) == Nel(2, 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = unzip(Nel((1, 1), Nil)) == (Nel(1, Nil), Nel(1, Nil))

    @test
    def unzip02(): Bool = unzip(Nel((1, 1), (2, 2) :: Nil)) == (Nel(1, 2 :: Nil), Nel(1, 2 :: Nil))

    @test
    def unzip03(): Bool = unzip(Nel((1, 1), (2, 2) :: (3, 3) :: Nil)) == (Nel(1, 2 :: 3 :: Nil), Nel(1, 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = toList(Nel(1, Nil)) == 1 :: Nil

    @test
    def toList02(): Bool = toList(Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @test
    def toList03(): Bool = toList(Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil

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
    if (x < y) LessThan
    else if (x == y) EqualTo
    else GreaterThan

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
    (Nel.sortBy(x -> 4*x+7, a) == Nel.sort(a)) and
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

}
