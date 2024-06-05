use Monad.{>>=}
// Definitions for List.*
pub def runAllTests(): Unit \ IO =
    testAp();
    testAppend();
    testCount();
    testDistinct();
    testDestinctWith();
    testDrop();
    testDropWhile();
    testEmpty();
    testExists();
    testFilter();
    testFilterMap();
    testFind();
    testFindLeft();
    testFindMap();
    testFindRight();
    testFlatMap();
    testFlatten();
    testFold();
    testFold2();
    testFoldLeft();
    testFoldLeft2();
    testFoldMap();
    testFoldRight();
    testFoldRight2();

    testReverse();
    testTranspose()

// ap
pub def testAp(): Unit \ IO =
    // Adding two lists element-wise in two different ways should give the same result
    def propAp(xs: List[Int32], ys: List[Int32]) =      List.ap(List.map(x -> y -> x + y)(xs), ys) == List.flatMap(x -> List.map(y -> x + y)(ys))(xs);
    def propAp2(xs: List[Float64], ys: List[Float64]) = List.ap(List.map(x -> y -> x + y)(xs), ys) == List.flatMap(x -> List.map(y -> x + y)(ys))(xs);
    // Property for Bool is slightly different, as we can't add booleans
    def propAp3(xs: List[Bool], ys: List[Bool]) =       List.ap(List.map(x -> y -> x and y)(xs), ys) == List.flatMap(x -> List.map(y -> x and y)(ys))(xs);
    def propAp4(xs: List[String], ys: List[String]) =   List.ap(List.map(x -> y -> x + y)(xs), ys) == List.flatMap(x -> List.map(y -> x + y)(ys))(xs);
    // Property for Char is slightly different, as we can't add chars
    def propAp5(xs: List[Char], ys: List[Char]) =       List.ap(List.map(x -> y -> x == y)(xs), ys) == List.flatMap(x -> List.map(y -> x == y)(ys))(xs);

    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp2) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp3) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp4) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAp5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// append
pub def testAppend(): Unit \ IO =
    // Testing the ++ operator
    def propAppendInt1(xs: List[Int32], ys: List[Int32]) =      List.append(xs, ys) == xs ++ ys;
    // Testing the infix operator
    def propAppendInt2(xs: List[Int32], ys: List[Int32]) =      List.append(xs, ys) == xs ::: ys;
    // Appending should give the same result as folding over the list with the append function
    def propAppendInt3(xs: List[Int32], ys: List[Int32]) =      List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    // testing with different types
    def propAppendChar(xs: List[Char], ys: List[Char]) =        List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    def propAppendString(xs: List[String], ys: List[String]) =  List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    def propAppendBool(xs: List[Bool], ys: List[Bool]) =        List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);
    def propAppendFloat(xs: List[Float64], ys: List[Float64]) = List.append(xs, ys) == List.foldLeft((acc, x) -> List.append(acc, List.point(x)), List.empty(), xs ++ ys);

    // Slight inconvenience : we can only combine TFuncs of the same type.
    let propList : List[TFunc2[List[Int32], List[Int32], Bool]]= TFunc2.TFunc2(propAppendInt1) :: TFunc2.TFunc2(propAppendInt2) :: TFunc2.TFunc2(propAppendInt3) :: Nil;

    let otherProps = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppendChar) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppendString) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppendBool) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propAppendFloat) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    };
    foreach (oProp <- otherProps) {
        flixCheck(oProp)
    }
    
// count
pub def testCount(): Unit \ IO =
    // Property that checks if the count of all elements in a list is the same as the length of the list
    def propCount(xs: List[Bool]) =     List.count(x -> x == x, xs) == List.length(xs);
    def propCount2(xs: List[Char]) =    List.count(x -> x == x, xs) == List.length(xs);
    def propCount3(xs: List[Float64]) = List.count(x -> x == x, xs) == List.length(xs);
    def propCount4(xs: List[Int32]) =   List.count(x -> x == x, xs) == List.length(xs);
    def propCount5(xs: List[String]) =  List.count(x -> x == x, xs) == List.length(xs);

    let propList: List[Property] = forAll(Arbitrary.arbitrary(), propCount) :: 
                    forAll(Arbitrary.arbitrary(), propCount2) :: 
                    forAll(Arbitrary.arbitrary(), propCount3) :: 
                    forAll(Arbitrary.arbitrary(), propCount4) :: 
                    forAll(Arbitrary.arbitrary(), propCount5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// distinct
pub def testDistinct(): Unit \ IO =
    // Property that checks if the distinct of a list is the same as the list with duplicates removed
    def propDistinct(xs: List[Bool]) =      List.distinct(xs) == List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinct2(xs: List[Char]) =     List.distinct(xs) == List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinct3(xs: List[Float64]) =  List.distinct(xs) == List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinct4(xs: List[Int32]) =    List.distinct(xs) == List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinct5(xs: List[String]) =   List.distinct(xs) == List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    // property that checks if the distinct of a list is the same as the the distinct of a list twice
    def propDistinct6(xs: List[Bool]) =     List.distinct(xs) == List.distinct(xs) |> List.distinct;
    def propDistinct7(xs: List[Char]) =     List.distinct(xs) == List.distinct(xs) |> List.distinct;
    def propDistinct8(xs: List[Float64]) =  List.distinct(xs) == List.distinct(xs) |> List.distinct;
    def propDistinct9(xs: List[Int32]) =    List.distinct(xs) == List.distinct(xs) |> List.distinct;
    def propDistinct10(xs: List[String]) =  List.distinct(xs) == List.distinct(xs) |> List.distinct;

    let propList = forAll(Arbitrary.arbitrary(), propDistinct) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct2) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct3) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct4) :: 
                    forAll(Arbitrary.arbitrary(), propDistinct5) ::
                    forAll(Arbitrary.arbitrary(), propDistinct6) ::
                    forAll(Arbitrary.arbitrary(), propDistinct7) ::
                    forAll(Arbitrary.arbitrary(), propDistinct8) ::
                    forAll(Arbitrary.arbitrary(), propDistinct9) ::
                    forAll(Arbitrary.arbitrary(), propDistinct10) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// distinctWith
pub def testDestinctWith():Unit \ IO =
    // distinctWith Returns the list l with duplicates removed using the supplied function f for comparison. 
    // The first occurrence of an element is kept and except for the removal of subsequent duplicates the order of l is preserved.
    // Property that checks if the distinct of a list is the same as the list with duplicates removed
    def propDistinctWith(xs: List[Int32]) =     List.distinctWith((x, y) -> x == y, xs) ==  List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinctWith2(xs: List[Char]) =     List.distinctWith((x, y) -> x == y, xs) ==  List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinctWith3(xs: List[Float64]) =  List.distinctWith((x, y) -> x == y, xs) ==  List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinctWith4(xs: List[String]) =   List.distinctWith((x, y) -> x == y, xs) ==  List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;
    def propDistinctWith5(xs: List[Bool]) =     List.distinctWith((x, y) -> x == y, xs) ==  List.foldLeft((acc, x) -> if (List.memberOf(x, acc)) acc else x :: acc, List.empty(), xs) |> List.reverse;

    let propList = forAll(Arbitrary.arbitrary(), propDistinctWith) :: 
                    forAll(Arbitrary.arbitrary(), propDistinctWith2) :: 
                    forAll(Arbitrary.arbitrary(), propDistinctWith3) :: 
                    forAll(Arbitrary.arbitrary(), propDistinctWith4) :: 
                    forAll(Arbitrary.arbitrary(), propDistinctWith5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// drop
pub def testDrop(): Unit \ IO =
    // dropping n elements should be the same as taking the reverse of the list and taking the first n elements
    def propDrop(n : Int32, xs:  List[Int32]) =     List.drop(n, xs) == List.reverse(xs) |> List.take(List.length(xs)-n) |> List.reverse;
    def propDrop2(n : Int32, xs:  List[Char]) =     List.drop(n, xs) == List.reverse(xs) |> List.take(List.length(xs)-n) |> List.reverse;
    def propDrop3(n : Int32, xs:  List[Float64]) =  List.drop(n, xs) == List.reverse(xs) |> List.take(List.length(xs)-n) |> List.reverse;
    def propDrop4(n : Int32, xs:  List[String]) =   List.drop(n, xs) == List.reverse(xs) |> List.take(List.length(xs)-n) |> List.reverse;
    def propDrop5(n : Int32, xs:  List[Bool]) =     List.drop(n, xs) == List.reverse(xs) |> List.take(List.length(xs)-n) |> List.reverse;

    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propDrop) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propDrop2) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propDrop3) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propDrop4) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propDrop5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// dropWhile
pub def testDropWhile(): Unit \ IO =
    // dropWhile Returns the longest prefix of l whose elements satisfy the predicate f.
    def dropWhileTester(xs: List[Int32]): List[Int32] =
        match xs {
            case Nil => Nil
            case h :: t => if (h < 5) dropWhileTester(t) else xs
    };
    def propDropWhile(xs: List[Int32]) =    List.dropWhile(x -> x < 5, xs) == dropWhileTester(xs);
    def propDropWhile2(xs: List[Int32]) =   List.length(List.dropWhile(x -> x < 5, xs)) <= List.length(xs);
    def propDropWhile3(xs: List[Char]) =    List.length(List.dropWhile(x -> x < 'e', xs)) <= List.length(xs);
    def propDropWhile4(xs: List[Float64]) = List.length(List.dropWhile(x -> x < 5.0, xs)) <= List.length(xs);
    def propDropWhile5(xs: List[String]) =  List.length(List.dropWhile(x -> x < "e", xs)) <= List.length(xs);
    def propDropWhile6(xs: List[Bool]) =    List.length(List.dropWhile(x -> x == true, xs)) <= List.length(xs);


    let propList = forAll(Arbitrary.arbitrary(), propDropWhile) :: 
                    forAll(Arbitrary.arbitrary(), propDropWhile2) :: 
                    forAll(Arbitrary.arbitrary(), propDropWhile3) :: 
                    forAll(Arbitrary.arbitrary(), propDropWhile4) :: 
                    forAll(Arbitrary.arbitrary(), propDropWhile5) :: 
                    forAll(Arbitrary.arbitrary(), propDropWhile6) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// empty
pub def testEmpty(): Unit \ IO =
    // Property that checks if the empty list is indeed empty
    def propEmpty() =   List.isEmpty(List.empty()) == true;
    def propEmpty2() =  List.length(List.empty()) == 0;

    let propList = propEmpty() :: propEmpty2() :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// enumerator
// TODO when effects are implemented
//
// Returns an iterator over l zipped with the indices of the elements.
//
// Implementation:
// pub def enumerator(rc: Region[r], l: List[a]): Iterator[(Int32, a), r, r] \ r =
//     iterator(rc, l) |> Iterator.zipWithIndex


   

// exists
pub def testExists(): Unit \ IO =
    // Property that checks if the exists function returns true if the predicate is true for at least one element
    def propExists(xs: List[Int32]) =       List.exists(x -> x == 0, xs) == List.foldLeft((acc, x) -> acc or x == 0, false, xs);
    def propExists2(xs: List[Char]) =       List.exists(x -> x == 'a', xs) == List.foldLeft((acc, x) -> acc or x == 'a', false, xs);
    def propExists3(xs: List[Float64]) =    List.exists(x -> x == 0.0, xs) == List.foldLeft((acc, x) -> acc or x == 0.0, false, xs);
    def propExists4(xs: List[String]) =     List.exists(x -> x == "", xs) == List.foldLeft((acc, x) -> acc or x == "", false, xs);
    def propExists5(xs: List[Bool]) =       List.exists(x -> x == true, xs) == List.foldLeft((acc, x) -> acc or x == true, false, xs);

    let propList = forAll(Arbitrary.arbitrary(), propExists) :: 
                    forAll(Arbitrary.arbitrary(), propExists2) :: 
                    forAll(Arbitrary.arbitrary(), propExists3) :: 
                    forAll(Arbitrary.arbitrary(), propExists4) :: 
                    forAll(Arbitrary.arbitrary(), propExists5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// filter
pub def testFilter(): Unit \ IO =
    // Property that checks if the filter function returns a list with only the elements that satisfy the predicate
    def propFilter(xs: List[Int32]) =    List.filter(x -> x == 0, xs) == List.foldLeft((acc, x) -> if (x == 0) x :: acc else acc, List.empty(), xs);
    def propFilter2(xs: List[Char]) =    List.filter(x -> x == 'a', xs) == List.foldLeft((acc, x) -> if (x == 'a') x :: acc else acc, List.empty(), xs);
    def propFilter3(xs: List[Float64]) = List.filter(x -> x == 0.0, xs) == List.foldLeft((acc, x) -> if (x == 0.0) x :: acc else acc, List.empty(), xs);
    def propFilter4(xs: List[String]) =  List.filter(x -> x == "", xs) == List.foldLeft((acc, x) -> if (x == "") x :: acc else acc, List.empty(), xs);
    def propFilter5(xs: List[Bool]) =    List.filter(x -> x == true, xs) == List.foldLeft((acc, x) -> if (x == true) x :: acc else acc, List.empty(), xs);

    def propFilter6(xs: List[Int32]) =   List.length(List.filter(x -> x == 0, xs)) <= List.length(xs);
    def propFilter7(xs: List[Char]) =    List.length(List.filter(x -> x == 'a', xs)) <= List.length(xs);
    def propFilter8(xs: List[Float64]) = List.length(List.filter(x -> x == 0.0, xs)) <= List.length(xs);
    def propFilter9(xs: List[String]) =  List.length(List.filter(x -> x == "", xs)) <= List.length(xs);
    def propFilter10(xs: List[Bool]) =   List.length(List.filter(x -> x == true, xs)) <= List.length(xs);

    let propList = forAll(Arbitrary.arbitrary(), propFilter) :: 
                    forAll(Arbitrary.arbitrary(), propFilter2) :: 
                    forAll(Arbitrary.arbitrary(), propFilter3) :: 
                    forAll(Arbitrary.arbitrary(), propFilter4) :: 
                    forAll(Arbitrary.arbitrary(), propFilter5) ::
                    forAll(Arbitrary.arbitrary(), propFilter6) ::
                    forAll(Arbitrary.arbitrary(), propFilter7) ::
                    forAll(Arbitrary.arbitrary(), propFilter8) ::
                    forAll(Arbitrary.arbitrary(), propFilter9) ::
                    forAll(Arbitrary.arbitrary(), propFilter10) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }
// filterMap
pub def testFilterMap(): Unit \ IO =
    // Property that checks if the filterMap function returns a list with only the elements that satisfy the predicate
    def propFilterMap(xs: List[Int32]) =    List.filterMap(x -> if (x == 0) Some(x) else None, xs) == List.foldLeft((acc, x) -> if (x == 0) x :: acc else acc, List.empty(), xs);
    def propFilterMap2(xs: List[Char]) =    List.filterMap(x -> if (x == 'a') Some(x) else None, xs) == List.foldLeft((acc, x) -> if (x == 'a') x :: acc else acc, List.empty(), xs);
    def propFilterMap3(xs: List[Float64]) = List.filterMap(x -> if (x == 0.0) Some(x) else None, xs) == List.foldLeft((acc, x) -> if (x == 0.0) x :: acc else acc, List.empty(), xs);
    def propFilterMap4(xs: List[String]) =  List.filterMap(x -> if (x == "") Some(x) else None, xs) == List.foldLeft((acc, x) -> if (x == "") x :: acc else acc, List.empty(), xs);
    def propFilterMap5(xs: List[Bool]) =    List.filterMap(x -> if (x == true) Some(x) else None, xs) == List.foldLeft((acc, x) -> if (x == true) x :: acc else acc, List.empty(), xs);

    def propFilterMap6(xs: List[Int32]) =   List.length(List.filterMap(x -> if (x == 0) Some(x) else None, xs)) <= List.length(xs);
    def propFilterMap7(xs: List[Char]) =    List.length(List.filterMap(x -> if (x == 'a') Some(x) else None, xs)) <= List.length(xs);
    def propFilterMap8(xs: List[Float64]) = List.length(List.filterMap(x -> if (x == 0.0) Some(x) else None, xs)) <= List.length(xs);
    def propFilterMap9(xs: List[String]) =  List.length(List.filterMap(x -> if (x == "") Some(x) else None, xs)) <= List.length(xs);
    def propFilterMap10(xs: List[Bool]) =   List.length(List.filterMap(x -> if (x == true) Some(x) else None, xs)) <= List.length(xs);

    let propList = forAll(Arbitrary.arbitrary(), propFilterMap) :: 
                    forAll(Arbitrary.arbitrary(), propFilterMap2) :: 
                    forAll(Arbitrary.arbitrary(), propFilterMap3) :: 
                    forAll(Arbitrary.arbitrary(), propFilterMap4) :: 
                    forAll(Arbitrary.arbitrary(), propFilterMap5) ::
                    forAll(Arbitrary.arbitrary(), propFilterMap6) ::
                    forAll(Arbitrary.arbitrary(), propFilterMap7) ::
                    forAll(Arbitrary.arbitrary(), propFilterMap8) ::
                    forAll(Arbitrary.arbitrary(), propFilterMap9) ::
                    forAll(Arbitrary.arbitrary(), propFilterMap10) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// find
// Alias for findLeft.
pub def testFind(): Unit \ IO =
    // Just check that find and findLeft are the same
    def propFind(xs: List[Int32]) =     List.find(x -> x == 0, xs) == List.findLeft(x -> x == 0, xs);
    def propFind2(xs: List[Char]) =     List.find(x -> x == 'a', xs) == List.findLeft(x -> x == 'a', xs);
    def propFind3(xs: List[Float64]) =  List.find(x -> x == 0.0, xs) == List.findLeft(x -> x == 0.0, xs);
    def propFind4(xs: List[String]) =   List.find(x -> x == "", xs) == List.findLeft(x -> x == "", xs);
    def propFind5(xs: List[Bool]) =     List.find(x -> x == true, xs) == List.findLeft(x -> x == true, xs);

    let propList = forAll(Arbitrary.arbitrary(), propFind) :: 
                    forAll(Arbitrary.arbitrary(), propFind2) :: 
                    forAll(Arbitrary.arbitrary(), propFind3) :: 
                    forAll(Arbitrary.arbitrary(), propFind4) :: 
                    forAll(Arbitrary.arbitrary(), propFind5) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// findLeft
pub def testFindLeft(): Unit \ IO =
    // Property that checks if the findLeft function returns the first element that satisfies the predicate
    def propFindLeft(xs: List[Int32]) =     List.findLeft(x -> x == 0, xs) == List.filter(x -> x == 0, xs) |> List.head;
    def propFindLeft2(xs: List[Char]) =     List.findLeft(x -> x == 'a', xs) == List.filter(x -> x == 'a', xs) |> List.head;
    def propFindLeft3(xs: List[Float64]) =  List.findLeft(x -> x == 0.0, xs) == List.filter(x -> x == 0.0, xs) |> List.head;
    def propFindLeft4(xs: List[String]) =   List.findLeft(x -> x == "", xs) == List.filter(x -> x == "", xs) |> List.head;
    def propFindLeft5(xs: List[Bool]) =     List.findLeft(x -> x == true, xs) == List.filter(x -> x == true, xs) |> List.head;

    let propList = forAll(Arbitrary.arbitrary(), propFindLeft) :: 
                    forAll(Arbitrary.arbitrary(), propFindLeft2) :: 
                    forAll(Arbitrary.arbitrary(), propFindLeft3) :: 
                    forAll(Arbitrary.arbitrary(), propFindLeft4) :: 
                    forAll(Arbitrary.arbitrary(), propFindLeft5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// findMap
pub def testFindMap(): Unit \ IO =
    // Property that checks if the findMap function returns the first element that satisfies the predicate
    def propFindMap(xs: List[Int32]) =      List.findMap(x -> if (x == 0) Some(x) else None, xs) == List.filterMap(x -> if (x == 0) Some(x) else None, xs) |> List.head;
    def propFindMap2(xs: List[Char]) =      List.findMap(x -> if (x == 'a') Some(x) else None, xs) == List.filterMap(x -> if (x == 'a') Some(x) else None, xs) |> List.head;
    def propFindMap3(xs: List[Float64]) =   List.findMap(x -> if (x == 0.0) Some(x) else None, xs) == List.filterMap(x -> if (x == 0.0) Some(x) else None, xs) |> List.head;
    def propFindMap4(xs: List[String]) =    List.findMap(x -> if (x == "") Some(x) else None, xs) == List.filterMap(x -> if (x == "") Some(x) else None, xs) |> List.head;
    def propFindMap5(xs: List[Bool]) =      List.findMap(x -> if (x == true) Some(x) else None, xs) == List.filterMap(x -> if (x == true) Some(x) else None, xs) |> List.head;

    let propList = forAll(Arbitrary.arbitrary(), propFindMap) :: 
                    forAll(Arbitrary.arbitrary(), propFindMap2) :: 
                    forAll(Arbitrary.arbitrary(), propFindMap3) :: 
                    forAll(Arbitrary.arbitrary(), propFindMap4) :: 
                    forAll(Arbitrary.arbitrary(), propFindMap5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }
// findRight
pub def testFindRight(): Unit \ IO =
    // Property that checks if the findRight function returns the last element that satisfies the predicate
    def propFindRight(xs: List[Int32]) =    List.findRight(x -> x == 0, xs) == List.filter(x -> x == 0, xs) |> List.last;
    def propFindRight2(xs: List[Char]) =    List.findRight(x -> x == 'a', xs) == List.filter(x -> x == 'a', xs) |> List.last;
    def propFindRight3(xs: List[Float64]) = List.findRight(x -> x == 0.0, xs) == List.filter(x -> x == 0.0, xs) |> List.last;
    def propFindRight4(xs: List[String]) =  List.findRight(x -> x == "", xs) == List.filter(x -> x == "", xs) |> List.last;
    def propFindRight5(xs: List[Bool]) =    List.findRight(x -> x == true, xs) == List.filter(x -> x == true, xs) |> List.last;

    let propList = forAll(Arbitrary.arbitrary(), propFindRight) :: 
                    forAll(Arbitrary.arbitrary(), propFindRight2) :: 
                    forAll(Arbitrary.arbitrary(), propFindRight3) :: 
                    forAll(Arbitrary.arbitrary(), propFindRight4) :: 
                    forAll(Arbitrary.arbitrary(), propFindRight5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }
// flatMap
pub def testFlatMap(): Unit \ IO =
    // Property that checks if the flatMap function returns the same result as mapping and then flattening
    def propFlatMap(xs: List[Int32]) =      List.flatMap(x -> x :: x :: Nil, xs) == List.map(x -> x :: x :: Nil, xs) |> List.flatten;
    def propFlatMap2(xs: List[Char]) =      List.flatMap(x -> x :: x :: Nil, xs) == List.map(x -> x :: x :: Nil, xs) |> List.flatten;
    def propFlatMap3(xs: List[Float64]) =   List.flatMap(x -> x :: x :: Nil, xs) == List.map(x -> x :: x :: Nil, xs) |> List.flatten;
    def propFlatMap4(xs: List[String]) =    List.flatMap(x -> x :: x :: Nil, xs) == List.map(x -> x :: x :: Nil, xs) |> List.flatten;
    def propFlatMap5(xs: List[Bool]) =      List.flatMap(x -> x :: x :: Nil, xs) == List.map(x -> x :: x :: Nil, xs) |> List.flatten;

    let propList = forAll(Arbitrary.arbitrary(), propFlatMap) :: 
                    forAll(Arbitrary.arbitrary(), propFlatMap2) :: 
                    forAll(Arbitrary.arbitrary(), propFlatMap3) :: 
                    forAll(Arbitrary.arbitrary(), propFlatMap4) :: 
                    forAll(Arbitrary.arbitrary(), propFlatMap5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// flatten
pub def testFlatten(): Unit \ IO =
    // Property that checks if the flatten function returns the same result as mapping x to x and then flattening
    def propFlatten(xs: List[List[Int32]]) =    List.flatten(xs) == List.flatMap(x -> x, xs);
    def propFlatten2(xs: List[List[Char]]) =    List.flatten(xs) == List.flatMap(x -> x, xs);
    def propFlatten3(xs: List[List[Float64]]) = List.flatten(xs) == List.flatMap(x -> x, xs);
    def propFlatten4(xs: List[List[String]]) =  List.flatten(xs) == List.flatMap(x -> x, xs);
    def propFlatten5(xs: List[List[Bool]]) =    List.flatten(xs) == List.flatMap(x -> x, xs);

    let propList = forAll(Arbitrary.arbitrary(), propFlatten) :: 
                    forAll(Arbitrary.arbitrary(), propFlatten2) :: 
                    forAll(Arbitrary.arbitrary(), propFlatten3) :: 
                    forAll(Arbitrary.arbitrary(), propFlatten4) :: 
                    forAll(Arbitrary.arbitrary(), propFlatten5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// fold
pub def testFold(): Unit \ IO =
    // Property that checks if the fold function returns the same result as the foldLeft function
    def propFold(xs: List[Int32]) =     List.fold(xs) == List.foldLeft((acc, x) -> acc + x, 0, xs);
    def propFold2(xs: List[Float64]) =  List.fold(xs) == List.foldLeft((acc, x) -> acc + x, 0.0, xs);
    def propFold3(xs: List[String]) =   List.fold(xs) == List.foldLeft((acc, x) -> acc + x, "", xs);
    // Note, cannot fold over Bool or Char as they are not monoids, and not in the SemiGroup

    let propList = forAll(Arbitrary.arbitrary(), propFold) :: 
                    forAll(Arbitrary.arbitrary(), propFold2) :: 
                    forAll(Arbitrary.arbitrary(), propFold3) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }
// fold2
// Alias for foldLeft2.
pub def testFold2(): Unit \ IO =
    // Property that checks if the fold2 function returns the same result as the foldLeft2 function
    def propFold2(xs: List[Int32], ys: List[Int32]) =       List.fold2((acc, x, y) -> acc + x + y, 0, xs, ys) == List.foldLeft2((acc, x, y) -> acc + x + y, 0, xs, ys);
    def propFold2_2(xs: List[Float64], ys: List[Float64]) = List.fold2((acc, x, y) -> acc + x + y, 0.0, xs, ys) == List.foldLeft2((acc, x, y) -> acc + x + y, 0.0, xs, ys);
    def propFold2_3(xs: List[String], ys: List[String]) =   List.fold2((acc, x, y) -> acc + x + y, "", xs, ys) == List.foldLeft2((acc, x, y) -> acc + x + y, "", xs, ys);

    // Note, cannot add Bools, so we check if any is true
    def propFold2_4(xs: List[Bool], ys: List[Bool]) =       List.fold2((acc, x, y) -> acc or x or y, false, xs, ys) ==  List.foldLeft2((acc, x, y) -> acc or x or y, false, xs, ys);

    // Cannot add Chars, so we check if the elements are different, and this should be the same as zipping the two lists and checking if any of the elements are different in that list
    def propFold2_5(xs: List[Char], ys: List[Char]) =       List.fold2((acc, x, y) -> if (x != y) acc else false, true, xs, ys) == 
                                                                List.foldLeft2((acc, x, y) -> if (x != y) acc else false, true, xs, ys);

    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFold2) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFold2_2) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFold2_3) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFold2_4) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFold2_5) :: Nil;  

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// foldLeft
pub def testFoldLeft(): Unit \ IO =
    // Property that checks if the foldLeft function returns the same result when applying foldleft to only to the tail of the list, while seperately evaluating the head
    def propFoldLeft(xs: List[Int32]) = 
        match xs {
            case Nil        => List.foldLeft((acc, x) -> acc + x, 0, xs) == 0
            case h :: Nil   => List.foldLeft((acc, x) -> acc + x, 0, xs) == h
            case h :: t     => List.foldLeft((acc, x) -> acc + x, 0, xs) == List.foldLeft((acc, x) -> acc + x, h, t)
        };
    def propFoldLeft2(xs: List[Float64]) = 
        match xs {
            case Nil        => List.foldLeft((acc, x) -> acc + x, 0.0, xs) == 0.0
            case h :: Nil   => List.foldLeft((acc, x) -> acc + x, 0.0, xs) == h
            case h :: t     => List.foldLeft((acc, x) -> acc + x, 0.0, xs) == List.foldLeft((acc, x) -> acc + x, h, t)
        };
    def propFoldLeft3(xs: List[String]) = 
        match xs {
            case Nil        => List.foldLeft((acc, x) -> acc + x, "", xs) == ""
            case h :: Nil   => List.foldLeft((acc, x) -> acc + x, "", xs) == h
            case h :: t     => List.foldLeft((acc, x) -> acc + x, "", xs) == List.foldLeft((acc, x) -> acc + x, h, t)
        };
    def propFoldLeft4(xs: List[Bool]) = 
        match xs {
            case Nil        => List.foldLeft((acc, x) -> acc or x, false, xs) == false
            case h :: Nil   => List.foldLeft((acc, x) -> acc or x, false, xs) == h
            case h :: t     => List.foldLeft((acc, x) -> acc or x, false, xs) == List.foldLeft((acc, x) -> acc or x, h, t)
        };
    def propFoldLeft5(xs: List[Char]) =
          match xs {
            case Nil        => List.foldLeft((acc, x) -> if (acc == x) acc else 'Z', 'A', xs) == 'A'
            case h :: Nil   => List.foldLeft((acc, x) -> if (acc == x) acc else 'Z', 'A', xs) == if (h == 'A') 'A' else 'Z'
            case h :: t     => List.foldLeft((acc, x) -> if (acc == x) acc else 'Z', 'A', xs) == if (List.foldLeft((acc, x) -> if (acc == x) acc else 'Z', h, t) == 'A') 'A' else 'Z'
        };

    let propList = forAll(Arbitrary.arbitrary(), propFoldLeft) :: 
                    forAll(Arbitrary.arbitrary(), propFoldLeft2) :: 
                    forAll(Arbitrary.arbitrary(), propFoldLeft3) :: 
                    forAll(Arbitrary.arbitrary(), propFoldLeft4) ::
                    forAll(Arbitrary.arbitrary(), propFoldLeft5) :: Nil;
    
    foreach (prop <- propList) {
        flixCheck(prop)
    }

// foldLeft2
pub def testFoldLeft2(): Unit \ IO =
    // Property that checks if the foldLeft2 function returns the same result as zipping the two lists, and then using foldLeft on the zipped list 
    def propFoldLeft2(xs: List[Int32], ys: List[Int32]) =       List.foldLeft2((acc, x, y) -> acc + x + y, 0, xs, ys) == List.zip(xs, ys) |> List.foldLeft((acc, x) -> acc + fst(x) + snd(x), 0);
    def propFoldLeft2_2(xs: List[Float64], ys: List[Float64]) = List.foldLeft2((acc, x, y) -> acc + x + y, 0.0, xs, ys) == List.zip(xs, ys) |> List.foldLeft((acc, x) -> acc + fst(x) + snd(x), 0.0);
    def propFoldLeft2_3(xs: List[String], ys: List[String]) =   List.foldLeft2((acc, x, y) -> acc + x + y, "", xs, ys) == List.zip(xs, ys) |> List.foldLeft((acc, x) -> acc + fst(x) + snd(x), "");

    // Note, cannot add Bools, so we check if any is true
    def propFoldLeft2_4(xs: List[Bool], ys: List[Bool]) =       List.foldLeft2((acc, x, y) -> acc or x or y, false, xs, ys) ==  List.zip(xs, ys) |> List.foldLeft((acc, x) -> acc or fst(x) or snd(x), false);

    // Cannot add Chars, so we check if the elements are different, and this should be the same as zipping the two lists and checking if any of the elements are different in that list
    def propFoldLeft2_5(xs: List[Char], ys: List[Char]) =       List.foldLeft2((acc, x, y) -> if (x != y) acc else false, true, xs, ys) == 
                                                                List.foldLeft((acc, x) -> if (fst(x) != snd(x)) acc else false, true, List.zip(xs, ys));
    
    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldLeft2) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldLeft2_2) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldLeft2_3) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldLeft2_4) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldLeft2_5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// foldMap
pub def testFoldMap(): Unit \ IO =
    // Property that checks if the foldMap function returns the same result as mapping and then folding
    def propFoldMap(xs: List[Int32]) =      List.foldMap(x -> x + 1, xs) == List.map(x -> x + 1, xs) |> List.foldLeft((acc, x) -> acc + x, 0);
    def propFoldMap2(xs: List[Float64]) =   List.foldMap(x -> x + 1.0, xs) == List.map(x -> x + 1.0, xs) |> List.foldLeft((acc, x) -> acc + x, 0.0);
    def propFoldMap3(xs: List[String]) =    List.foldMap(x -> x + "a", xs) == List.map(x -> x + "a", xs) |> List.foldLeft((acc, x) -> acc + x, "");
    // mapping all true to 1 and false to 0, then summing should be the same as counting the number of true
    def propFoldMap4(xs: List[Bool]) =      List.foldMap(x -> if (x == true) 1 else 0, xs) == List.map(x -> if (x == true) 1 else 0, xs) |> List.foldLeft((acc, x) -> acc + x, 0);    
    use Char.{isLowerCase, isUpperCase};
    // Counts the number of lower case characters in the list 
    def propFoldMap5(xs: List[Char]) =      List.foldMap(x -> if (Char.isLowerCase(x)) 1 else 0, xs) == List.map(x -> if (Char.isLowerCase(x)) 1 else 0, xs) |> List.foldLeft((acc, x) -> acc + x, 0);
    // Counts the number of upper case characters in the list 
    def propFoldMap6(xs: List[Char]) =      List.foldMap(x -> if (Char.isUpperCase(x)) 1 else 0, xs) == List.map(x -> if (Char.isUpperCase(x)) 1 else 0, xs) |> List.foldLeft((acc, x) -> acc + x, 0);

    let propList = forAll(Arbitrary.arbitrary(), propFoldMap) :: 
                    forAll(Arbitrary.arbitrary(), propFoldMap2) :: 
                    forAll(Arbitrary.arbitrary(), propFoldMap3) :: 
                    forAll(Arbitrary.arbitrary(), propFoldMap4) ::
                    forAll(Arbitrary.arbitrary(), propFoldMap5) ::
                    forAll(Arbitrary.arbitrary(), propFoldMap6) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// foldRight
pub def testFoldRight(): Unit \ IO =
    // Property that checks if the foldRight function returns the same result as the foldLeft function when the list is reversed
    def propFoldRight(xs: List[Int32]) =     List.foldRight((x, acc) -> x + acc, 0, xs) == List.foldLeft((acc, x) -> x + acc, 0, List.reverse(xs));
    def propFoldRight2(xs: List[Float64]) =  List.foldRight((x, acc) -> x + acc, 0.0, xs) == List.foldLeft((acc, x) -> x + acc, 0.0, List.reverse(xs));
    def propFoldRight3(xs: List[String]) =   List.foldRight((x, acc) -> x + acc, "", xs) == List.foldLeft((acc, x) -> x + acc, "", List.reverse(xs));
    // Note, cannot add Bools, so we check if any is true
    def propFoldRight4(xs: List[Bool]) =     List.foldRight((x, acc) -> x or acc, false, xs) == List.foldLeft((acc, x) -> x or acc, false, List.reverse(xs));
    // Cannot add Chars, so we check if the elements are different, and this should be the same as zipping the two lists and checking if any of the elements are different in that list
    def propFoldRight5(xs: List[Char]) =     List.foldRight((x, acc) -> if (x != acc) x else 'Z', 'A', xs) == List.foldLeft((acc, x) -> if (x != acc) x else 'Z', 'A', List.reverse(xs));

    let propList = forAll(Arbitrary.arbitrary(), propFoldRight) :: 
                    forAll(Arbitrary.arbitrary(), propFoldRight2) :: 
                    forAll(Arbitrary.arbitrary(), propFoldRight3) :: 
                    forAll(Arbitrary.arbitrary(), propFoldRight4) ::
                    forAll(Arbitrary.arbitrary(), propFoldRight5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }
// foldRight2
pub def testFoldRight2(): Unit \ IO =
    // Property that checks if the foldRight2 function returns the same result as the foldLeft2 function when the lists are reversed
    def propFoldRight2(xs: List[Int32], ys: List[Int32]) =       List.foldRight2((x, y, acc) -> x + y + acc, 0, xs, ys) == List.foldLeft2((acc, x, y) -> x + y + acc, 0, List.reverse(xs), List.reverse(ys));
    def propFoldRight2_2(xs: List[Float64], ys: List[Float64]) = List.foldRight2((x, y, acc) -> x + y + acc, 0.0, xs, ys) == List.foldLeft2((acc, x, y) -> x + y + acc, 0.0, List.reverse(xs), List.reverse(ys));
    def propFoldRight2_3(xs: List[String], ys: List[String]) =   List.foldRight2((x, y, acc) -> x + y + acc, "", xs, ys) == List.foldLeft2((acc, x, y) -> x + y + acc, "", List.reverse(xs), List.reverse(ys));

    // Note, cannot add Bools, so we check if any is true
    def propFoldRight2_4(xs: List[Bool], ys: List[Bool]) =       List.foldRight2((x, y, acc) -> x or y or acc, false, xs, ys) ==  List.foldLeft2((acc, x, y) -> x or y or acc, false, List.reverse(xs), List.reverse(ys));

    // Cannot add Chars, so we check if the elements are different, and this should be the same as zipping the two lists and checking if any of the elements are different in that list
    def propFoldRight2_5(xs: List[Char], ys: List[Char]) =       List.foldRight2((x, y, acc) -> if (x != y) acc else false, true, xs, ys) == 
                                                                List.foldLeft2((acc, x, y) -> if (x != y) acc else false, true, List.reverse(xs), List.reverse(ys));

    let propList = forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldRight2) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldRight2_2) :: 
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldRight2_3) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldRight2_4) ::
                    forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), propFoldRight2_5) :: Nil;

    foreach (prop <- propList) {
        flixCheck(prop)
    }

// foldRightWithCont

    // ///
    // /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    // ///
    // /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    // /// A `foldRightWithCont` allows early termination by not calling the continuation.
    // ///
    // pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: List[a]): b \ ef = match l {
    //     case Nil     => z
    //     case x :: xs => f(x, _ -> foldRightWithCont(f, z, xs))
//     // }
// pub def testFoldRightWithCont(): Unit \ IO =
// TODO

pub def devTest(): Unit \ IO  =

    def sumWithEarlyTermination(x: Int32, cont: Unit -> Int32): Int32 =
        if (x == 0) 0 else x + cont();
    
    def exampleUsage(): Int32 = {
        let l = 1:: 2:: 3 :: 0 :: 4 :: 5 :: Nil;
        let z = 0;
        List.foldRightWithCont(sumWithEarlyTermination, z, l)
    };
    let foo = exampleUsage();
    println(foo)
   
// forAll
// forEach
// forEachWithIndex
// frequency
// groupBy
// head
// indexOf
// init
// intercalate
// intersperse
// isEmpty
// isInfixOf
// isPrefixOf
// isSuffixOf
// iterator
// join
// joinWith
// last
// length
// map
// map2
// map3
// map4
// map5
// mapWithIndex
// maximum
// maximumBy
// memberOf
// merge
// minimum
// minimumBy
// nonEmpty
// partition
// patch
// permutations
// point
// range
// reduceLeft
// reduceRight
// repeat
// replace
// reverse
def testReverse(): Unit \ IO =
    // Property that checks if the reverse of a list is the same as the original list
    def propRevRev(xs: List[Char]) = List.reverse(List.reverse(xs)) == xs;
    // Property that checks if the reverse of a single element list is the same as the original list
    def propRev(x: Int32) = (x :: Nil) == List.reverse(x :: Nil);
    // Property that checks if the reverse of the concatenation of two lists is the same as the concatenation of the reversed lists
    def propRevApp(pair: (List[Int32], List[Int32])) = List.append(List.reverse(fst(pair)), List.reverse(snd(pair))) == List.reverse(List.append(snd(pair), fst(pair)));
    let propList = forAll(Arbitrary.arbitrary(), propRevRev) :: forAll(Arbitrary.arbitrary(), propRev) :: forAll(Arbitrary.arbitrary(), propRevApp) :: Nil;
    foreach (prop <- propList) {
        flixCheck(prop)
    }
// rotateLeft
// rotateRight
// scan
// scanLeft
// scanRight
// sequence
// shuffle
// slice
// sort
// sortBy
// sortWith
// span
// splitAt
// subsequences
// sum
// sumWith
// take
// takeWhile
// toArray
// toChain
// toDelayList
// toDelayMap
// toMap
// toMapWith
// toMutDeque
// toMutList
// toNec
// toNel
// toSet
// toString
// toVector

// transpose
/// Testing the properties of the List.Transpose function ///

def testTranspose(): Unit \ IO =
    let matrixGen: Gen[List[List[Int32]]] = 
        sized(size -> 
            chooseInt(1, size) >>= l -> sizedListOf(l, vector(l))
        );
    // Check that the matrix self inverse property holds
    flixCheck(forAll(matrixGen, propSelfInverse));
    // Check that the matrix dimensions property holds
    flixCheck(forAll(matrixGen, propDimensions));
    // Check that the matrix addition property holds
    flixCheck(forAll2(matrixGen, matrixGen, propAddition));
    // Check that the matrix scalar multiplication property holds
    flixCheck(forAll2(matrixGen, chooseInt(0, 100), propScalarMultiplication));
    // Check that the matrix multiplication property holds
    flixCheck(forAll2(matrixGen, matrixGen, propMatrixMultiplication))

// The transpose of the transpose of a matrix is the matrix itself
def propSelfInverse(a: List[List[Int32]]): Bool =
    List.transpose(List.transpose(a)) == a

// When transposed the dimensions should be swapped (2x3 -> 3x2)
def propDimensions(a: List[List[Int32]]): Bool =
    // Helper function for getting the dimensions of a matrix
    def getDimensions(xs: List[List[Int32]]): (Int32, Int32) = (
        let rows = List.length(xs);
        let cols = match List.head(xs) {
            case Some(row) => List.length(row),
            case None => 0
        };
        (rows, cols)
    );
    let (aRows, aCols) = getDimensions(a);
    let (a_TRows, a_TCols) = getDimensions(List.transpose(a));  
    aRows == a_TCols and aCols == a_TRows
    
// (A + B)^T = A^T + B^T
def propAddition(a: List[List[Int32]], b: List[List[Int32]]): Bool =
    // Helper function for adding two matrices
    def addMatrix(xs: List[List[Int32]], ys: List[List[Int32]]): List[List[Int32]] = (
        let addFunc = listPair -> List.map(intPair -> fst(intPair) + snd(intPair), List.zip(fst(listPair), snd(listPair)));
        List.map(addFunc, List.zip(xs, ys))
    );
    List.transpose(addMatrix(a, b)) == addMatrix(List.transpose(a), List.transpose(b))
    
// (kA)^T= kA^T
def propScalarMultiplication(a: List[List[Int32]], k: Int32): Bool =
    List.transpose(List.map(row -> List.map(x -> k * x, row), a)) == List.map(row -> List.map(x -> k * x, row), List.transpose(a))

// (AB)^T=(B^T)(A^T)
def propMatrixMultiplication(a: List[List[Int32]], b: List[List[Int32]]): Bool =
    // Helper function for multiplying two matrices
    def multiplyMatrix(xs: List[List[Int32]], ys: List[List[Int32]]): List[List[Int32]] = (
        let multiplyFunc = row -> List.map(col -> List.foldLeft2((acc, x, y) -> acc + x * y, 0, row, col), List.transpose(ys));
        List.map(multiplyFunc, xs)
    );
    List.transpose(multiplyMatrix(a, b)) == multiplyMatrix(List.transpose(b), List.transpose(a))



// traverse
// unfold
// unfoldWithIter
// unfoldWithOkIter
// unzip
// unzip3
// update
// zip
// zip3
// zipWith
// zipWith3
// zipWithA
// zipWithIndex
