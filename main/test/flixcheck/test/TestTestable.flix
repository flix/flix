use Testable.{Testable, TFunc, TFunc2, forAll, forAll2, Property, label, classify, collect}
use Gen.{chooseInt, Arbitrary, Gen, sizedListOf, sized, vector}
use Monad.{>>=}
// use Testable.{TestableFunc => ts} // TODO : spørg Magnus
pub def testingMain(): Unit \ IO =
    let prop = TFunc.TFunc(x -> x < 100);
    let c = {size = _ -> 99| verbose()};
    check(c, prop)

pub def shouldFail(): Unit \ IO =
    // Define a testable function that should fail
    let f = TFunc.TFunc(x -> Int32.pow(x, 2) < 80);
    // Set the function as a property
    let prop = Testable.property(f);
    println("Verbose check");
    // Check the property
    verboseCheck(prop)

pub def testReverse(): Unit \ IO =
    def prop_revRev(xs: List[Char]) = List.reverse(List.reverse(xs)) == xs;
    def prop_rev(x: Int32) = (x :: Nil) == List.reverse(x :: Nil);
    def prop_revApp(pair: (List[Int32], List[Int32])) = List.append(List.reverse(fst(pair)), List.reverse(snd(pair))) == List.reverse(List.append(snd(pair), fst(pair)));
    let propList = forAll(Arbitrary.arbitrary(), prop_revRev) :: forAll(Arbitrary.arbitrary(), prop_rev) :: forAll(Arbitrary.arbitrary(), prop_revApp) :: Nil;
    foreach (prop <- propList) {
        verboseCheck(prop)
    }

pub def testtt(): Unit \ IO =
    def prop_add(x: Int32, y: Int32) = x + y == y + x;
    let f = TFunc2.TFunc2(prop_add);
    flixCheck(f)

pub def newtest(): Unit \ IO =
    let prob_true = true;
    flixCheck(prob_true)

// Testing the properties of the Transpose function

// The transpose of the transpose of a matrix is the matrix itself
pub def propSelfInverse(a: List[List[Int32]]): Bool =
    List.transpose(List.transpose(a)) == a

// When transposed the dimentions should be swapped (2x3 -> 3x2)
pub def propDimensions(a: List[List[Int32]]): Bool =
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
pub def propAddition(a: List[List[Int32]], b: List[List[Int32]]): Bool =
    // Helper function for adding two matrices
    def addMatrix(xs: List[List[Int32]], ys: List[List[Int32]]): List[List[Int32]] = (
        let addFunc = listPair -> List.map(intPair -> fst(intPair) + snd(intPair), List.zip(fst(listPair), snd(listPair)));
        List.map(addFunc, List.zip(xs, ys))
    );
    List.transpose(addMatrix(a, b)) == addMatrix(List.transpose(a), List.transpose(b))
    

pub def testTranspose(): Unit \ IO =
    let matrixGen: Gen[List[List[Int32]]] = 
        sized(_ -> 
            chooseInt(0, 0) >>= l -> sizedListOf(l, vector(l))
        );
    // Check that the matrix self inverse property holds
    flixCheck(forAll(matrixGen, propSelfInverse));
    // Check that the matrix dimensions property holds
    flixCheck(forAll(matrixGen, propDimensions));
    // Check that the matrix addition property holds
    flixCheck(forAll2(matrixGen, matrixGen, propAddition))

pub def testClassify(): Unit \ IO =
    // tests and prints the distribution of n % 2 and stamps them with "even"
    let prop = forAll(chooseInt(1, 10), n -> classify(Int32.modulo(n, 2) == 0, "even", n == n));
    flixCheck(prop)
    
pub def testCollectAndClassify(): Unit \ IO =
    // prints both the individual distribution of numbers and the distribution of n % 2
    let prop = forAll(chooseInt(1, 10), n -> collect(n, classify(Int32.modulo(n, 2) == 0, "even", n == n)));
    flixCheck(prop)

   