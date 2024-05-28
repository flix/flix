use Monad.{>>=}

def sizeConfig(): Unit \ IO =
    let prop = TFunc.TFunc(x -> x < 100);
    let c = {size = _ -> 99| verbose()};
    check(c, prop)

def shouldFail(): Unit \ IO =
    // Define a testable function that should fail
    let f = TFunc.TFunc(x -> Int32.pow(x, 2) <= 81);
    // Set the function as a property
    let prop = Testable.property(f);
    // Check the property
    //verboseCheck(prop)
    check(verbose(), prop)

def testReverse2(): Unit \ IO =
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

def propRevRev(xs: List[Int32]): Bool =
    List.reverse(List.reverse(xs)) == xs

def propWithCollect(): Unit \ IO =
    let evenListGen = chooseInt(0, 5) >>= n -> vector(2*n);
    let prop = forAll(evenListGen, n -> collect(List.length(n), propRevRev(n)));
    flixCheck(prop)

/// Testing the properties of the List.Transpose function ///

// The transpose of the transpose of a matrix is the matrix itself
def propSelfInverse(a: List[List[Int32]]): Bool =
    List.transpose(List.transpose(a)) == a

// When transposed the dimentions should be swapped (2x3 -> 3x2)
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
    flixCheck(forAll2(matrixGen, matrixGen, propAddition))


def testClassify(): Unit \ IO =
    // tests and prints the distribution of n % 2 and stamps them with "even" or "odd"
    def prop(n: Int32): Property = (
        (n == n) |> 
        classify(Int32.modulo(n, 2) == 0, "even") |>
        classify(Int32.modulo(n, 2) == 1, "odd")
    );
    flixCheck(forAll(chooseInt(1, 10), prop))
    
def testCollectAndClassify(): Unit \ IO =
    // prints both the individual distribution of numbers and the distribution of n % 2
    def prop(n: Int32): Property = (
        (n == n) |>
        classify(Int32.modulo(n, 2) == 0, "even") |>
        collect(n)
    );
    flixCheck(forAll(chooseInt(1, 10), prop))
    
// Test the max function, using the `implies` function to define the property
def testMaxLe(): Unit \ IO =
    // tests the max function
    def propMaxLe(x: Int32, y: Int32) = 
        (x <= y) ==> (Int32.max(x, y) == y);
    flixCheck(TFunc2.TFunc2(propMaxLe))

// Test and hopefully crash modulo
def testModulo(): Unit \ IO =
    // tests the modulo function, assuming that the result is non-negative
    def propModulo(x: Int32, y: Int32) = 
        Int32.modulo(x, y) >= 0;
    verboseCheck(TFunc2.TFunc2(propModulo))

def insertOrdered(e: a, l: List[a]): List[a] with Order[a] =
    match l {
        case Nil => e :: Nil,
        case (y::ys) if e < y => e :: y :: ys,
        case (y::ys) => y :: insertOrdered(e, ys)
    }

def isOrdered(l: List[a]): Bool with Order[a] =
    // checks if a list is ordered
    match l {
        case (x :: y :: xs) => x <= y and isOrdered(y :: xs),
        case _ => true
    }

def testInsertOrdered(): Unit \ IO =
    let propInsertOrdered = TFunc2.TFunc2(
        (e: Int32, xs: List[Int32]) -> implies(isOrdered(xs), isOrdered(insertOrdered(e, xs)))
    );
    // tests the insertOrdered function
    check({maxFail = 500| default()}, propInsertOrdered)

def testInsertOrdered2(): Unit \ IO =
    // tests the insertOrdered function
    let genOrderedList = sized(size -> orderedListOf(size, Arbitrary.arbitrary()));
    def propInsertOrdered(e: Int32, xs: List[Int32]) = implies(isOrdered(xs), isOrdered(insertOrdered(e, xs)));
    flixCheck(forAll2(Arbitrary.arbitrary(), genOrderedList, propInsertOrdered))

def testModuloBiggerThan(): Unit \ IO =
    // Tests that x modulo y equals x, when y is larger than x, and both are positive/larger than 1
    def propModulo(x: Int32, y: Int32) = implies(y > x and y > 1 and x > 1, Int32.modulo(x, y) == x);
    verboseCheck(TFunc2.TFunc2(propModulo))