use Testable.{forAll, Property, TFunc}
use Gen.{Arbitrary}
def testOddEven(): Unit \ IO =
    oddOrEven(12345) |> println;
    oddOrEven(1234) |> println;
    oddOrEven(0) |> println;
    oddOrEven(1) |> println;
    oddOrEven(42) |> println;
    oddOrEven(-1) |> println
    
// now missing the case for negative numbers
def isOdd(n: Int32): Bool =
    if (n == 0) false else if (n < 0) isEvn(n) else isEvn(n - 1)

def isEvn(n: Int32): Bool =
    if (n == 0) true else if (n < 0) isOdd(n + 1) else isOdd(n - 1)

// For nice strings
def oddOrEven(n: Int32): Unit \ IO = 
    let res = isOdd(n);
    if (res) println("The number ${n} is Odd") 
        else println("The number ${n} is Even") 

pub def flixCheckOddEven(): Unit \ IO =
    flixCheck(forAll(Arbitrary.arbitrary(), n -> isOdd(n) == not isEvn(n)))