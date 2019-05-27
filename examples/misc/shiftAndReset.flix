//def main(): Int = gcd(5,2)
//
//def gcd(x: Int, y: Int): Int =
//    if (y==0) x
//    else gcd(y, x%y)

//def main(): Int = applyN(inc, 0, 10)
//
//def inc(x: Int): Int = x+1
//
//def applyN(f: Int -> Int, x:Int, n: Int): Int =
//    if (0 >= n) x
//    else applyN(f, f(x), n-1)

/// Returns the sum of x and y.
//def add(x: Int, y: Int): Int = x + y
//
///// Returns x plus one.
//def inc(x: Int): Int = add(x, 1)
//
///// Returns a function that applies f twice.
//def twice(f: Int -> Int): Int -> Int = x -> f(f(x))
//
///// Returns x plus two.
//def two(x: Int): Int = twice(inc)(x)
//
///// Returns 123 plus 4 = 127.
//def main(): Int = twice(two)(123)


//def main(): Int = f(1)
//def f(x: Int): Int =
//    let y = 1;
//    x+y


//def main(): Int = f(2)
//
//def f(a: Int): Int =
//    if (a==0) a
//    else f(a-1)

def main(): List[Int] = List.range(1,5)


//def main(): Int32 =
//  mainP(x -> x)
//
//def mainP[a](k: Int32 -> a): a =
//  (g -> (x -> g(x, k))(2))(fP)
//
//def fP[a](x: Int32, k: Int32 -> a): a =
//  (y -> (z -> (c -> (
//    if (c) k(x)
//    else (g -> (m -> (n -> (xP -> (g(xP, k)))(m - n))(1))(x))(fP)
//  ))(y == z))(0))(x)