//def main(): Int = gcd(5,2)
//
//def gcd(x: Int, y: Int): Int =
//    if (y==0) x
//    else gcd(y, x%y)

def main(): Int = applyN(inc, 0, 10)

def inc(x: Int): Int = x+1

//def add(x: Int, y: Int): Int = x+y

def applyN(f: Int -> Int, x:Int, n: Int): Int =
    if (0 >= n) x
    else applyN(f, f(x), n)


//def main(): Int = f(1)
//def f(x: Int): Int =
//    let y = 1;
//    x+y