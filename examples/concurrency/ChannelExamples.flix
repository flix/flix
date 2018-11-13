//def allTests(): Int = {
//    let x = 2;
//    x + 2;
//    x
//}

//def chanTest(): Channel[Float64] = chan Float64

//
//def getchTest(): Bool = let c = chan Bool; let c1 = c <- true; <- c1

//def putchTest(): Channel[Bool] = let c = chan Bool; c <- true
//
def closechTest(): Channel[Int] = let c = chan Int; let c1 = closech c; c <- 1
//

//def collatz(n: Int): Int =
   // let c = chan Int;
  //  c <- n;


//def slowFunction(n: Int): Int = collatz(n)

//def f(c: Channel[Int]): List[Channel[Int]] = List.map(x -> c <- x, List.range(1, 5))
//
//@test
//def spawnTest(): Int =
//    let c = chan Int;
//    spawn f(c); spawn f(c); spawn f(c); spawn f(c); spawn f(c);
//    spawn f(c); spawn f(c); spawn f(c); spawn f(c); spawn f(c);
//    <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c
//
//
def selectTest(): Int = {
    let c1 = chan Int;
    let asd = c1 <- 1;
    let c2 = chan Int;
    let fgh = c2 <- 2;
    select {
        case x <- c2 => x + 1
        case y <- c1 => y + 1
    }
}
//
//def statementTest(): Int = 1 ; 2