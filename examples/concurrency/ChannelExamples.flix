def allTests(): (Int, Int) = {
    (2, 2)
}

//def newchTest(): Channel[Int] = newch Int
//
//def getchTest(): Int = <- newchTest()

def putchTest(): Int =
    let c = newch Int;
    let c2 = c <- 23;
    let c3 = c2 <- 22;
    <- c
//
//def closechTest(): Unit = closech newchTest()
//

def f(c: Channel[Int]): List[Channel[Int]] = List.map(x -> c <- x, List.range(1, 50))

def spawnTest(): Int =
    let c = newch Int;
    spawn f(c); spawn f(c); spawn f(c); spawn f(c); spawn f(c);
    spawn f(c); spawn f(c); spawn f(c); spawn f(c); spawn f(c);
    <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c


def selectTest(): Int = {
    let c1 = newch Int;
    let c2 = newch Int;
    select {
        case x <- c1 => 2
        case y <- c2 => 3
    }
}
//
//def statementTest(): Int = 1 ; 2