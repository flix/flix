def runTests(): (Int, Int) = {
    (2, 2)
}

def newcTest(): Channel[Int] = newch

def getchTest(): Int = getch newcTest()

def putchTest(): Channel[Int] = putch newcTest() 23

def closechTest(): Unit = closech newcTest()

def spawnTest(): Unit = spawn 2

def selectTest(): Int = {
    let c = newch;
    select {
        case x <- c => 2
        case y <- putchTest() => 3
    }
}

def statementTest(): Int = {1 + 2 ; 2 + 3}