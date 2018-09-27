def runTests(): (Int, Int) = {
    (2, 2)
}

def newchTest(): Channel[Int] = newch

def getchTest(): Int = getch newchTest()

def putchTest(): Channel[Int] = putch newchTest() 23

def closechTest(): Unit = closech newchTest()

def spawnTest(): Unit = spawn 2

def selectTest(): Int = {
    let c = newch;
    select {
        case x <- c1 => 2
        case y <- putchTest() => 3
    }
}

def statementTest(): Int = "hej"; 3