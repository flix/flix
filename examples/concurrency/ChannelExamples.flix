def runTests(): (Int, Int) = {
    (2, 2)
}

def makechTest(): Channel[Int] = makech[Int]

def getchTest(): Int = getch makechTest()

def putchTest(): Channel[Int] = putch makechTest() 23

def closechTest(): Unit = closech makechTest()

def spawnTest(): Unit = spawn 2

def selectTest(): Unit = {
    select {
        case 2=> 2
        case 3 =>
            3
        default =>
            4
    }
}