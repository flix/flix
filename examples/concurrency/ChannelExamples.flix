def allTests(): (Int, Int) = {
    (2, 2)
}

def newchTest(): Channel[Int] = newch Int

def getchTest(): Int = <- newchTest()

def putchTest(): Channel[Int] = newchTest() <- 23

def closechTest(): Unit = closech newchTest()

def spawnTest(): Unit = spawn 2

def selectTest(): Int = {
    let c = newch Int;
    select {
        case x <- c => 2
        case y <- putchTest() => 3
    }
}

def statementTest(): Int = 1 ;; 2