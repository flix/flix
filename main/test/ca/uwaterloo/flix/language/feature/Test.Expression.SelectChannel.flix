//TODO SJ: make tests
def testSelectChannel(): Int =
    let c1 = chan Int;
    c1 <- 2;
    select {
        case x <- c1 => x
    }