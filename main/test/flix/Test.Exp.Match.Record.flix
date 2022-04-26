namespace Test/Exp/Match/Record {

    @test
    def testMatchRecord01(): Bool = match {x = 12, y = 10} {
        case { x, y } => x == 12 && y == 10
        case _ => false
    }

    @test
    def testMatchRecord02(): Bool = match {x = "a", y = "b"} {
        case { x, y } => x == "a" && y == "b"
        case _ => false
    }

    @test
    def testMatchRecord03(): Bool = match {x = 12, y = 10} {
        case { x } => x == 12
        case _ => false
    }

    @test
    def testMatchRecord04(): Bool = match {x = 12, y = 10} {
        case { y } => y == 10
        case _ => false
    }

    @test
    def testMatchRecord05(): Bool = match {x = 12, y = 10} {
        case { y } => y == 10
        case { x } => false
        case _ => false
    }

    @test
    def testMatchRecord06(): Bool = match {y = 10, x = 12} {
        case { x, y } => x == 12 && y == 10
        case _ => false
    }

    @test
    def testMatchRecord07(): Bool = match {y = 10, x = 12} {
        case { x } => x == 12
    }

    @test
    def testMatchRecord08(): Bool = match {x = 10, y = 12, v = { z = 11 } } {
        case { x, y, v = { z } } => x == 10 && y == 12 && z == 11
    }

    @test
    def testMatchRecord09(): Bool = match {x = 10, y = 12, v = { z = 11 } } {
        case { x, v = { z } } => x == 10 && z == 11
    }

    @test
    def testMatchRecord10(): Bool = match {x = 10, y = 12, v = { z = 11 } } {
        case { x, v = { z } } => x == 10 && z == 11
        case _ => false
    }
}
