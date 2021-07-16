namespace Test/Exp/Match/String {

    @test
    def testMatchString01(): Bool = match "" {
        case ""    => true
        case "a"   => false
        case "abc" => false
        case _     => false
    }

    @test
    def testMatchString02(): Bool = match "a" {
        case ""    => false
        case "a"   => true
        case "abc" => false
        case _     => false
    }

    @test
    def testMatchString03(): Bool = match "abc" {
        case ""    => false
        case "a"   => false
        case "abc" => true
        case _     => false
    }

    @test
    def testMatchString04(): Bool = match "def" {
        case ""    => false
        case "a"   => false
        case "abc" => false
        case _     => true
    }

    @test
    def testMatchString05(): Bool = match "\"" {
        case "\"" => true
        case _ => false
    }

    @test
    def testMatchStringVar01(): Bool = match "abc" {
        case x => x == "abc"
    }

    @test
    def testMatchStringVar02(): Bool = match "def" {
        case ""    => false
        case "a"   => false
        case "abc" => false
        case x     => x == "def"
    }

}
