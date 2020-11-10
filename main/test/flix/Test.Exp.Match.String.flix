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

}
