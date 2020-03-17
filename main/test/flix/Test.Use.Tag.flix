namespace Test/Use/Tag {

    @test
    def testUseEnum01(): Bool =
        use Test/Use/Tag/A.Color.Red;
        Red == Red

    @test
    def testUseEnum02(): Bool =
        use Test/Use/Tag/A.Color.Blu;
        Blu == Blu

    @test
    def testUseEnum03(): Bool =
        use Test/Use/Tag/A.Color.Red;
        use Test/Use/Tag/A.Color.Blu;
        Red != Blu

    @test
    def testUseEnum04(): Bool =
        use Test/Use/Tag/A.Color.{Red, Blu};
        Red != Blu

    @test
    def testUseEnum05(): Bool =
        use Test/Use/Tag/A.Color.{Red => R};
        R == R

    @test
    def testUseEnum06(): Bool =
        use Test/Use/Tag/A.Color.{Blu => B};
        B == B

    @test
    def testUseEnum07(): Bool =
        use Test/Use/Tag/A.Color.{Red => R};
        use Test/Use/Tag/A.Color.{Blu => B};
        R != B

    @test
    def testUseEnum08(): Bool =
        use Test/Use/Tag/A.Color.{Red => R, Blu => B};
        R != B

    @test
    def testUseEnum09(): Bool =
        use Test/Use/Tag/A.Color;
        Color.Red == Color.Red

    @test
    def testUseEnum10(): Bool =
        use Test/Use/Tag/A.Color;
        Color.Blu == Color.Blu

    @test
    def testUseEnum11(): Bool =
        use Test/Use/Tag/A.Color;
        Color.Red != Color.Blu

    @test
    def testUseEnum12(): Bool =
        use Test/Use/Tag/A/B.Currency.USD;
        USD == USD

    @test
    def testUseEnum13(): Bool =
        use Test/Use/Tag/A/B.Currency.CAD;
        CAD == CAD

    @test
    def testUseEnum14(): Bool =
        use Test/Use/Tag/A/B.Currency.USD;
        use Test/Use/Tag/A/B.Currency.CAD;
        USD != CAD

    @test
    def testUseEnum15(): Bool =
        use Test/Use/Tag/A/B.Currency.{USD, CAD};
        USD != CAD

    @test
    def testUseEnum16(): Bool =
        use Test/Use/Tag/A/B.Currency;
        Currency.CAD == Currency.CAD

    @test
    def testUseEnum17(): Bool =
        use Test/Use/Tag/A/B.Currency;
        Currency.USD == Currency.USD

    @test
    def testUseEnum18(): Bool =
        use Test/Use/Tag/A/B.Currency;
        Currency.CAD != Currency.USD

    namespace A {
        pub enum Color {
            case Red,
            case Blu
        }
        namespace B {
            pub enum Currency {
                case USD,
                case CAD
            }
        }
    }
}
