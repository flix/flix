@test
def testUseEnum01(): Bool =
    use A.Color.Red;
    Red == Red

@test
def testUseEnum02(): Bool =
    use A.Color.Blu;
    Blu == Blu

@test
def testUseEnum03(): Bool =
    use A.Color.Red;
    use A.Color.Blu;
    Red != Blu

@test
def testUseEnum04(): Bool =
    use A.Color.{Red, Blu};
    Red != Blu

@test
def testUseEnum05(): Bool =
    use A.Color.{Red => R};
    R == R

@test
def testUseEnum06(): Bool =
    use A.Color.{Blu => B};
    B == B

@test
def testUseEnum07(): Bool =
    use A.Color.{Red => R};
    use A.Color.{Blu => B};
    R != B

@test
def testUseEnum08(): Bool =
    use A.Color.{Red => R, Blu => B};
    R != B

@test
def testUseEnum09(): Bool =
    use A.Color;
    Color.Red == Color.Red

@test
def testUseEnum10(): Bool =
    use A.Color;
    Color.Blu == Color.Blu

@test
def testUseEnum11(): Bool =
    use A.Color;
    Color.Red != Color.Blu

@test
def testUseEnum12(): Bool =
    use A/B.Currency.USD;
    USD == USD

@test
def testUseEnum13(): Bool =
    use A/B.Currency.CAD;
    CAD == CAD

@test
def testUseEnum14(): Bool =
    use A/B.Currency.USD;
    use A/B.Currency.CAD;
    USD != CAD

@test
def testUseEnum15(): Bool =
    use A/B.Currency.{USD, CAD};
    USD != CAD

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
