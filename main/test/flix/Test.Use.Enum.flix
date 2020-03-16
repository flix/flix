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
