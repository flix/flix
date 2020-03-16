@test
def testUseEnum01(): Bool =
    use A.Color.Red;
    Red == Red

@test
def testUseEnum02(): Bool =
    use A.Color.Blu;
    Blu == Blu


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
