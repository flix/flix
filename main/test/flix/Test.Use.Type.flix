use A.Color
use A/B.Currency

namespace X {

    pub def f(): Color = Red

    pub def g(): Currency = USD


}


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
