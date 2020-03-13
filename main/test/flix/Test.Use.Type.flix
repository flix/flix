use A.Color
use A/B.Currency
use A/B.Kelvin

namespace X {
    pub def f(): Color = Red
    pub def g(): Currency = USD
    pub def h(): Kelvin = Kelvin(123)
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
        pub opaque type Kelvin = Int
    }

}
