use A.Color
use A/B.Currency
use A/B.Kelvin
use A.{Color => MyColor}
use A/B.{Currency => MyCurrency}

namespace X {
    pub def f(): Color = Red
    pub def g(): Currency = USD
    pub def h(): Kelvin = Kelvin(123)
    pub def j(): Currency = Currency.USD
    pub def k(): Currency = Currency.CAD
    pub def l(): MyColor = MyColor.Red
    pub def m(): MyCurrency = MyCurrency.USD
    pub def n(): MyCurrency = MyCurrency.CAD
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
        type alias Meter = Int
    }

}
