/// We derive the type classes Eq, Order, and ToString for the enum Month
enum Month with Eq, Order, ToString {
    case January
    case February
    case March
    case April
    case May
    case June
    case July
    case August
    case September
    case October
    case November
    case December
}

type alias Year = Int
type alias Day = Int

/// The Date type derives the type classes Eq and Order
opaque type Date with Eq, Order = (Year, Month, Day)

/// We implement our own instance of ToString for Date
/// since we don't want the default "Date(1948, December, 10)"
instance ToString[Date] {
    pub def toString(x: Date): String =
        let Date(y, m, d) = x;
        "${d} ${m}, ${y}"
}

/// Thanks to the Eq and Order type classes, we can easily compare dates.
def earlierDate(d1: Date, d2: Date): Date = Order.min(d1, d2)

/// Thanks to the ToString type class, we can easily convert dates to strings.
def printDate(d: Date): Unit & Impure =
    let message = "The date is ${d}!";
    println(message)
