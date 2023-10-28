/// We derive the traits Eq, Order, and ToString for the enum Month
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

type alias Year = Int32
type alias Day = Int32

/// The Date type derives the traits Eq and Order
enum Date(Year, Month, Day) with Eq, Order

/// We implement our own instance of ToString for Date
/// since we don't want the default "Date(1948, December, 10)"
instance ToString[Date] {
    pub def toString(x: Date): String =
        let Date.Date(y, m, d) = x;
        "${d} ${m}, ${y}"
}

/// Thanks to the Eq and Order traits, we can easily compare dates.
def earlierDate(d1: Date, d2: Date): Date = Order.min(d1, d2)

/// Thanks to the ToString trait, we can easily convert dates to strings.
def printDate(d: Date): Unit \ IO =
    let message = "The date is ${d}!";
    println(message)
