enum Suit with Eq, Order, ToString {
    case Diamonds,  // ♦
    case Clubs,     // ♣
    case Hearts,    // ♥
    case Spades     // ♠
}

def main(): Unit \ IO =
    println("${Suit.Diamonds == Suit.Diamonds}");
    println("${Suit.Diamonds <= Suit.Hearts}");
    println("${Suit.Diamonds}")
