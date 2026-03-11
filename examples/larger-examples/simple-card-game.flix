// A Suit type deriving an Eq and ToString instance
enum Suit with Eq, ToString {
    case Clubs
    case Hearts
    case Spades
    case Diamonds
}

// A Rank type deriving an Eq and Order instance
enum Rank with Eq, Order {
    case Number(Int32)
    case Jack
    case Queen
    case King
    case Ace
}

// A Card type deriving an Eq instance
enum Card(Rank, Suit) with Eq

// An instance of ToString for Ranks
instance ToString[Rank] {
    pub def toString(x: Rank): String = match x {
        case Rank.Number(n) => "${n}"
        case Rank.Jack      => "Jack"
        case Rank.Queen     => "Queen"
        case Rank.King      => "King"
        case Rank.Ace       => "Ace"
    }
}

// An instance of ToString for Cards
instance ToString[Card] {
    pub def toString(x: Card): String = match x {
        case Card.Card(r, s) => "${r} of ${s}"
    }
}

// Simulates a game of War, printing each player's turn.
def playWar(p1: List[Card], p2: List[Card], spoils: List[Card]): Unit \ IO = match (p1, p2) {
    case (Nil, Nil) => println("No one has any cards. It's a draw.")
    case (Nil, _) => println("Player 1 is out of cards. Player 2 wins!")
    case (_, Nil) => println("Player 2 is out of cards. Player 1 wins!")
    case (c1 :: d1, c2 :: d2) =>
        let Card.Card(r1, _) = c1;
        let Card.Card(r2, _) = c2;
        println("Player 1 plays ${c1}. Player 2 plays ${c2}.");
        if (r1 > r2) {
            println("Player 1 wins the battle.");
            // Add the spoils and losing card to the winner's deck.
            playWar(d1 ::: c1 :: c2 :: spoils, d2, Nil)
        } else if (r2 > r1) {
            println("Player 2 wins the battle.");
            // Add the spoils and losing card to the winner's deck.
            playWar(d1, d2 ::: c1 :: c2 :: spoils, Nil)
        } else {
            println("The battle is a draw. Time for war!");
            // Each player contributes their top 3 cards to the spoils
            let s1 = List.take(3, d1);
            let s2 = List.take(3, d2);
            // Their decks are what remains
            let rest1 = List.drop(3, d1);
            let rest2 = List.drop(3, d2);
            // Add the cards to the spoils deck
            playWar(rest1, rest2, s1 ::: s2 ::: c1 :: c2 :: spoils)
        }
}
