namespace Cards {
    enum Suit {
        case Hearts,
        case Diamonds,
        case Clubs,
        case Spades
    };

    enum Card {
        case Jack(Suit),
        case Queen(Suit),
        case King(Suit),
        case Ace(Suit),
        case Number(Int, Suit)
    };

    def isAce(c: Card): Bool = match c with {
        case Card.Ace(_) => true;
        case _ => false;
    }

    rel Hand(player: String, c1: Card, c2: Card)

    Hand("Albert Einstein", Card.Queen(Suit.Hearts), Card.Queen(Suit.Clubs)).
    Hand("Niels Bohr", Card.Number(10, Suit.Clubs), Card.Number(2, Suit.Clubs)).
    Hand("Isaac Newton", Card.Ace(Suit.Diamonds), Card.King(Suit.Diamonds)).
    Hand("Cheater", Card.Ace(Suit.Hearts), Card.Ace(Suit.Hearts)).

    rel HasDuplicateCards(name: String)
    HasDuplicateCards(x) :- Hand(x, c, c).

    rel HasPocketAces(name: String)
    HasPocketAces(x) :- isAce(c1), isAce(c2), Hand(x, c1, c2).

    //Not allowed
    //HasPocketAces(x) :- Hand(x, Card.Ace(_), Card.Ace(_)).
}