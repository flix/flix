// Player Actions
enum Action {
    case Hit,
    case Stand
}

// Card Ranks
enum Card with ToString {
    case Two,
    case Three,
    case Four,
    case Five,
    case Six,
    case Seven,
    case Eight,
    case Nine,
    case Ten,
    case Jack,
    case Queen,
    case King,
    case Ace
}

// Deck Operations
eff Deck {
    // generate and shuffle a 52 card deck
    def genDeck() : List[Card]
    // pull card
    def pullCard(deck: List[Card]): (Option[Card], List[Card])
}

eff Terminal {
    def println(s: String): Unit
}

def cardValue(card: Card): Int32 = match card {
   case Card.Two => 2
   case Card.Three => 3
   case Card.Four => 4
   case Card.Five => 5
   case Card.Six => 6
   case Card.Seven => 7
   case Card.Eight => 8
   case Card.Nine => 9
   case Card.Ace => 11
   case _ => 10 // 10 and face cards
}
def gameOver(p_val: Int32, d_val: Int32): Bool \ Terminal = match (p_val, d_val) {
    case (p, _) if p == 21 => Terminal.println("player -> ${p}, Player wins."); true
    case (p, _) if p > 21 => Terminal.println("player -> ${p}, Dealer wins."); true
    case (_, d) if d == 21 => Terminal.println("dealer -> ${d}, Dealer wins."); true
    case (_, d) if d > 21 => Terminal.println("dealer -> ${d}, Player wins."); true
    case (_, _) => false
}

def handValue(cards: List[Card]): Int32 =
    List.foldLeft((x, y) -> x + y, 0, List.map(cardValue, cards))

def gameLoop(deck: List[Card], player: List[Card], dealer: List[Card]): Unit \ {Terminal, Deck} = {
    match Deck.pullCard(deck) { // player draws
        case (Some(p_card), afterPlayer)  => {
            Terminal.println("Player => ${List.toString(p_card::player)}");
            // players hand with new card
            let p_val = handValue(p_card::player);
            // dealer never hits after 16
            if (handValue(dealer) > 16) {
                if (gameOver(p_val, handValue(dealer))) { // check winner or loser
                    Terminal.println("Done.")
                } else {
                    gameLoop(afterPlayer, p_card::player, dealer)
                }
            } else {
                match Deck.pullCard(afterPlayer) { // dealer draws
                    case (Some(d_card), afterDealer) => {
                        Terminal.println("Dealer => ${List.toString(d_card::dealer)}");
                        let d_val = handValue(d_card::dealer);
                        if (gameOver(p_val, d_val)) { // check winner or loser
                            Terminal.println("Done.")
                        } else {
                            gameLoop(afterDealer, p_card :: player, d_card :: dealer)
                        }
                    }
                    case (None, _) => Terminal.println("No more cards")
                }
            }
        }
        case (None, _) => Terminal.println("No more cards")
    }
}

def main(): Unit \ {NonDet, IO} =
    run {
        let deck = Deck.genDeck();
        gameLoop(deck, Nil, Nil)
    } with handler Deck {
        def genDeck(resume) = {
            // type of cards
            let cards = List#{
                Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five,
                Card.Six, Card.Seven, Card.Eight, Card.Nine, Card.Ten,
                Card.Jack, Card.Queen, Card.King
            };
            // 4 of each card per deck
            let deck = List.flatten(List.map(card -> List.repeat(4, card), cards));
            // shuffle
            resume(List.shuffle(deck))
        }
        // present the next card
        def pullCard(deck, resume) = match deck {
            case Nil => Terminal.println("No more cards"); resume((None, Nil)),
            case (card :: tail) => resume((Some(card),tail))
        }
    } with handler Terminal {
        def println(s, resume) = {println(s); resume()}
    }
