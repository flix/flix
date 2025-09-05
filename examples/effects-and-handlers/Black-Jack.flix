///
/// Player Actions.
///
enum Action with Eq, ToString {
    case Hit,
    case Stand
}

///
/// Player Action.
///
eff Play {
    def hitOrStand(): Action
}

///
/// Card Ranks.
///
enum Card with ToString, Eq {
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

///
/// Return the numeric value of a Card.
///
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

///
/// Pop a card from the top of the Deck.
///
def pullCard(deck: List[Card]): Option[(Card, List[Card])] \ Console = match deck {
    case Nil => Console.println("No more cards"); None
    case (card :: tail) => Some((card,tail))
}

///
/// Return a randomly shuffled Deck of Cards.
///
def genDeck(): List[Card] \ NonDet = {
    // type of cards
    let cards = List#{
        Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five,
        Card.Six, Card.Seven, Card.Eight, Card.Nine, Card.Ten,
        Card.Jack, Card.Queen, Card.King
    };
    // 4 of each card per deck
    let deck = List.flatten(List.map(card -> List.repeat(4, card), cards));
    // shuffle
    List.shuffle(deck)
}

///
/// Return the result of the game if the Player or Dealer has won.
///
def gameOver(pVal: Int32, dVal: Int32): Bool \ Console = match (pVal, dVal) {
    case (p, _) if p == 21 => Console.println("player -> ${p}, Player wins."); true
    case (p, _) if p > 21 => Console.println("player -> ${p}, Dealer wins."); true
    case (_, d) if d == 21 => Console.println("dealer -> ${d}, Dealer wins."); true
    case (_, d) if d > 21 => Console.println("dealer -> ${d}, Player wins."); true
    case (_, _) => false
}

///
/// Calculate the value of a hand.
///
def handValue(cards: List[Card]): Int32 =
    let value = List.sum(List.map(cardValue, cards));
    if (value > 21 and List.exists(x -> x == Card.Ace, cards)) value - 10 else value

///
/// Play the game until there is a winner.
///
def gameLoop(deck: List[Card], player: List[Card], dealer: List[Card], actions: List[Action]): Unit \ Console = {
    run {
    if (List.length(player) == 0) // start of game
        // draw first 4 cards
        match deck {
            case p1::d1::p2::d2::rest =>
                Console.println("player -> ${p2::p1::Nil}");
                Console.println("dealer -> ${d2::d1::Nil}");
                gameLoop(rest, p1::p2::player, d1::d2::dealer, actions)
            case _ =>
                Console.println("not enough cards")
        }
    else
        let pValue = handValue(player);
        match actions {
            case (Action.Stand :: _) => { // Player Stands
                let dValue = handValue(dealer);
                if (dValue >= 17) { // Dealer Stands
                    if (gameOver(pValue, dValue)) { // check winner or loser
                        Console.println("Done.")
                    } else {
                        if (pValue > dValue) { // who has the better hand
                            Console.println("dealer -> ${dValue}\nplayer -> ${pValue}\n, Player wins.")
                        } else {
                            Console.println("dealer -> ${dValue}\nplayer -> ${pValue}\n, Dealer wins.")
                        }
                    }
                } else {
                    match pullCard(deck) { // dealer draws
                        case Some((dCard, afterDealer)) => {
                            Console.println("Dealer => ${List.toString(dCard::dealer)}");
                            let dVal_new = handValue(dCard::dealer);
                            if (gameOver(pValue, dVal_new)) { // check winner or loser
                                Console.println("Done.")
                            } else {
                                gameLoop(afterDealer, player, dCard :: dealer, actions)
                            }
                        }
                        case None => Console.println("No more cards")
                    }
                }
            }
            case _ => {
                Console.println("Hit[y] or Stand[n]:");
                match Play.hitOrStand() {
                    case Action.Hit => // player hits
                        match pullCard(deck) {
                            case Some((pCard, afterPlayer)) =>
                                Console.println("Player => ${List.toString(pCard::player)}");
                                Console.println("play -> ${Action.Hit::actions}");
                                let pVal_new = handValue(pCard::player);
                                if (gameOver(pVal_new, handValue(dealer))) { // check winner or loser
                                    Console.println("Done.")
                                } else {
                                    gameLoop(afterPlayer, pCard::player, dealer, Action.Hit :: actions)
                                }
                            case None => Console.println("No more cards")
                        }
                    case Action.Stand => // player stands
                        Console.println("play -> ${Action.Stand::actions}");
                        gameLoop(deck, player, dealer, Action.Stand :: actions)
                }
            }
        }
    } with handler Play {
        def hitOrStand(k) =  {
            k(handlePlay(Play.hitOrStand))
        }
    }
}

pub def handlePlay(f: Unit -> b \ ef): b \ {ef, Console} - Play =
    run {
        f()
    } with handler Play {
        def hitOrStand(resume) =
            let line = Console.readln();
            match line { // get Player action
                case "y" => resume(Action.Hit)
                case "n" => resume(Action.Stand)
                case _ => Console.println("wrong input..."); resume(handlePlay(Play.hitOrStand))
            }
    }

def main(): Unit \ {NonDet, IO} =
    run {
        let deck = genDeck();
        gameLoop(deck, Nil, Nil, Nil)
    } with Console.runWithIO

