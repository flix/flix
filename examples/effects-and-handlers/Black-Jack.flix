import java.lang.System
import java.io.BufferedReader;
import java.io.InputStreamReader;

/// Player Actions
enum Action with ToString {
    case Hit,
    case Stand
}

/// Player input
eff Play {
    def hitOrStand(): Result[String,Action]
}

/// Card Ranks
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

/// Deck Operations
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
    let value = List.foldLeft((x, y) -> x + y, 0, List.map(cardValue, cards));
    if (value > 21 and List.exists(x -> x == Card.Ace, cards)) value - 10 else value

def gameLoop(deck: List[Card], player: List[Card], dealer: List[Card], actions: List[Action]): Unit \ {Terminal, Deck, Play} = {
    if (List.length(player) == 0) 
        // draw first 4 cards
        match deck {
            case p1::d1::p2::d2::rest => 
                Terminal.println("player -> ${p2::p1::Nil}");
                Terminal.println("dealer -> ${d2::d1::Nil}");
                gameLoop(rest, p1::p2::player, d1::d2::dealer, actions)
            case _ =>
                Terminal.println("not enough cards")
        }
    else 
        let p_value = handValue(player);
        match actions {
            case (Action.Stand :: _) => { // player stands
                let d_value = handValue(dealer);
                if (d_value >= 17) { // dealer stands
                    if (gameOver(p_value, d_value)) { // check winner or loser
                        Terminal.println("Done.")
                    } else {
                        if (p_value > d_value) { // who has the better hand
                            Terminal.println("dealer -> ${d_value}\nplayer -> ${p_value}\n, Player wins.")
                        } else {
                            Terminal.println("dealer -> ${d_value}\nplayer -> ${p_value}\n, Dealer wins.")
                        }
                    }
                } else {
                    match Deck.pullCard(deck) { // dealer draws
                        case (Some(d_card), afterDealer) => {
                            Terminal.println("Dealer => ${List.toString(d_card::dealer)}");
                            let d_val_new = handValue(d_card::dealer);
                            if (gameOver(p_value, d_val_new)) { // check winner or loser
                                Terminal.println("Done.")
                            } else {
                                gameLoop(afterDealer, player, d_card :: dealer, actions)
                            }
                        }
                        case (None, _) => Terminal.println("No more cards")
                    }
                }
            }
            case _ => {
                Terminal.println("Hit[y] or Stand[n]:");
                match Play.hitOrStand() {
                    case Result.Ok(Action.Hit) => // player hits
                        match Deck.pullCard(deck) {
                            case (Some(p_card), afterPlayer)  => {
                                Terminal.println("Player => ${List.toString(p_card::player)}");
                                Terminal.println("play -> ${Action.Hit::actions}");
                                let p_val_new = handValue(p_card::player);
                                if (gameOver(p_val_new, handValue(dealer))) { // check winner or loser
                                    Terminal.println("Done.")
                                } else {
                                    gameLoop(afterPlayer, p_card::player, dealer, Action.Hit :: actions)
                                }
                            }
                            case (None, _) => Terminal.println("No more cards")
                        }
                    case Result.Ok(Action.Stand) => // player stands
                        Terminal.println("play -> ${Action.Stand::actions}");
                        gameLoop(deck, player, dealer, Action.Stand :: actions)
                    case Result.Err(e) => Terminal.println(e)
                }
            }
        }
}

def main(): Unit \ {NonDet, IO} =
    run {
        let deck = Deck.genDeck();
        gameLoop(deck, Nil, Nil, Nil)
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
    } with handler Play {
        def hitOrStand(resume) = 
            let reader = new BufferedReader(new InputStreamReader(System.in));
            let line = reader.readLine();
            let result  = match line {
                case "y" => Result.Ok(Action.Hit)
                case "n" => Result.Ok(Action.Stand)
                case _ => Result.Err("bad input")
            };
            resume(result)
    }
