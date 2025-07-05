import java.lang.System
import java.io.BufferedReader;
import java.io.InputStreamReader;

///
/// Player Actions.
///
enum Action with ToString {
    case Hit,
    case Stand
}

///
/// Player Action.
///
eff Play {
    def hitOrStand(): Result[String,Action]
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
def pullCard(deck: List[Card]): (Option[Card], List[Card]) \ IO = match deck {
    case Nil => println("No more cards"); ((None, Nil)),
    case (card :: tail) => (Some(card),tail)
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
def gameOver(p_val: Int32, d_val: Int32): Bool \ IO = match (p_val, d_val) {
    case (p, _) if p == 21 => println("player -> ${p}, Player wins."); true
    case (p, _) if p > 21 => println("player -> ${p}, Dealer wins."); true
    case (_, d) if d == 21 => println("dealer -> ${d}, Dealer wins."); true
    case (_, d) if d > 21 => println("dealer -> ${d}, Player wins."); true
    case (_, _) => false
}

///
/// Calculate the value of a hand.
///
def handValue(cards: List[Card]): Int32 =
    let value = List.foldLeft((x, y) -> x + y, 0, List.map(cardValue, cards));
    if (value > 21 and List.exists(x -> x == Card.Ace, cards)) value - 10 else value

///
/// Play the game until there is a winner.
///
def gameLoop(deck: List[Card], player: List[Card], dealer: List[Card], actions: List[Action]): Unit \ {IO, Play} = {
    if (List.length(player) == 0) // start of game
        // draw first 4 cards
        match deck {
            case p1::d1::p2::d2::rest =>
                println("player -> ${p2::p1::Nil}");
                println("dealer -> ${d2::d1::Nil}");
                gameLoop(rest, p1::p2::player, d1::d2::dealer, actions)
            case _ =>
                println("not enough cards")
        }
    else
        let p_value = handValue(player);
        match actions {
            case (Action.Stand :: _) => { // Player Stands
                let d_value = handValue(dealer);
                if (d_value >= 17) { // Dealer Stands
                    if (gameOver(p_value, d_value)) { // check winner or loser
                        println("Done.")
                    } else {
                        if (p_value > d_value) { // who has the better hand
                            println("dealer -> ${d_value}\nplayer -> ${p_value}\n, Player wins.")
                        } else {
                            println("dealer -> ${d_value}\nplayer -> ${p_value}\n, Dealer wins.")
                        }
                    }
                } else {
                    match pullCard(deck) { // dealer draws
                        case (Some(d_card), afterDealer) => {
                            println("Dealer => ${List.toString(d_card::dealer)}");
                            let d_val_new = handValue(d_card::dealer);
                            if (gameOver(p_value, d_val_new)) { // check winner or loser
                                println("Done.")
                            } else {
                                gameLoop(afterDealer, player, d_card :: dealer, actions)
                            }
                        }
                        case (None, _) => println("No more cards")
                    }
                }
            }
            case _ => {
                println("Hit[y] or Stand[n]:");
                match Play.hitOrStand() {
                    case Result.Ok(Action.Hit) => // player hits
                        match pullCard(deck) {
                            case (Some(p_card), afterPlayer)  => {
                                println("Player => ${List.toString(p_card::player)}");
                                println("play -> ${Action.Hit::actions}");
                                let p_val_new = handValue(p_card::player);
                                if (gameOver(p_val_new, handValue(dealer))) { // check winner or loser
                                    println("Done.")
                                } else {
                                    gameLoop(afterPlayer, p_card::player, dealer, Action.Hit :: actions)
                                }
                            }
                            case (None, _) => println("No more cards")
                        }
                    case Result.Ok(Action.Stand) => // player stands
                        println("play -> ${Action.Stand::actions}");
                        gameLoop(deck, player, dealer, Action.Stand :: actions)
                    case Result.Err(e) => println(e)
                }
            }
        }
}

def main(): Unit \ {NonDet, IO} =
    run {
        // generate new Deck for the game.
        let deck = genDeck();
        // start with no actions and no cards for Dealer or Player
        gameLoop(deck, Nil, Nil, Nil)
    } with handler Play {
        def hitOrStand(resume) =
            let reader = new BufferedReader(new InputStreamReader(System.in));
            let line = reader.readLine();
            let result  = match line { // get Player action
                case "y" => Result.Ok(Action.Hit)
                case "n" => Result.Ok(Action.Stand)
                case _ => Result.Err("bad input")
            };
            resume(result)
    }

