//An interface that involves possibly infected foodstuffs.
//Note: Please follow conventionally accepted culinary practices IRL
enum Victual[_infection: Bool] {
    case F({victual_name = String, weight = Int32, disease = Option[String]})
}

type alias Infected = true
type alias Safe = false

//Creating cooked food is safe
def cookFood(s: String, w: Int32): Victual[Safe] = {
    Victual.F({victual_name = s, weight = w, disease = None})
}

//If you neglect to refrigerate a victual, it will develop norovirus
//Note that it is a type error to try and spoil an already infected victual
//(In this world, norovirus can't outcompete other pathogens)
def spoil(v: Victual[Safe]): Victual[Infected] = match v {
    case Victual.F(r) =>
      Victual.F({victual_name = r.victual_name, weight = r.weight, disease = Some("Norovirus")})
}




//A possible (albeit sinister) action is to poison clean food with pathogens of your choice!
def poison(v: Victual[Safe], d: String): Victual[Infected] = match v {
    case Victual.F(r) =>
      Victual.F({victual_name = r.victual_name, weight = r.weight, disease = Some(d)})
}

//You may wish to make a meal by mixing two existing victuals.


//helper function for nomenclature modularity
def new_name_and_weight(n1: String, n2: String, w1: Int32, w2: Int32): (String, Int32) = {
    let new_name = "Amalgamation of + ${n1} and  + ${n2}";
    let new_weight = w1 + w2;
    (new_name, new_weight)
}


//Calculate the result of combining two safe victuals
def combine_safe(v1: Victual[Safe], v2: Victual[Safe]): Victual[Safe] = match (v1, v2) {
    case (Victual.F(r1), Victual.F(r2)) =>
      let (new_name, new_weight) =
        new_name_and_weight(r1.victual_name, r2.victual_name, r1.weight, r2.weight);
      Victual.F({victual_name = new_name, weight = new_weight, disease = None})
}

//Calculate the result of combining a safe victual with a tainted one
//Note that while the two victuals may be inputted in either order, they MUST have opposing types
def combine_adulterate(v1: Victual[b], v2: Victual[not b]): Victual[Infected] = match (v1, v2) {
    case (Victual.F(r1), Victual.F(r2)) =>
      let (new_name, new_weight) =
        new_name_and_weight(r1.victual_name, r2.victual_name, r1.weight, r2.weight);
      let new_disease = match (r1.disease, r2.disease) {
        case (None, Some(d)) => Some(d)
        case (Some(d), None) => Some(d)
        case _               => None //useless case??
      };
      Victual.F({victual_name = new_name, weight = new_weight, disease = new_disease})
}

//Calculate the result of combining two unsafe victuals
def combine_unsafe(v1: Victual[Infected], v2: Victual[Infected]): Victual[Infected] =
    match (v1, v2) {
      case (Victual.F(r1), Victual.F(r2)) => {
        let (new_name, new_weight) =
          new_name_and_weight(r1.victual_name, r2.victual_name, r1.weight, r2.weight);
        let new_disease = match (r1.disease, r2.disease) {
          case (Some(d1), Some(d2)) => Some(d1 + " and " + d2)
          case _               => None //useless case??
        };
        Victual.F({victual_name = new_name, weight = new_weight, disease = new_disease})
        }
    }

//digest a victual. Logically, this is only possible when the victual is safe
//Flix's type system enforces this, preventing the possibility for indigestion
def digest(v: Victual[Safe]): Unit \ Impure = {
    match v {
      case Victual.F(r1) => {
        println("You have successfully digested ${r1.weight} grams of clean ${r1.victual_name}.")
      }
    }
}

//If you are foolish enough to ingest infected food, your gut will fail to digest it.
def poisoned(v: Victual[Infected]): Unit \ Impure = {
    match v {
      case Victual.F(r1) => {
        let d = match r1.disease {
          case Some(x) => x
          case None => ""
        };
        println("You have failed to digest ${r1.weight} grams of ${r1.victual_name}
          because it was infected with " + d)
        }
    }
}