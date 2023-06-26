//Example 1:


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

//Example 2:

//this is a currently dysfunctional example that was thwarted when I realized I needed to use
//typematch in order to achieve my goal. I'm keeping it here just in case we revive it for some reason
enum Container[_: Bool, _: Bool] {
  case Purse({food = String, drink = String})
}

type alias Full = true
type alias Empty = false

//you wish to store your prospective lunch (which ideally conists of both a food and a drink item)
//However, you only want one of each, as two food items or two drinks would be excessive
def addDrink(p: Container[f][Empty], newDrink: String): Container[f][Full] = match p {
  case Container.Purse(r) => Container.Purse({food = r.food, drink = newDrink})
}

def addFood(p: Container[Empty][d], newFood: String): Container[Full][d] = match p {
  case Container.Purse(r) => Container.Purse({food = newFood, drink = r.food})
}

// def completeLunch(p: Container[f][not f], newItem: String): Container[Full][Full] = match p {
//   case Container.Purse(r) =>
//     if (f) Container.Purse({food = r.food, drink = newItem})
//     else Container.Purse(food = newItem, drink = r.drink)
// }




//EXAMPLE 3:

//You wish to possess a vehicle. However, you can only afford broken cars that must
//first be fixed (assume you have an informative manual or are well-versed in automobile
//repairs) Unfortunately, every time you drive, the car breaks again and warrants a repair
//before its next use.

enum Car[_works: Bool] {
  case C({name = String, year = Int32, miles = Int32})
}

type alias Working = true
type alias Broken = false

//price is negligible because these are exceptionally cheap (and dysfunctional) cars
//returns a new car that needs to be fixed before use
def buyCar(n: String, y: Int32, d: Int32): Car[Broken] =
  Car.C({name = n, year = y, miles = d})

def repairCar(car: Car[Broken]): Car[Working] = match car {
  case Car.C(r) => Car.C(r)
}

//Note that it is a type error to call drive on a broken car
def drive(car: Car[Working], distance: Int32): Car[Broken] = match car {
  case Car.C(r) => Car.C({name = r.name, year = r.year, miles = r.miles + distance})
}




enum Input[_sanitized: Bool] {
  case Str(String)
}

type alias Sanitized = true
type alias Unsantized = false

//sanitize a string in order to ensure security
def sanitize(_: String): String = {
  ???//add sanitization code here and return intended string
}

//make a new sanitized Input from a given string
def newSanitizedInput(s: String): Input[Sanitized] = {
  Input.Str(sanitize(s))
}

//sanitize an Input(can be done on an already sanitized Input)
def sanitizeInput(input: Input[_]): Input[Sanitized] = match input {
  case Input.Str(s) => Input.Str(sanitize(s))
}

//Apply a function to a sanitized Input. This is safe because Flix's type system enforces
//that the given Input is sanitized, or else a type error transpires at compile time
def useSanitizedStr(input: Input[Sanitized], f: String -> Unit): Unit = match input {
  case Input.Str(s) => f(s)
}

