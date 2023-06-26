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
