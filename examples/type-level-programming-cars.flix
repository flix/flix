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