/// A mutable struct that represents a person
struct Person[r] {
    name: String,
    age: Int32,
    height: Int32
}

/// Create a new `Person` struct with the specified name
def giveBirth(name: String, rc: Region[r]): Person[r] \ r =
    new Person { name = name, age = 0, height = 30 } @ rc

/// Print `person`
def displayPerson(person: Person[r]): Unit \ {r, IO} =
    let message = "${person€name} is ${person€age} years old and is ${person€height} cm tall";
    println(message)

/// Increase the age of `person` by 1 and if the age is less than 18,
/// increase its height by 10.
def birthday(person: Person[r]): Unit \ r =
    person€age = person€age + 1;
    if(person€age < 18) {
        person€height = person€height + 10
    } else {
        ()
    }

def main(): Unit \ IO = region rc {
    let joe = giveBirth("Joe", rc);
    birthday(joe);
    birthday(joe);
    birthday(joe);
    displayPerson(joe)
}
