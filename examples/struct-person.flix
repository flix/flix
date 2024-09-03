/// A mutable struct that represents a person
struct Person[r] {
    name: String,
    age: Int32,
    height: Int32
}

mod Person {
    /// Create a new `Person` struct with the specified name
    pub def giveBirth(name: String, rc: Region[r]): Person[r] \ r =
        new Person @ rc { name = name, age = 0, height = 30 }

    /// Increase the age of `person` by 1 and if the age is less than 18,
    /// increase their height by 10.
    pub def birthday(person: Person[r]): Unit \ r =
        person->age = person->age + 1;
        if(person->age < 18) {
            person->height = person->height + 10
        } else {
            ()
        }

    pub def printPerson(person: Person[r]): Unit \ {IO, r} =
        println("${person->name} is ${person->age} years old and is ${person->height} cm tall")
}
def main(): Unit \ IO = region rc {
    let joe = Person.giveBirth("Joe", rc);
    Person.birthday(joe);
    Person.birthday(joe);
    Person.birthday(joe);
    Person.printPerson(joe)
}
