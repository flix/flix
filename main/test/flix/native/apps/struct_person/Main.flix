//
// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
//
// Exercises:
// - region-scoped mutable structs
// - struct field access + mutation
//
def assertEqInt32(actual: Int32, expected: Int32, msg: String): Unit =
    if (actual == expected) () else bug!("${msg}: expected=${expected}, got=${actual}")

def assertTrue(cond: Bool, msg: String): Unit =
    if (cond) () else bug!(msg)

/// A mutable struct that represents a person.
struct Person[r] {
    name: String,
    mut age: Int32,
    mut height: Int32
}

mod Person {

    /// Create a new `Person` struct with the specified name.
    pub def giveBirth(name: String, rc: Region[r]): Person[r] \ r =
        new Person @ rc { name = name, age = 0i32, height = 30i32 }

    /// Increase the age of `person` by 1 and if the age is less than 18,
    /// increase their height by 10.
    pub def birthday(person: Person[r]): Unit \ r =
        person->age = person->age + 1i32;
        if (person->age < 18i32) {
            person->height = person->height + 10i32
        } else {
            ()
        }

    pub def nameOf(person: Person[r]): String = person->name

    pub def ageOf(person: Person[r]): Int32 \ r = person->age

    pub def heightOf(person: Person[r]): Int32 \ r = person->height
}

def main(): Unit \ IO = region rc {
    let joe = Person.giveBirth("Joe", rc);

    Person.birthday(joe);
    Person.birthday(joe);
    Person.birthday(joe);

    let _ = assertTrue(Person.nameOf(joe) == "Joe", "name mismatch");
    let _ = assertEqInt32(Person.ageOf(joe), 3i32, "age");
    let _ = assertEqInt32(Person.heightOf(joe), 60i32, "height");

    println("struct-person: ok");
    ()
}
