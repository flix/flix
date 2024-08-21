struct Person[r] {
    name: String,
    age: Int32,
    height: Int32
}

def giveBirth(name: String, rc: Region[r]): Person[r] \ r =
    new Person { name = name, age = 0, height = 30 } @ rc

def displayPerson(person: Person[r]): Unit \ {r, IO} =
    let message = person€name + " is " + ToString.toString(person€age) + " years old and is " + ToString.toString(person€height) + " cm tall";
    println(message)

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
