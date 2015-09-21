namespace FamilyTree {
    rel Person(name: String, mother: String, father: String)

    Person("Elizabeth", "Elizabeth Bowes-Lyon", "George").

    Person("Charles", "Elizabeth", "Philip").
    Person("Anne", "Elizabeth", "Philip").
    Person("Andrew", "Elizabeth", "Philip").
    Person("Edward", "Elizabeth", "Philip").

    Person("William", "Diana", "Charles").
    Person("Henry", "Diana", "Charles").

    Person("Charlotte", "Catherine", "William").
    Person("George", "Catherine", "William").

    Person("Beatrice", "Sarah", "Andrew").
    Person("Eugenie", "Sarah", "Andrew").

    Person("Louise", "Sophie", "Edward").
    Person("James", "Sophie", "Edward").

    rel Grandmother(person: String, grandmother: String)
    Grandmother(p, g) :- Person(p, m, _), Person(m, g, _).
    Grandmother(p, g) :- Person(p, _, f), Person(f, g, _).

    rel Ancestor(person: String, ancestor: String)
    Ancestor(p, m) :- Person(p, m, _).
    Ancestor(p, f) :- Person(p, _, f).
    Ancestor(p, m) :- Ancestor(p, a), Person(a, m, _).
    Ancestor(p, f) :- Ancestor(p, a), Person(a, _, f).

    rel OwnAncestor(x: String)
    OwnAncestor(x) :- Ancestor(x, x).

    //false :- OwnAncestor(x)
}