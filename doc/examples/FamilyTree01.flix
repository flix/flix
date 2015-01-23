namespace FamilyTree {

    type Person = Str;

    var Parent:     Person -> Set[Person];
    var Male:       Set[Person];
    var Female:     Set[Person];
    var MGP:        Person -> Set[Person];

    fact Parent "Caroline" -> {"IngerM"};
    fact Parent "Caroline" -> {"FritsT"};
    fact Parent "Bjarke"   -> {"IngerM"};
    fact Parent "Bjarke"   -> {"FritsT"};
    fact Parent "Magnus"   -> {"IngerM"};
    fact Parent "Magnus"   -> {"FritsT"};
    fact Parent "FritsT"   -> {"IngerT"};
    fact Parent "FritsT"   -> {"OrlaT"};
    fact Parent "IngerM"   -> {"GreteM"};

    fact Male   {"Bjarke", "Magnus", "Frits", "OrlaT"};
    fact Female {"Caroline", "IngerM", "IngerT", "GreteM"};

    rule MGP x -> {z} if
        Parent x -> {y},
        Parent y -> {z},
        Male {z};

};
