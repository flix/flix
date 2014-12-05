namespace FamilyTree {

    type Person = Str;

    var Parent:     Person -> {Person};
    var Male:       {Person};
    var Female:     {Person};
    var MGP:        Person -> {Person};

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
