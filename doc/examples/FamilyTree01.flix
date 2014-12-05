namespace FamilyTree {

    var Parent:     Map[Str, Set[Str]]; // Str -> {Str}
    var Male:       Set[Str];           // {Str}
    var Female:     Set[Str];           // {Str}
    var MGP:        Map[Str, Set[Str]]; // Str -> {Str}

    // TODO: paren free syntax:
    // fact Parent "Caroline" -> {"FritsT"};

//    rule MGP x -> {z} if
//        Parent x -> {y},
//        Parent y -> {z},
//        Male {z};

    fact Parent("Caroline"  -> {"IngerM"});
    fact Parent("Caroline"  -> {"FritsT"});
    fact Parent("Bjarke"    -> {"IngerM"});
    fact Parent("Bjarke"    -> {"FritsT"});
    fact Parent("Magnus"    -> {"IngerM"});
    fact Parent("Magnus"    -> {"FritsT"});
    fact Parent("FritsT"    -> {"IngerT"});
    fact Parent("FritsT"    -> {"OrlaT"});
    fact Parent("IngerM"    -> {"GreteM"});

    fact Male({"Bjarke", "Magnus", "Frits", "OrlaT"});
    fact Female({"Caroline", "IngerM", "IngerT", "GreteM"});

    rule MGP(x -> {z}) if
        Parent(x -> {y}),
        Parent(y -> {z}),
        Male({z});

};
