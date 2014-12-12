/**
 * The purpose of this file is to showcase the syntax of Flix.
 */
namespace TheName {

    // A line comment ...
    // Another line comment ...
    // yet another

    // Look, a nested namespace:
    namespace AnotherName {

    };

    // Let us introduce some named values, i.e. constants.

    // A bool.
    val v1: Bool = ???;

    // A set.
    val v2: Set[Bool] = ???;

    // A map.
    val v3: Map[Int, Int] = ???;

    // A tuple.
    val v4: (Bool, Int, Str) = ???;

    // TODO: Add fn syntax and function type syntax and map, set, syntax.


//    var foo: x -> y -> z;
//    var foo: x -> y -> {z}; // should parse as Map (x, Map, Set(y), Set(z))
//    var foo: x -> y -> (z, w);

};

// We can add elements to existing namespaces:
namespace TheName.AnotherName {
    // ...
};
