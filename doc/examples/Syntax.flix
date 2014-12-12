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
    val v1: Bool = true;

    // A set.
    val v2: Set[Int] = { 1, 2, 3 };

    // A map.
    val v3: Map[Int, Str] = { 1 -> "a", 2 -> "b", 3 -> "c" };

    // A tuple.
    val v4: (Bool, Int, Str) = (true, 2, "c");

    // A function.
    val v5: (Int, Int) => Int = fn(x: Int, y: Int): Int = x + y;

    // Or
    def sum(x: Int, y: Int): Int = x + y;

    // TODO: Add fn syntax and function type syntax and map, set, syntax.

//    var foo: x -> y -> z;
//    var foo: x -> y -> {z}; // should parse as Map (x, Map, Set(y), Set(z))
//    var foo: x -> y -> (z, w);

};

// We can add elements to existing namespaces:
namespace TheName.AnotherName {
    // ...
};
