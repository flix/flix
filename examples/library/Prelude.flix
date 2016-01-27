// TODO: Everything in here is experimental and not yet implemented.

// ops to impl:  ∀, ¬, ∧, ∨, →, ↔, ≡, ⊥, ⊤, ⊑, ⊔, ⊓, ▽, △


/**
 * A partial order is a set E equipped with a partial order function `leq`.
 *
 * Informally,
 * If `leq(x, y)` then `x` is less than or equal to `y`.
 * If `leq(x, y)` and `leq(y, x)` are both true then `x == y`.
 * Finally, if neither `leq(x, y)` and `leq(y, x)` are true then `x` and `y` are incomparable.
 *
 * A partial order must satisfy three laws:
 *   - Reflexivity
 *   - AntiSymmetry
 *   - Transitivity
 */
class PartialOrder[E] {

    /**
     * Returns `true` iff the element `x` is less than or equal to the element `y`.
     */
    fn leq(x: E, y: E): Bool

    /**
     * Alias for `leq`.
     */
    fn ⊑(x: E, y: E): Bool = leq(x, y)

    /**
     * The reflexivity law requires that any element be less than or equal to itself.
     */
    law Reflexivity(x: E): Bool = x ⊑ x

    /**
     * The anti symmetry law requires that if `x` is less than or equal to `y`
     * and vice versa then those two elements must be equal.
     */
    law AntiSymmetry(x: E, y: E): Bool = (x ⊑ y ∧ y ⊑ x) → (x ≡ y)

    /**
     * The transitivity law requires that if `x` less than or equal to `y`
     * and `y` is less than or equal to `z` then `x` must be less than or
     * equal to `z`.
     */
    law Transitivity(x: E, y: E, z: E): Bool = (x ⊑ y ∧ y ⊑ z) → (x ⊑ z)

}

/**
 * A join semi lattice is a partial order equipped with
 * a bottom element and a least upper bound operator.
 *
 * A join semi lattice must satisfy X laws:
 */
class JoinSemiLattice[E: PartialOrder] {



    // bot, lub

    law LeastElement(x: E): Bool = ⊥ ⊑ x


}

class MeetSemiLattice[E] {
    // top, glb
}

class BoundedLattice[E] {
    // height?
}



class WidenOperator[E] {

}

class NarrowOperator[E] {
    fn narrow(x: E): E

    /**
     *
     */
    law LowerBound(x: E, y: E): Bool =
}

class PreOrder[E] {

}

class NormOperator[E] {

}

// TODO: How to deal with these?
// TODO: Need some notion of function type here?
class Strict[A: JoinSemiLattice, B: JoinSemiLattice] {
    fn f(a: A): B

    // and then even this requirement cant readily be described?
    law Strict(): Bool = f(bot) == bot
}

impl Strict for (A => B) {
    // ... ?
}

//or how about this:
class Strict[A => B] where A: PartialOrder, B: PartialOrder {
    law Strict(f: A => B): Bool = f(bot) == bot
}
// this seems closer to what we want, since A => B is truly the member of this class.

instance Strict[MySpecificFun]
// the above could be rewritten with @strict


// TODO: How to deal with these?
class Monotone[E: PartialOrder] {
    fn f(e: E): E
}

// TODO: use word "impl"? for ?
// TODO: class or concept?
impl PartialOrder Sign {

    fn leq(x: Sign, y: Sign): Bool = ???

}

namespace std::ops {

    class Add[E] {
        fn add(x: E, y: E): E
    }

    impl Add for Int32 {
        fn add(x: Int32, y: Int32): Int32 = native
    }

}


impl PartialOrder for (A: PartialOrder, B: PartialOrder) {

    fn leq(x: (A, B), y: (A, B)): Bool =
        let (xa, xb) = x in
        let (ya, yb) = y in
            leq(xa, xb) && leq(ya, yb)

}

//
//idris:
//The class and instance keywords have been deprecated. The class keyword has been replaced by interface. Instances are now called implementations and are introduced by giving the interface name and parameters. For example:
//interface Eq a where
//    (==) : ty -> ty -> Bool
//    (/=) : ty -> ty -> Bool
//
//    (==) x y = not (/=) x y
//    (/=) x y = not (==) x y
//
//Eq Bool where
//    True  == True  = True
//    True  == False = False
//    False == True  = False
//    False == False = True
//
//Eq a => Eq (Maybe a) where
//    Nothing  == Nothing  = True
//    Nothing  == (Just _) = False
//    (Just _) == Nothing  = False
//    (Just a) == (Just b) = a == b
//This is as before, but without the instance keyword.
//
//The rationale for this change is that interfaces as implemented in Idris have several differences from Haskell type classes, even though they have a lot in common. They can be parameterised by things which are not types (including values and interfaces themselves). They are primarily for overloading interfaces, rather than classifying types, and hence there can be multiple implementations.
//
//New Fractional interface, with functions (/) and recip.


//// rust:
//trait HasArea {
//    fn area(&self) -> f64;
//}
//
//impl HasArea for Circle {
//    fn area(&self) -> f64 {
//        std::f64::consts::PI * (self.radius * self.radius)
//    }
//}
//impl HasArea for Square {
//    fn area(&self) -> f64 {
//        self.side * self.side
//    }
//}
//
//
//You’ve seen that you can bound a generic type parameter with a trait:
//
//fn foo<T: Clone>(x: T) {
//    x.clone();
//}
//If you need more than one bound, you can use +:
//
//use std::fmt::Debug;
//
//fn foo<T: Clone + Debug>(x: T) {
//    x.clone();
//    println!("{:?}", x);
//}
//T now needs to be both Clone as well as Debug.
//
//Where clause
//
//Writing functions with only a few generic types and a small number of trait bounds isn’t too bad, but as the number increases, the syntax gets increasingly awkward:
//
//use std::fmt::Debug;
//
//fn foo<T: Clone, K: Clone + Debug>(x: T, y: K) {
//    x.clone();
//    y.clone();
//    println!("{:?}", y);
//}
//The name of the function is on the far left, and the parameter list is on the far right. The bounds are getting in the way.
//
//Rust has a solution, and it’s called a ‘where clause’:
//
//use std::fmt::Debug;
//
//fn foo<T: Clone, K: Clone + Debug>(x: T, y: K) {
//    x.clone();
//    y.clone();
//    println!("{:?}", y);
//}
//
//fn bar<T, K>(x: T, y: K) where T: Clone, K: Clone + Debug {
//    x.clone();
//    y.clone();
//    println!("{:?}", y);
//}
//
//fn main() {
//    foo("Hello", "world");
//    bar("Hello", "world");
//}
//foo() uses the syntax we showed earlier, and bar() uses a where clause. All you need to do is leave off the bounds when defining your type parameters, and then add where after the parameter list. For longer lists, whitespace can be added:
//
//use std::fmt::Debug;
//
//fn bar<T, K>(x: T, y: K)
//    where T: Clone,
//          K: Clone + Debug {
//
//    x.clone();
//    y.clone();
//    println!("{:?}", y);
//}
//This flexibility can add clarity in complex situations.
//
//where is also more powerful than the simpler syntax. For example:
//
//trait ConvertTo<Output> {
//    fn convert(&self) -> Output;
//}
//
//impl ConvertTo<i64> for i32 {
//    fn convert(&self) -> i64 { *self as i64 }
//}
//
//// can be called with T == i32
//fn normal<T: ConvertTo<i64>>(x: &T) -> i64 {
//    x.convert()
//}
//
//// can be called with T == i64
//fn inverse<T>() -> T
//        // this is using ConvertTo as if it were "ConvertTo<i64>"
//        where i32: ConvertTo<T> {
//    42.convert()
//}
//This shows off the additional feature of where clauses: they allow bounds where the left-hand side is an arbitrary type (i32 in this case), not just a plain type parameter (like T).

//
//Inheritance
//
//Sometimes, implementing a trait requires implementing another trait:
//
//trait Foo {
//    fn foo(&self);
//}
//
//trait FooBar : Foo {
//    fn foobar(&self);
//}
//Implementors of FooBar must also implement Foo, like this:
//
//struct Baz;
//
//impl Foo for Baz {
//    fn foo(&self) { println!("foo"); }
//}
//
//impl FooBar for Baz {
//    fn foobar(&self) { println!("foobar"); }
//}
//If we forget to implement Foo, Rust will tell us:
//
//error: the trait `main::Foo` is not implemented for the type `main::Baz` [E0277]
