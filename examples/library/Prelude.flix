// TODO: Everything in here is experimental and not yet implemented.
// ∀, ¬, ∧, ∨, →, ↔, ≡, ⊥, ⊤, ⊑, ⊔, ⊓

// TODO: Use E or A for type parameters?

class PartialOrder[E] {

    /**
     * Returns `true` iff the element `x` is less than or equal to
     * the element `y` according to the partial order on E.
     */
    fn leq(x: E, y: E): Bool

    /**
     * Alias for `leq`.
     */
    fn ⊑(x: E, y: E): Bool = leq(x, y)

    /**
     * The Reflexivity law requires that any element be less than or equal to itself.
     */
    law Reflexivity(x: E): Bool = x ⊑ x

    /**
     * The AntiSymmetry law requires that if one element x is less than or equal to
     * another element y and vice versa then those two elements must infact be equal.
     *
     * NB: A pre-order or quasi-order does not satisfy the AntiSymmetry law.
     */
    law AntiSymmetry(x: E, y: E): Bool = (x ⊑ y ∧ y ⊑ x) → (x ≡ y)


    law Transitivity()


}

class PreOrder[E] {

}

class JoinSemiLattice[E] {
    // bot, lub
}

class MeetSemiLattice[E] {
    // top, glb
}

class BoundedLattice[E] {
    // height?
}

class NormOperator[E] {

}

class WidenOperator[E] {

}

class NarrowOperator[E] {

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
instance PartialOrder[Sign] {

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
