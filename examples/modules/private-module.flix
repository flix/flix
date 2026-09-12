// A module declared without `pub` is private to the module that declares it.
//
// As in Rust, a private module is visible from its declaring module and from
// every module nested within it, siblings included. It is not visible from
// unrelated modules, and every module along a qualified path must be visible.
mod Geometry {

    // `Internal` is private to `Geometry`.
    mod Internal {
        pub def square(x: Int32): Int32 = x * x
    }

    // `Circle` is a sibling of `Internal`. Both are declared in `Geometry`,
    // so `Circle` can use `Internal`.
    mod Circle {
        pub def area(r: Int32): Int32 = 3 * Geometry.Internal.square(r)
    }

    // `Geometry` itself can use its private modules.
    pub def squareArea(s: Int32): Int32 = Internal.square(s)

    // `Circle` is not visible outside `Geometry`, so its area is exposed through this def.
    pub def circleArea(r: Int32): Int32 = Circle.area(r)

}

def main(): Unit \ IO =
    // Calling `Geometry.Internal.square(2)` here would be an error:
    // the root module lies outside `Geometry`.
    println(Geometry.squareArea(2) + Geometry.circleArea(3))
