mod Test.Dec.AssociatedTypes.Mul {

    trait Mul[a] {
        pub type Right: Type
        pub type Out: Type
        pub def mul(x: a, y: Mul.Right[a]): Mul.Out[a]
    }

    instance Mul[Int32] {
        pub type Right = Int32
        pub type Out = Int32
        pub def mul(x: Int32, y: Int32): Int32 = x * y
    }

    instance Mul[String] {
        pub type Right = Int32
        pub type Out = String
        pub def mul(x: String, y: Int32): String = String.repeat(y, x)
    }

    instance Mul[Vector[Int32]] {
        pub type Right = Int32
        pub type Out = Vector[Int32]
        pub def mul(x: Vector[Int32], y: Int32): Vector[Int32] = Vector.map(z -> z * y, x)
    }

    instance Mul[Vector[Vector[Int32]]] {
        pub type Right = Int32
        pub type Out = Vector[Vector[Int32]]
        pub def mul(x: Vector[Vector[Int32]], y: Int32): Vector[Vector[Int32]] = Vector.map(Vector.map(z -> z * y), x)
    }

    instance Mul[Vector[Vector[a]]] with Mul[a] {
        pub type Right = Mul.Right[a]
        pub type Out = Vector[Vector[a]]
        pub def mul(x: Vector[Vector[a]], y: Mul.Right[a]): Vector[Vector[Mul.Out[a]]] = Vector.map(Vector.map(z -> Mul.mul(z, y)), x)
    }
}
