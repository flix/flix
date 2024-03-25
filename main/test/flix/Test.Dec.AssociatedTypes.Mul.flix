mod Test.Dec.AssociatedTypes.Mul {

    ////////////////////////////////////////////////////////
    // Type Definitions                                   //
    ////////////////////////////////////////////////////////

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

    instance Mul[Vector[a]] with Mul[a] {
        pub type Right = Mul.Right[a]
        pub type Out = Vector[Mul.Out[a]]
        pub def mul(x: Vector[a], y: Mul.Right[a]): Vector[Mul.Out[a]] = Vector.map(z -> Mul.mul(z, y), x)
    }


    ////////////////////////////////////////////////////////
    // Polymorphic functions using Mul                    //
    ////////////////////////////////////////////////////////

    pub def twiceRight(f: Mul.Out[a] -> Mul.Right[a], x: a, y: Mul.Right[a]): Mul.Out[a] with Mul[a] =
        Mul.mul(x, f(Mul.mul(x, y)))

    pub def twiceLeft(f: Mul.Out[a] -> a, x: a, y: Mul.Right[a]): Mul.Out[a] with Mul[a] =
        Mul.mul(f(Mul.mul(x, y)), y)


    ////////////////////////////////////////////////////////
    // Monomorphic functions using Mul                    //
    ////////////////////////////////////////////////////////

    // Int32

    pub def twiceRightInt3201(x: Int32, y: Int32): Int32 =
        Mul.mul(x, Mul.mul(x, y))

    pub def twiceLeftInt3201(x: Int32, y: Int32): Int32 =
        Mul.mul(Mul.mul(x, y), y)

    pub def twiceRightInt3202(x: Int32, y: Int32): Int32 =
        twiceRight(identity, x, y)

    pub def twiceLeftInt3202(x: Int32, y: Int32): Int32 =
        twiceLeft(identity, x, y)


    // String

    pub def twiceRightString01(x: String, y: Int32): String =
        Mul.mul(x, String.length(Mul.mul(x, y)))

    pub def twiceLeftString01(x: String, y: Int32): String =
        Mul.mul(Mul.mul(x, y), y)

    pub def twiceRightString02(x: String, y: Int32): String =
        twiceRight(String.length, x, y)

    pub def twiceLeftString02(x: String, y: Int32): String =
        twiceLeft(identity, x, y)

    // Vector

    pub def twiceRightVector01(x: Vector[Int32], y: Int32): Vector[Int32] =
        Mul.mul(x, Vector.sum(Mul.mul(x, y)))

    pub def twiceLeftVector01(x: Vector[Int32], y: Int32): Vector[Int32] =
        Mul.mul(Mul.mul(x, y), y)

    pub def twiceRightVector02(x: Vector[Int32], y: Int32): Vector[Int32] =
        twiceRight(Vector.sum, x, y)

    pub def twiceLeftVector02(x: Vector[Int32], y: Int32): Vector[Int32] =
        twiceLeft(identity, x, y)

    pub def tensor(x: Vector[Int32], y: Vector[Int32]): Vector[Vector[Int32]] =
        Vector.map(z -> Mul.mul(y, z), x)


    ////////////////////////////////////////////////////////
    // Tests                                              //
    ////////////////////////////////////////////////////////

    @Test
    pub def testTwiceRightInt3201(): Bool =
        12 == twiceRightInt3201(2, 3)

    @Test
    pub def testTwiceLeftInt3201(): Bool =
        18 == twiceLeftInt3201(2, 3)

    @Test
    pub def testTwiceRightInt3202(): Bool =
        12 == twiceRightInt3202(2, 3)

    @Test
    pub def testTwiceLeftInt3202(): Bool =
        18 == twiceLeftInt3202(2, 3)

}
