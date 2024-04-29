mod Test.Dec.Assoc.Type.Mult {

    ////////////////////////////////////////////////////////
    // Type Definitions                                   //
    ////////////////////////////////////////////////////////

    trait Mult[a] {
        pub type Right: Type
        pub type Out: Type
        pub def mul(x: a, y: Mult.Right[a]): Mult.Out[a]
    }

    instance Mult[Int32] {
        pub type Right = Int32
        pub type Out = Int32
        pub def mul(x: Int32, y: Int32): Int32 = x * y
    }

    instance Mult[String] {
        pub type Right = Int32
        pub type Out = String
        pub def mul(x: String, y: Int32): String = String.repeat(y, x)
    }

    instance Mult[Vector[a]] with Mult[a] {
        pub type Right = Mult.Right[a]
        pub type Out = Vector[Mult.Out[a]]
        pub def mul(x: Vector[a], y: Mult.Right[a]): Vector[Mult.Out[a]] = Vector.map(z -> Mult.mul(z, y), x)
    }


    ////////////////////////////////////////////////////////
    // Polymorphic functions using Mult                   //
    ////////////////////////////////////////////////////////

    pub def twiceLeft(f: Mult.Out[a] -> Mult.Right[a], x: a, y: Mult.Right[a]): Mult.Out[a] with Mult[a] =
        Mult.mul(x, f(Mult.mul(x, y)))

    pub def twiceRight(f: Mult.Out[a] -> a, x: a, y: Mult.Right[a]): Mult.Out[a] with Mult[a] =
        Mult.mul(f(Mult.mul(x, y)), y)


    ////////////////////////////////////////////////////////
    // Monomorphic functions using Mult                   //
    ////////////////////////////////////////////////////////

    // Int32

    pub def twiceLeftInt3201(x: Int32, y: Int32): Int32 =
        Mult.mul(x, Mult.mul(x, y))

    pub def twiceRightInt3201(x: Int32, y: Int32): Int32 =
        Mult.mul(Mult.mul(x, y), y)

    pub def twiceLeftInt3202(x: Int32, y: Int32): Int32 =
        twiceLeft(identity, x, y)

    pub def twiceRightInt3202(x: Int32, y: Int32): Int32 =
        twiceRight(identity, x, y)


    // String

    pub def twiceLeftString01(x: String, y: Int32): String =
        Mult.mul(x, String.length(Mult.mul(x, y)))

    pub def twiceRightString01(x: String, y: Int32): String =
        Mult.mul(Mult.mul(x, y), y)

    pub def twiceLeftString02(x: String, y: Int32): String =
        twiceLeft(String.length, x, y)

    pub def twiceRightString02(x: String, y: Int32): String =
        twiceRight(identity, x, y)

    // Vector

    pub def twiceLeftVector01(x: Vector[Int32], y: Int32): Vector[Int32] =
        Mult.mul(x, Vector.sum(Mult.mul(x, y)))

    pub def twiceRightVector01(x: Vector[Int32], y: Int32): Vector[Int32] =
        Mult.mul(Mult.mul(x, y), y)

    pub def twiceLeftVector02(x: Vector[Int32], y: Int32): Vector[Int32] =
        twiceLeft(Vector.sum, x, y)

    pub def twiceRightVector02(x: Vector[Int32], y: Int32): Vector[Int32] =
        twiceRight(identity, x, y)

    pub def outerProduct(x: Vector[Int32], y: Vector[Int32]): Vector[Vector[Int32]] =
        Vector.map(z -> Mult.mul(y, z), x)


    ////////////////////////////////////////////////////////
    // Tests                                              //
    ////////////////////////////////////////////////////////


    // Int32

    @Test
    pub def testInt3201(): Bool =
        12 == twiceLeftInt3201(2, 3)

    @Test
    pub def testInt3202(): Bool =
        12 == twiceLeftInt3202(2, 3)

    @Test
    pub def testInt3203(): Bool =
        18 == twiceRightInt3201(2, 3)

    @Test
    pub def testInt3204(): Bool =
        18 == twiceRightInt3202(2, 3)


    // String

    @Test
    pub def testString01(): Bool =
        "abcabcabcabcabcabcabcabcabc" == twiceLeftString01("abc", 3)

    @Test
    pub def testString02(): Bool =
        "abcabcabcabcabcabcabcabcabc" == twiceLeftString02("abc", 3)

    @Test
    pub def testString03(): Bool =
        "abcabcabcabcabcabcabcabcabc" == twiceRightString01("abc", 3)

    @Test
    pub def testString04(): Bool =
        "abcabcabcabcabcabcabcabcabc" == twiceRightString02("abc", 3)


    // Vector

    @Test
    pub def testVector01(): Bool =
        let right = 2 * 3 + 3 * 3 + 4 * 3;
        Vector#{2 * right, 3 * right, 4 * right} == twiceLeftVector01(Vector#{2, 3, 4}, 3)

    @Test
    pub def testVector02(): Bool =
        let right = 2 * 3 + 3 * 3 + 4 * 3;
        Vector#{2 * right, 3 * right, 4 * right} == twiceLeftVector02(Vector#{2, 3, 4}, 3)

    @Test
    pub def testVector03(): Bool =
        Vector#{2 * 3 * 3, 3 * 3 * 3, 4 * 3 * 3} == twiceRightVector01(Vector#{2, 3, 4}, 3)

    @Test
    pub def testVector04(): Bool =
        Vector#{2 * 3 * 3, 3 * 3 * 3, 4 * 3 * 3} == twiceRightVector02(Vector#{2, 3, 4}, 3)

    @Test
    pub def testVector05(): Bool =
        Vector#{Vector#{2 * 5, 2 * 6}, Vector#{3 * 5, 3 * 6}, Vector#{4 * 5, 4 * 6}} == outerProduct(Vector#{2, 3, 4}, Vector#{5, 6})

}
