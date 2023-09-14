mod Test.Dec.AssocType {

    class Coll[a] {
        type Elem[a]: Type

        pub def contains(x: Coll.Elem[a], c: a): Bool

        pub def head(x: a): Option[Coll.Elem[a]]
    }

    instance Coll[String] {
        type Elem[String] = Char

        pub def contains(x: Char, s: String): Bool = s |> String.toList |> List.memberOf(x)

        pub def head(x: String): Option[Char] = if (String.isEmpty(x)) None else Some(String.charAt(0, x))
    }

    pub def hasA(s: String): Bool = Coll.contains('A', s)

    pub def hasAGeneric(s: a): Bool with Coll[a] where Coll.Elem[a] ~ Char = Coll.contains('A', s)

    @Test
    def testHasA01(): Bool = hasA("ABC")

    @Test
    def testHasA02(): Bool = not hasA("XYZ")

    @Test
    def testHasAGeneric01(): Bool = hasAGeneric("ABC")

    @Test
    def testHasAGeneric02(): Bool = not hasAGeneric("XYZ")

    @Test
    def testHead01(): Bool = Coll.head("ABC") == Some('A')

    class Iterable2[a] {
        type Eff[a]: Eff
        type Elem[a]: Type

        pub def iterator(rc: Region[r], x: a): Iterator[Iterable2.Elem[a], Iterable2.Eff[a] + r, r] \ Iterable2.Eff[a] + r
    }

    instance Iterable2[String] {
        type Eff[String] = Pure
        type Elem[String] = Char

        pub def iterator(rc: Region[r], x: String): Iterator[Char, r, r] \ r = String.iterator(rc, x)
    }

    instance Iterable2[MutList[a, r]] {
        type Eff[MutList[a, r]] = r
        type Elem[MutList[a, r]] = a

        pub def iterator(rc: Region[r2], x: MutList[a, r]): Iterator[a, r + r2, r2] \ r + r2 = MutList.iterator(rc, x)
    }

    class C[a] {
        type T[a]: Type
        pub def f(x: a): C.T[a]
    }

    instance C[Unit] {
        type T[Unit] = Bool
        pub def f(_: Unit): Bool = true
    }

    @Test
    pub def testCUnit(): Bool = C.f() == true

    instance C[Bool] {
        type T[Bool] = Char
        pub def f(_: Bool): Char = 'a'
    }

    @Test
    pub def testCBool(): Bool = C.f(true) == 'a'

    instance C[Char] {
        type T[Char] = Float32
        pub def f(_: Char): Float32 = 1.0f32
    }

    @Test
    pub def testCChar(): Bool = C.f('a') == 1.0f32

    instance C[Float32] {
        type T[Float32] = Float64
        pub def f(_: Float32): Float64 = 1.0f64
    }

    @Test
    pub def testCFloat32(): Bool = C.f(1.0f32) == 1.0f64

    instance C[Float64] {
        type T[Float64] = Int8
        pub def f(_: Float64): Int8 = 1i8
    }

    @Test
    pub def testCFloat64(): Bool = C.f(1.0f64) == 1i8

    instance C[Int8] {
        type T[Int8] = Int16
        pub def f(_: Int8): Int16 = 1i16
    }

    @Test
    pub def testCInt8(): Bool = C.f(1i8) == 1i16

    instance C[Int16] {
        type T[Int16] = Int32
        pub def f(_: Int16): Int32 = 1i32
    }

    @Test
    pub def testCInt16(): Bool = C.f(1i16) == 1i32

    instance C[Int32] {
        type T[Int32] = Int64
        pub def f(_: Int32): Int64 = 1i64
    }

    @Test
    pub def testCInt32(): Bool = C.f(1i32) == 1i64

    instance C[Int64] {
        type T[Int64] = String
        pub def f(_: Int64): String = "foo"
    }

    @Test
    pub def testCInt64(): Bool = C.f(1i64) == "foo"

    instance C[String] {
        type T[String] = BigInt
        pub def f(_: String): BigInt = 123ii
    }

    @Test
    pub def testCString(): Bool = C.f("foo") == 123ii

    instance C[BigInt] {
        type T[BigInt] = BigDecimal
        pub def f(_: BigInt): BigDecimal = 123ff
    }

    @Test
    pub def testCBigInt(): Bool = C.f(123ii) == 123ff

    instance C[BigDecimal] {
        type T[BigDecimal] = Unit
        pub def f(_: BigDecimal): Unit = ()
    }

    @Test
    pub def testCBigDecimal(): Bool = C.f(123ff) == ()

    instance C[(a, b, c)] {
        type T[(a, b, c)] = (a, b)
        pub def f(x: (a, b, c)): (a, b) = match x {
            case (y, z, _) => (y, z)
        }
    }

    @Test
    pub def testCTuple3(): Bool = C.f((1, "a", 'g')) == (1, "a")

    instance C[(a, b)] {
        type T[(a, b)] = a
        pub def f(x: (a, b)): a = match x {
            case (y, _) => y
        }
    }

    @Test
    pub def testCTuple2(): Bool = C.f((1, "a")) == 1

    instance C[List[a]] {
        type T[List[a]] = Option[a]

        pub def f(x: List[a]): Option[a] = match x {
            case Nil => None
            case hd :: _ => Some(hd)
        }
    }

    @Test
    pub def testCList01(): Bool = C.f(Nil: List[Int32]) == None

    @Test
    pub def testCList02(): Bool = C.f(1 :: 2 :: Nil) == Some(1)

    instance C[Option[a]] with C[a] {
        type T[Option[a]] = Option[C.T[a]]

        pub def f(x: Option[a]): Option[C.T[a]] = match x {
            case Some(y) => Some(C.f(y))
            case None => None
        }
    }

    @Test
    pub def testCOptionList01(): Bool = C.f(Some(1 :: 2 :: Nil)) == Some(Some(1))

    @Test
    pub def testCOptionList02(): Bool = C.f(Some(Nil: List[Int32])) == Some(None)

    @Test
    pub def testCOptionList03(): Bool = C.f(None: Option[List[Int32]]) == None

    enum Option2[a] {
        case None2
        case Some2(a)
    }

    instance C[Option2[a]] with C[a] {
        type T[Option2[a]] = C.T[a]

        pub def f(x: Option2[a]): C.T[a] = match x {
            case Option2.Some2(y) => C.f(y)
            case Option2.None2 => bug!("unexpected empty option")
        }
    }

    @Test
    pub def testCOption2List01(): Bool = C.f(Option2.Some2(1 :: 2 :: Nil)) == Some(1)

    @Test
    pub def testCOption2List02(): Bool = C.f(Option2.Some2(Option2.Some2(1 :: 2 :: Nil))) == Some(1)

    @Test
    pub def testCOption2List03(): Bool = C.f(Option2.Some2(Option2.Some2(Option2.Some2(1 :: 2 :: Nil)))) == Some(1)
}
