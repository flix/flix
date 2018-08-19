@test
def testTrueHeadPredicate(): ConstraintSet[Checkable] = true.

@test
def testFalseHeadPredicate(): ConstraintSet[Checkable] = false.

rel UnitRel(x: Unit)

@test
def testUnitRel01(): ConstraintSet[Checkable] = UnitRel(()).

rel BoolRel(x: Bool)

@test
def testBoolRel01(): ConstraintSet[Checkable] = BoolRel(true).

@test
def testBoolRel02(): ConstraintSet[Checkable] = BoolRel(false).

@test
def testBoolRel03(): ConstraintSet[Checkable] = BoolRel(x) :- BoolRel(x).

@test
def testBoolRel04(): ConstraintSet[Checkable] = {
    BoolRel(true).
    BoolRel(false).
    BoolRel(x) :- BoolRel(x).
}

rel CharRel(x: Char)

@test
def testCharRel01(): ConstraintSet[Checkable] = CharRel('a').

@test
def testCharRel02(): ConstraintSet[Checkable] = CharRel('b').

@test
def testCharRel03(): ConstraintSet[Checkable] = CharRel(x) :- CharRel(x).

@test
def testCharRel04(): ConstraintSet[Checkable] = {
    CharRel('a').
    CharRel('b').
    CharRel(x) :- CharRel(x).
}

rel Float32Rel(x: Float32)

@test
def testFloat32Rel01(): ConstraintSet[Checkable] = Float32Rel(0.0f32).

@test
def testFloat32Rel02(): ConstraintSet[Checkable] = Float32Rel(123.0f32).

@test
def testFloat32Rel03(): ConstraintSet[Checkable] = Float32Rel(x) :- Float32Rel(x).

@test
def testFloat32Rel04(): ConstraintSet[Checkable] = {
    Float32Rel(0.0f32).
    Float32Rel(123.0f32).
    Float32Rel(x) :- Float32Rel(x).
}

rel Float64Rel(x: Float64)

@test
def testFloat64Rel01(): ConstraintSet[Checkable] = Float64Rel(0.0f64).

@test
def testFloat64Rel02(): ConstraintSet[Checkable] = Float64Rel(123.0f64).

@test
def testFloat64Rel03(): ConstraintSet[Checkable] = Float64Rel(x) :- Float64Rel(x).

@test
def testFloat64Rel04(): ConstraintSet[Checkable] = {
    Float64Rel(0.0f64).
    Float64Rel(123.0f64).
    Float64Rel(x) :- Float64Rel(x).
}

rel Int8Rel(x: Int8)

@test
def testInt8Rel01(): ConstraintSet[Checkable] = Int8Rel(0i8).

@test
def testInt8Rel02(): ConstraintSet[Checkable] = Int8Rel(123i8).

@test
def testInt8Rel03(): ConstraintSet[Checkable] = Int8Rel(x) :- Int8Rel(x).

@test
def testInt8Rel04(): ConstraintSet[Checkable] = {
    Int8Rel(0i8).
    Int8Rel(123i8).
    Int8Rel(x) :- Int8Rel(x).
}

rel Int16Rel(x: Int16)

@test
def testInt16Rel01(): ConstraintSet[Checkable] = Int16Rel(0i16).

@test
def testInt16Rel02(): ConstraintSet[Checkable] = Int16Rel(123i16).

@test
def testInt16Rel03(): ConstraintSet[Checkable] = Int16Rel(x) :- Int16Rel(x).

@test
def testInt16Rel04(): ConstraintSet[Checkable] = {
    Int16Rel(0i16).
    Int16Rel(123i16).
    Int16Rel(x) :- Int16Rel(x).
}

rel Int32Rel(x: Int32)

@test
def testInt32Rel01(): ConstraintSet[Checkable] = Int32Rel(0i32).

@test
def testInt32Rel02(): ConstraintSet[Checkable] = Int32Rel(123i32).

@test
def testInt32Rel03(): ConstraintSet[Checkable] = Int32Rel(x) :- Int32Rel(x).

@test
def testInt32Rel04(): ConstraintSet[Checkable] = {
    Int32Rel(0i32).
    Int32Rel(123i32).
    Int32Rel(x) :- Int32Rel(x).
}

rel Int64Rel(x: Int64)

@test
def testInt64Rel01(): ConstraintSet[Checkable] = Int64Rel(0i64).

@test
def testInt64Rel02(): ConstraintSet[Checkable] = Int64Rel(123i64).

@test
def testInt64Rel03(): ConstraintSet[Checkable] = Int64Rel(x) :- Int64Rel(x).

@test
def testInt64Rel04(): ConstraintSet[Checkable] = {
    Int64Rel(0i64).
    Int64Rel(123i64).
    Int64Rel(x) :- Int64Rel(x).
}

rel BigIntRel(x: BigInt)

@test
def testBigIntRel01(): ConstraintSet[Checkable] = BigIntRel(0ii).

@test
def testBigIntRel02(): ConstraintSet[Checkable] = BigIntRel(123ii).

@test
def testBigIntRel03(): ConstraintSet[Checkable] = BigIntRel(x) :- BigIntRel(x).

@test
def testBigIntRel04(): ConstraintSet[Checkable] = {
    BigIntRel(0ii).
    BigIntRel(123ii).
    BigIntRel(x) :- BigIntRel(x).
}

rel StrRel(x: Str)

@test
def testStrRel01(): ConstraintSet[Checkable] = StrRel("hello").

@test
def testStrRel02(): ConstraintSet[Checkable] = StrRel("world").

@test
def testStrRel03(): ConstraintSet[Checkable] = StrRel(x) :- StrRel(x).

@test
def testStrRel04(): ConstraintSet[Checkable] = {
    StrRel("hello").
    StrRel("world").
    StrRel(x) :- StrRel(x).
}

rel TupleRel(x: (Bool, Int, Str))

@test
def testTupleRel01(): ConstraintSet[Checkable] = TupleRel((true, 21, "hello")).

@test
def testTupleRel02(): ConstraintSet[Checkable] = TupleRel((false, 42, "world")).

@test
def testTupleRel03(): ConstraintSet[Checkable] = TupleRel(x) :- TupleRel(x).

@test
def testTupleRel04(): ConstraintSet[Checkable] = {
    TupleRel((true, 21, "hello")).
    TupleRel((false, 42, "world")).
    StrRel(x) :- StrRel(x).
}

rel OptionRel(x: Option[Int])

@test
def testOptionRel01(): ConstraintSet[Checkable] = OptionRel(None).

@test
def testOptionRel02(): ConstraintSet[Checkable] = OptionRel(Some(42)).

@test
def testOptionRel03(): ConstraintSet[Checkable] = OptionRel(x) :- OptionRel(x).

@test
def testOptionRel04(): ConstraintSet[Checkable] = {
   OptionRel(None).
   OptionRel(Some(42)).
   OptionRel(x) :- OptionRel(x).
}

// TODO: Broken.
//rel ListRel(x: List[Int])
//
//@test
//def testListRel01(): ConstraintSet[Checkable] = ListRel(Nil).
//
//@test
//def testListRel02(): ConstraintSet[Checkable] = ListRel(1 :: 2 :: 3 :: Nil).
//
//@test
//def testListRel03(): ConstraintSet[Checkable] = ListRel(x) :- ListRel(x).
//
//@test
//def testListRel04(): ConstraintSet[Checkable] = {
//   ListRel(Nil).
//   ListRel(1 :: 2 :: 3 :: Nil).
//   ListRel(x) :- ListRel(x).
//}

namespace Example01 {

    enum Person {
        case Earendil,
        case Elrond,
        case Arwen
    }

    rel Parent(p1: Person, p2: Person)
    rel Ancestor(p1: Person, p2: Person)

    @test
    def example01(): ConstraintSet[Checkable] = {
        Parent(Earendil, Elrond).
        Parent(Elrond, Arwen).
        Ancestor(x, y) :- Parent(x, y).
        Ancestor(x, z) :- Ancestor(x, y), Ancestor(y, z).
    }

}

