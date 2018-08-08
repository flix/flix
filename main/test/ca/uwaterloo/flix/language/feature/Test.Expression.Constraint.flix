@test
def testTrueHeadPredicate(): ConstraintSet = true.

@test
def testFalseHeadPredicate(): ConstraintSet = false.

rel UnitRel(x: Unit)

@test
def testUnitRel01(): ConstraintSet = UnitRel(()).

rel BoolRel(x: Bool)

@test
def testBoolRel01(): ConstraintSet = BoolRel(true).

@test
def testBoolRel02(): ConstraintSet = BoolRel(false).

@test
def testBoolRel03(): ConstraintSet = BoolRel(x) :- BoolRel(x).

@test
def testBoolRel04(): ConstraintSet = {
    BoolRel(true).
    BoolRel(false).
    BoolRel(x) :- BoolRel(x).
}

rel CharRel(x: Char)

@test
def testCharRel01(): ConstraintSet = CharRel('a').

@test
def testCharRel02(): ConstraintSet = CharRel('b').

@test
def testCharRel03(): ConstraintSet = CharRel(x) :- CharRel(x).

@test
def testCharRel04(): ConstraintSet = {
    CharRel('a').
    CharRel('b').
    CharRel(x) :- CharRel(x).
}

rel Float32Rel(x: Float32)

@test
def testFloat32Rel01(): ConstraintSet = Float32Rel(0.0f32).

@test
def testFloat32Rel02(): ConstraintSet = Float32Rel(123.0f32).

@test
def testFloat32Rel03(): ConstraintSet = Float32Rel(x) :- Float32Rel(x).

@test
def testFloat32Rel04(): ConstraintSet = {
    Float32Rel(0.0f32).
    Float32Rel(123.0f32).
    Float32Rel(x) :- Float32Rel(x).
}

rel Float64Rel(x: Float64)

@test
def testFloat64Rel01(): ConstraintSet = Float64Rel(0.0f64).

@test
def testFloat64Rel02(): ConstraintSet = Float64Rel(123.0f64).

@test
def testFloat64Rel03(): ConstraintSet = Float64Rel(x) :- Float64Rel(x).

@test
def testFloat64Rel04(): ConstraintSet = {
    Float64Rel(0.0f64).
    Float64Rel(123.0f64).
    Float64Rel(x) :- Float64Rel(x).
}

rel Int8Rel(x: Int8)

@test
def testInt8Rel01(): ConstraintSet = Int8Rel(0i8).

@test
def testInt8Rel02(): ConstraintSet = Int8Rel(123i8).

@test
def testInt8Rel03(): ConstraintSet = Int8Rel(x) :- Int8Rel(x).

@test
def testInt8Rel04(): ConstraintSet = {
    Int8Rel(0i8).
    Int8Rel(123i8).
    Int8Rel(x) :- Int8Rel(x).
}

rel Int16Rel(x: Int16)

@test
def testInt16Rel01(): ConstraintSet = Int16Rel(0i16).

@test
def testInt16Rel02(): ConstraintSet = Int16Rel(123i16).

@test
def testInt16Rel03(): ConstraintSet = Int16Rel(x) :- Int16Rel(x).

@test
def testInt16Rel04(): ConstraintSet = {
    Int16Rel(0i16).
    Int16Rel(123i16).
    Int16Rel(x) :- Int16Rel(x).
}

// TODO
//    case Type.Int32 => Set.empty
//    case Type.Int64 => Set.empty
//    case Type.BigInt => Set.empty
//    case Type.Str => Set.empty
//    case Type.Tuple(l) => Set.empty
//    case Type.Enum(_, _) => Set.empty
//    case Type.Relation(_, _) => Set.empty
//    case Type.Lattice(_, _) => Set.empty
//    case Type.ConstraintSet => Set.empty

namespace Example01 {

    enum Person {
        case Earendil,
        case Elrond,
        case Arwen
    }

    rel Parent(p1: Person, p2: Person)
    rel Ancestor(p1: Person, p2: Person)

    @test
    def example01(): ConstraintSet = {
        Parent(Earendil, Elrond).
        Parent(Elrond, Arwen).
        Ancestor(x, y) :- Parent(x, y).
        Ancestor(x, z) :- Ancestor(x, y), Ancestor(y, z).
    }

}

