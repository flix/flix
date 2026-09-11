// The compiler generates classes whose names are built from a fixed vocabulary of
// erased type names -- Bool, Char, Int8, Int16, Int32, Int64, Float32, Float64, and
// Obj -- e.g. `Tag$Obj`, `Struct$Obj`, and `RecordExtend$Obj`. Each of those is also a
// legal enum case name, and each family prefix is a legal enum name, so an enum in the
// root namespace can name its class after one of them.
//
// The enums below are declared in the root namespace on purpose: that is the namespace
// whose classes share a package with the generated ones.
//
// Each case is reached through a recursive function so that the enum survives to code
// generation. A tag folded away by the optimizer generates no class, and would make
// these tests pass without testing anything.

pub enum Tag {
    case Obj
    case Int64
    case Other
}

pub enum Tuple {
    case Obj
    case Other
}

pub enum Struct {
    case Obj
    case Other
}

pub enum ExtTag {
    case Obj
    case Other
}

pub enum RecordExtend {
    case Obj
    case Other
}

pub enum Main {
    case Obj
    case Other
}

pub enum Eff {
    case Beep
    case Other
}

pub eff Beep {
    def beep(): Unit
}

mod Test.Dec.Enum.ClassName {

    use Assert.assertEq;

    /// Returns `x` after `n` recursive steps. Opaque to the optimizer, so the tag
    /// it is applied to is not folded into a constant.
    def opaque(n: Int32, x: a): a = if (n <= 0) x else opaque(n - 1, x)

    @Test
    def testTagObj(): Unit \ Assert =
        match opaque(3, Tag.Obj) {
            case Tag.Obj   => assertEq(expected = 1, 1)
            case Tag.Int64 => assertEq(expected = 1, 2)
            case Tag.Other => assertEq(expected = 1, 3)
        }

    @Test
    def testTagInt64(): Unit \ Assert =
        match opaque(3, Tag.Int64) {
            case Tag.Obj   => assertEq(expected = 2, 1)
            case Tag.Int64 => assertEq(expected = 2, 2)
            case Tag.Other => assertEq(expected = 2, 3)
        }

    @Test
    def testTupleObj(): Unit \ Assert =
        match opaque(3, Tuple.Obj) {
            case Tuple.Obj   => assertEq(expected = 1, 1)
            case Tuple.Other => assertEq(expected = 1, 2)
        }

    @Test
    def testStructObj(): Unit \ Assert =
        match opaque(3, Struct.Obj) {
            case Struct.Obj   => assertEq(expected = 1, 1)
            case Struct.Other => assertEq(expected = 1, 2)
        }

    @Test
    def testExtTagObj(): Unit \ Assert =
        match opaque(3, ExtTag.Obj) {
            case ExtTag.Obj   => assertEq(expected = 1, 1)
            case ExtTag.Other => assertEq(expected = 1, 2)
        }

    @Test
    def testRecordExtendObj(): Unit \ Assert =
        match opaque(3, RecordExtend.Obj) {
            case RecordExtend.Obj   => assertEq(expected = 1, 1)
            case RecordExtend.Other => assertEq(expected = 1, 2)
        }

    /// A struct with a single reference-typed field generates `Struct$Obj`, which is
    /// what the case class of `Struct.Obj` would be named without the prefix.
    struct Boxed[r] {
        boxedValue: String
    }

    mod Boxed {
        use Assert.assertEq;

        @Test
        def testStructClassIsGenerated(): Unit \ Assert = region rc {
            let b = new Boxed @ rc { boxedValue = "v" };
            assertEq(expected = "v", b->boxedValue)
        }
    }

    /// An extensible variant with a single reference-typed term generates `ExtTag$Obj`,
    /// which is what the case class of `ExtTag.Obj` would be named without the prefix.
    @Test
    def testExtTagClassIsGenerated(): Unit \ Assert =
        ematch (xvar A("x")) {
            case A(s) => assertEq(expected = "x", s)
        }

    /// `Main` is reserved for modules only, so an enum may still use it.
    @Test
    def testMainObj(): Unit \ Assert =
        match opaque(3, Main.Obj) {
            case Main.Obj   => assertEq(expected = 1, 1)
            case Main.Other => assertEq(expected = 1, 2)
        }

    /// The case `Eff.Beep` and the effect `Beep` would generate the same class if the
    /// case classes were not prefixed, since the effect class is named `Eff$Beep`.
    @Test
    def testEffBeep(): Unit \ Assert =
        let result = run {
            Beep.beep();
            opaque(3, Eff.Beep)
        } with handler Beep {
            def beep(k) = k(())
        };
        match result {
            case Eff.Beep  => assertEq(expected = 1, 1)
            case Eff.Other => assertEq(expected = 1, 2)
        }

}
