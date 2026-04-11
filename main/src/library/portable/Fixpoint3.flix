pub mod Fixpoint3 {
    use Fixpoint3.Ast.Shared.PredSym

    pub enum Boxed {
        case BoxedBool(Bool)
        case BoxedChar(Char)
        case BoxedInt8(Int8)
        case BoxedInt16(Int16)
        case BoxedInt32(Int32)
        case BoxedInt64(Int64)
        case BoxedFloat32(Float32)
        case BoxedFloat64(Float64)
        case NoValue
        case BoxedObject(
            AnyType,
            (AnyType, AnyType) -> Comparison
        )
    }

    instance Eq[Boxed] {
        pub def eq(b1: Boxed, b2: Boxed): Bool = match (b1, b2) {
            case (Boxed.BoxedBool(v1),         Boxed.BoxedBool(v2))      => v1 == v2
            case (Boxed.BoxedChar(v1),         Boxed.BoxedChar(v2))      => v1 == v2
            case (Boxed.BoxedInt8(v1),         Boxed.BoxedInt8(v2))      => v1 == v2
            case (Boxed.BoxedInt16(v1),        Boxed.BoxedInt16(v2))     => v1 == v2
            case (Boxed.BoxedInt32(v1),        Boxed.BoxedInt32(v2))     => v1 == v2
            case (Boxed.BoxedInt64(v1),        Boxed.BoxedInt64(v2))     => v1 == v2
            case (Boxed.BoxedFloat32(v1),      Boxed.BoxedFloat32(v2))   => v1 == v2
            case (Boxed.BoxedFloat64(v1),      Boxed.BoxedFloat64(v2))   => v1 == v2
            case (Boxed.BoxedObject(v1, cmp1), Boxed.BoxedObject(v2, _)) => cmp1(v1, v2) == Comparison.EqualTo
            case (Boxed.NoValue, Boxed.NoValue)                          => true
            case (Boxed.NoValue, _)                                      => false
            case (_, Boxed.NoValue)                                      => false
            case _                                                       => bug!("Mismatched types.")
        }
    }

    instance Order[Boxed] {
        pub def compare(b1: Boxed, b2: Boxed): Comparison = match (b1, b2) {
            case (Boxed.BoxedBool(v1),         Boxed.BoxedBool(v2))      => v1 <=> v2
            case (Boxed.BoxedChar(v1),         Boxed.BoxedChar(v2))      => v1 <=> v2
            case (Boxed.BoxedInt8(v1),         Boxed.BoxedInt8(v2))      => v1 <=> v2
            case (Boxed.BoxedInt16(v1),        Boxed.BoxedInt16(v2))     => v1 <=> v2
            case (Boxed.BoxedInt32(v1),        Boxed.BoxedInt32(v2))     => v1 <=> v2
            case (Boxed.BoxedInt64(v1),        Boxed.BoxedInt64(v2))     => v1 <=> v2
            case (Boxed.BoxedFloat32(v1),      Boxed.BoxedFloat32(v2))   => v1 <=> v2
            case (Boxed.BoxedFloat64(v1),      Boxed.BoxedFloat64(v2))   => v1 <=> v2
            case (Boxed.BoxedObject(v1, cmp1), Boxed.BoxedObject(v2, _)) => cmp1(v1, v2)
            case _                                                       => bug!("Mismatched types.")
        }
    }

    instance ToString[Boxed] {
        pub def toString(b: Boxed): String = match b {
            case Boxed.BoxedBool(v)      => ToString.toString(v)
            case Boxed.BoxedChar(v)      => ToString.toString(v)
            case Boxed.BoxedInt8(v)      => ToString.toString(v)
            case Boxed.BoxedInt16(v)     => ToString.toString(v)
            case Boxed.BoxedInt32(v)     => ToString.toString(v)
            case Boxed.BoxedInt64(v)     => ToString.toString(v)
            case Boxed.BoxedFloat32(v)   => ToString.toString(v)
            case Boxed.BoxedFloat64(v)   => ToString.toString(v)
            case Boxed.BoxedObject(v, _) => Debug.stringify(v)
            case Boxed.NoValue           => "Boxed.NoValue"
        }
    }

    pub struct Counter[r: Region] {
        mut state: Int32
    }

    pub trait PredSymsOf[t] {
        pub def predSymsOf(x: t): Set[PredSym]
    }

    enum ReadWriteLock[_: Region](AnyType)

    pub trait SubstitutePredSym[t] {
        pub def substitute(x: t, s: Map[PredSym, PredSym]): t
    }

    pub struct UniqueInts[k: Type, r: Region] {
        map: MutMap[k, Int32, r],
        counter: Counter[r]
    }

}
