
///
/// The purpose of this file is to record the transformation from Boxed values to Int64.
///

mod Fixpoint3.BoxingTypes {
    use Fixpoint3.AtomicCounter
    use Fixpoint3.ReadWriteLock
    use Fixpoint3.Boxed
    use Fixpoint3.Ast.Ram.RamId

    ///
    /// `Types` represent the different types a program can contain.
    ///
    @Internal
    pub enum Types {
        case Unknown
        case Bool
        case Char
        case Int8
        case Int16
        case Int32
        case Int64
        case Float32
        case Float64
        case Object
    }

    ///
    /// Describes the placement of type-information of a group of values that share the
    /// same type.
    ///
    @Internal
    pub type alias UnifiedTypePos = Int32    

    ///
    /// Maps `RamId`'s to the `UnifiedTypePos`. It facilitates getting the type
    /// information of a `RamTerm`.
    ///
    @Internal
    pub type alias RamIdToPos = Map[RamId, UnifiedTypePos]

    /// 
    /// Describes the type of values. The `UnifiedTypePos` describes where in `TypeInfo`
    /// the type of a specific value can be found.
    /// 
    type alias TypeInfo[r: Region] = Array[Types, r]

    ///
    /// Store the type `t` of a value saved at `index` in `typeInfo`.
    ///
    @Internal
    pub def setType(t: Types, index: UnifiedTypePos, typeInfo: TypeInfo[r]): Unit \ r =
        Array.put(t, index, typeInfo)

    ///
    /// Return the type of value saved at `index` in `typeInfo`.
    ///
    @Internal
    pub def getType(index: Int32, typeInfo: TypeInfo[r]): Types \ r = 
        Array.get(index, typeInfo)

    ///
    /// `Boxing=(values, valueToRep, typeInfo, locks)` contains the information needed to
    /// convert between Boxed objects and their Int64 representation
    ///
    /// The i'th value of each Vector belongs to a specific `UnifiedTypePos`. The
    /// description of the values will be for some `i`.
    /// 
    /// `valueToRep` maps Boxed objects to their `Int64` representation.
    ///
    /// `values` contains the Boxed objects being stored.
    ///
    /// The following invariant is kept `values(valueToRep[boxedValue]) == boxedValue`. This
    /// design limits us to 2^32 objects of a given type.
    ///
    /// Furthermore for lattice `Bot` should be represented by `0`.
    ///
    /// `locks[i]` must be acquired before a specific `values[i]` can be interacted with. 
    ///
    @Internal
    pub type alias Boxing[r: Region] = (
        Vector[MutList[Boxed, r]],
        Vector[BPlusTree[Boxed, Int64, r]],
        TypeInfo[r],
        Vector[ReadWriteLock[r]]
    )
}