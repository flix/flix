pub mod Fixpoint3.Boxing {
    use Fixpoint3.Ast.Datalog.Datalog
    use Fixpoint3.Ast.ExecutableRam.{Facts => EFacts}
    use Fixpoint3.Ast.Ram
    use Fixpoint3.Ast.Ram.{arityOf, BoolExp, RamProgram, RamStmt, RamTerm, RelSym, RelOp, toDenotation}
    use Fixpoint3.Ast.Shared.{PredSym, isRelational, Denotation}
    use Fixpoint3.Boxable
    use Fixpoint3.Boxed
    use Fixpoint3.Boxed.{BoxedBool, BoxedChar, BoxedFloat32, BoxedFloat64, BoxedInt8, BoxedInt16, BoxedInt32, BoxedInt64, BoxedObject}
    use Fixpoint3.BoxingType
    use Fixpoint3.BoxingType.{Boxing, setType, Types, TypeInfo, UnifiedTypePos}
    use Fixpoint3.Options.usedArity
    use Fixpoint3.Predicate.{fullRelSymsOfProgram, relSymsOfProgram}
    use Fixpoint3.ReadWriteLock
    use Fixpoint3.TypeInfo
    use Fixpoint3.Util.getOrCrash

    ///
    /// Portable note:
    /// We represent `Float32`/`Float64` through the object map, rather than raw bit casts,
    /// to avoid relying on JVM-specific helpers while preserving full observable semantics.
    ///

    pub def initialize(rc: Region[r], withProv: Bool, program: RamProgram): (Boxing[r], EFacts[r]) \ r =
        let boxingInfo = initializeInternal(rc, program, withProv);
        let facts = unsafe IO { initializeFacts(rc, program, boxingInfo) };
        (boxingInfo, facts)

    def initializeFacts(rc1: Region[r], program: RamProgram, boxingInfo: Boxing[r]): EFacts[r] \ r + IO =
        let RamProgram.Program(_, facts, _, _, typeInfo) = program;
        let (f1, f2) = facts;
        let newFacts = MutMap.empty(rc1);
        foreach ((relSym, relFacts) <- f1) {
            let RelSym.Symbol(PredSym.PredSym(_, id), relArity, _) = relSym;
            let search = BPlusTree.search(relFacts);
            let relType = TypeInfo.getTypeOf(id, typeInfo);
            if (Vector.length(relType) == 0) {
                bug!("Fixpoint3.Boxing.initializeFacts: relation id ${id} declared arity ${relArity} has key arity ${BPlusTree.minimumKey(relFacts) |> Option.map(match (k, _) -> Vector.length(k)) |> Option.getWithDefault(-1)} but type arity 0.")
            };
            let newTree = BPlusTree.emptyWithArityAndSearch(rc1, usedArity(), search);
            mapAllFacts(relFacts, boxingInfo, relType, newTree);
            MutMap.put(relSym, newTree, newFacts)
        };
        foreach ((relSym, relFacts) <- f2) {
            let RelSym.Symbol(PredSym.PredSym(_, id), relArity, _) = relSym;
            let search = BPlusTree.search(relFacts);
            let relType = TypeInfo.getTypeOf(id, typeInfo);
            if (Vector.length(relType) == 0) {
                bug!("Fixpoint3.Boxing.initializeFacts: relation id ${id} declared arity ${relArity} has key arity ${BPlusTree.minimumKey(relFacts) |> Option.map(match (k, _) -> Vector.length(k)) |> Option.getWithDefault(-1)} but type arity 0.")
            };
            let newTree = MutMap.getOrElsePut(relSym, BPlusTree.emptyWithArityAndSearch(rc1, usedArity(), search), newFacts);
            mapAllFacts(relFacts, boxingInfo, relType, newTree)
        };
        newFacts |> MutMap.toMap

    def mapAllFacts(
        facts: BPlusTree[Vector[Boxed], Boxed, Static],
        boxingInfo: Boxing[r],
        mapping: Vector[Int32],
        newTree: BPlusTree[Vector[Int64], Boxed, r]
    ): Unit \ r + IO = unchecked_cast({
        facts |>
            BPlusTree.forEach(tuple -> lat -> {
                unchecked_cast(({
                    let savedVec = tuple |>
                        Vector.mapWithIndex(index -> boxedVal ->
                            let unifiedPos =
                                if (index < Vector.length(mapping))
                                    Vector.get(index, mapping)
                                else
                                    bug!("Fixpoint3.Boxing.mapAllFacts: tuple arity ${Vector.length(tuple)} exceeds mapping arity ${Vector.length(mapping)}.");
                            unboxWith(boxedVal, unifiedPos, boxingInfo)
                        );
                    BPlusTree.put(savedVec, lat, newTree)
                }: _ \ r) as _ \ IO)}
            )
    } as _ \ IO + r)

    def initializeInternal(rc: Region[r], program: RamProgram, withProv: Bool): Boxing[r] \ r =
        let RamProgram.Program(_, _, _, _, typeInfo) = program;
        let max = 1 + Vector.map(v -> Option.getWithDefault(0, Vector.maximum(v)), typeInfo) |> Vector.maximum |> Option.getWithDefault(-1);
        let intToBox = Vector.init(_ -> MutList.empty(rc), max);
        let boxToInt = Vector.init(_ -> BPlusTree.empty(rc), max);
        let locks = Vector.init(_ -> ReadWriteLock.mkLock(rc), max);
        let posToIndex = Array.repeat(rc, max, Types.TyUnknown);
        let info = (intToBox, boxToInt, posToIndex, locks);
        let relSyms = fullRelSymsOfProgram(program);
        if (withProv) {
            setType(Types.TyInt64, TypeInfo.provType(), posToIndex)
        } else {
            ()
        };
        foreach (x <- relSyms) {
            match x {
                case RelSym.Symbol(PredSym.PredSym(_, index), arity, Denotation.Latticenal(bot, _, _, _)) =>
                    discard unboxWith(bot, TypeInfo.getType(index, arity - 1, typeInfo), info)
                case _ => ()
            }
        };
        info

    def deMarshalObject(value: Int64, index: UnifiedTypePos, info: Boxing[r]): Boxed \ r = {
        let (objectList, _, _, locks) = info;
        let lock = Vector.get(index, locks);
        ReadWriteLock.readLock(lock);
        let res = getOrCrash(MutList.nth(getOrCrash(Int64.tryToInt32(value)), Vector.get(index, objectList)));
        ReadWriteLock.unlockRead(lock);
        res
    }

    def marshallObject(box: Boxed, index: UnifiedTypePos, info: Boxing[r]): Int64 \ r = {
        let (objectLists, vecObjectMap, _, locks) = info;
        let objectList = Vector.get(index, objectLists);
        let objectMap = Vector.get(index, vecObjectMap);
        let lock = Vector.get(index, locks);
        BPlusTree.computeIfAbsent(() -> {
            ReadWriteLock.writeLock(lock);
            let id = getNextIndex(objectList);
            MutList.insert(box, id, objectList);
            ReadWriteLock.unlockWrite(lock);
            Int32.toInt64(id)
        }, box, objectMap)
    }

    def getNextIndex(list: MutList[Boxed, r]): Int32 \ r = MutList.size(list)

    pub def unboxWith(v: Boxed, index: UnifiedTypePos, info: Boxing[r]): Int64 \ r =
        let (_, _, symInfo, _) = info;
        match v {
            case BoxedBool(x) =>
                setType(Types.TyBool, index, symInfo);
                if (x) 1i64 else 0i64
            case BoxedChar(x) =>
                setType(Types.TyChar, index, symInfo);
                Int32.toInt64(Char.toBmpCodePoint(x))
            case BoxedInt8(x) =>
                setType(Types.TyInt8, index, symInfo);
                Int8.toInt64(x)
            case BoxedInt16(x) =>
                setType(Types.TyInt16, index, symInfo);
                Int16.toInt64(x)
            case BoxedInt32(x) =>
                setType(Types.TyInt32, index, symInfo);
                Int32.toInt64(x)
            case BoxedInt64(x) =>
                setType(Types.TyInt64, index, symInfo);
                x
            case BoxedFloat32(_) =>
                setType(Types.TyFloat32, index, symInfo);
                marshallObject(v, index, info)
            case BoxedFloat64(_) =>
                setType(Types.TyFloat64, index, symInfo);
                marshallObject(v, index, info)
            case BoxedObject(_, _) =>
                setType(Types.TyObject, index, symInfo);
                marshallObject(v, index, info)
            case Boxed.NoValue =>
                bug!("NoValue should never be in a position to be unboxed")
        }

    pub def boxWith(v: Int64, index: UnifiedTypePos, info: Boxing[r]): Boxed \ r =
        let (_, _, typeInfo, _) = info;
        match BoxingType.getType(index, typeInfo) {
            case Types.TyBool      => Boxed.BoxedBool(not(v == 0i64))
            case Types.TyChar      => BoxedChar(unchecked_cast(getOrCrash(Int64.tryToInt16(v)) as Char))
            case Types.TyInt8      => BoxedInt8(getOrCrash(Int64.tryToInt8(v)))
            case Types.TyInt16     => BoxedInt16(getOrCrash(Int64.tryToInt16(v)))
            case Types.TyInt32     => BoxedInt32(getOrCrash(Int64.tryToInt32(v)))
            case Types.TyInt64     => BoxedInt64(v)
            case Types.TyFloat32   => deMarshalObject(v, index, info)
            case Types.TyFloat64   => deMarshalObject(v, index, info)
            case Types.TyObject    => deMarshalObject(v, index, info)
            case Types.TyUnknown   => bug!("Unnormalizing value, which has never been normalized")
        }

}
