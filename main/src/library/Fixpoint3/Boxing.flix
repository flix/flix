/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// The purpose of this file is to represent every `Boxed` value with an `Int64`.
///
/// For all primitive values the `Int64` representation is simply the raw bits,
/// possibly extended to form an `Int64`.
///
/// For `BoxedObject` some `Int64` value is computed to represent it with the
/// promise that equality is maintained with respect to `Order` on the `BoxedObject`.
///
/// Consider the following program:
/// ```
/// A(x, y) :- B(x), C(y).
/// ```
/// We know that A[0] and B[0] should share the same value, and if we project the
/// representing `Int64` from `B` to `A` then we should be able to compute the
/// `Boxed` value. We therefore unify the two, effectively declaring that the types
/// are the same. The same is done for `A[1]` and `C[0]`. On the other hand unifying
/// `A[0]` and `A[1]` is not secure, as it could mix two unrelated types.
///
/// The `initialize` method computes a unification and transforms all `Boxed` facts
/// to `Int64` and returns a `BoxingType.Boxing`.
///
/// See `BoxingType.flix` for data types and an example of how `BoxedObject`s are treated.
///
/// The module operates on `Ram.Program`, so it uses, `Eq(x, y)`, `Meet(x, y)`, `Project(_)`
/// and so on to compute the equality constraints.
///
mod Fixpoint3.Boxing {
    import java.lang.{Float, Double, Object, Character, Runtime}
    use Fixpoint3.Ast.Datalog.Datalog
    use Fixpoint3.Ast.ExecutableRam.{Facts => EFacts}
    use Fixpoint3.Ast.Ram
    use Fixpoint3.Ast.Ram.{arityOf, BoolExp, RamProgram, RamStmt, RelSym, RamTerm, RelOp, toDenotation}
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
    /// Returns a `(boxing, facts, idToBoxing)` where `Boxing` is information as described above,
    /// `facts` are the EDB of `program` as `Int64`, with respect to `boxing`.
    ///
    @Internal
    pub def initialize(rc: Region[r], withProv: Bool, program: RamProgram): (Boxing[r], EFacts[r]) \ r =
        let boxingInfo = initializeInternal(rc, program, withProv);
        let facts = unsafely IO run initializeFacts(rc, program, boxingInfo);
        (boxingInfo, facts)

    ///
    /// Returns `(facts, boxing)` where `facts` consists of the `Int64` representatives of
    /// the facts in `program`, with respect to `boxing`. Note that the old `boxingInfo` is
    /// invalid for the returned `facts`.
    ///
    def initializeFacts(rc1: Region[r], program: RamProgram, boxingInfo: Boxing[r]): EFacts[r] \ r + IO =
        let RamProgram.Program(_, facts, _, (indexes, _), typeInfo) = program;
        let (f1, f2) = facts;
        let newFacts = MutMap.empty(rc1);
        foreach ((relSym, relFacts) <- f1) {
            let RelSym.Symbol(PredSym.PredSym(_, id), _, _) = relSym;
            // Get some search. This means that we have to create one less index in the interpreter.
            let search = getOrCrash(Map.get(relSym, indexes)) |> Vector.get(0);
            let newTree = BPlusTree.emptyWithArityAndSearch(rc1, usedArity(), search);
            // `id` will always be a full `id`.
            mapAllFacts(relFacts, boxingInfo, TypeInfo.getTypeOf(id, typeInfo), newTree);
            MutMap.put(relSym, newTree, newFacts)
        };
        foreach ((relSym, relFacts) <- f2) {
            let RelSym.Symbol(PredSym.PredSym(_, id), _, _) = relSym;
            let newTree = MutMap.getOrElsePut(relSym, BPlusTree.empty(rc1), newFacts);
            // `id` will always be a full `id`.
            mapAllFacts(relFacts, boxingInfo, TypeInfo.getTypeOf(id, typeInfo), newTree)
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
                // `facts` and `newTree` must have same the same region. Cast to achieve this.
                unchecked_cast(({
                    let savedVec = tuple |>
                        Vector.mapWithIndex(index -> boxedVal ->
                            let unifiedPos = Vector.get(index, mapping);
                            unboxWith(boxedVal, unifiedPos, boxingInfo)
                        );
                    BPlusTree.put(savedVec, lat, newTree)
                }: _ \ r) as _ \ IO)}
            )
    // This is to signal that we indeed modify the region `r`.
    } as _ \ IO + r)

    ///
    /// Construct a `Boxing` from `program`.
    ///
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
        // Add bot first to make sure it corresponds to 0.
        foreach (x <- relSyms) {
            match x {
                case RelSym.Symbol(PredSym.PredSym(_, index), arity, Denotation.Latticenal(bot, _, _, _)) =>
                    discard unboxWith(bot, TypeInfo.getType(index, arity - 1, typeInfo), info)
                case _ => ()
            }
        };
        info

    ///
    /// Returns the `Boxed` value associated with `value` for the type information
    /// at `index` in `info`.
    ///
    /// Thread safe.
    ///
    def deMarshalObject(value: Int64, index: UnifiedTypePos, info: Boxing[r]): Boxed \ r = {
        let (objectList, _, _, locks) = info;
        let lock = Vector.get(index, locks);
        ReadWriteLock.readLock(lock);
        let res = getOrCrash(MutList.nth(getOrCrash(Int64.tryToInt32(value)), Vector.get(index, objectList)));
        ReadWriteLock.unlockRead(lock);
        res
    }

    ///
    /// Returns the `Int64` value associated with `box` for the type information
    /// at `index` in `info`.
    ///
    /// Thread safe.
    ///
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

    ///
    /// Return the position that the next value inserted in `list` will get.
    ///
    def getNextIndex(list: MutList[Boxed, r]): Int32 \ r = MutList.size(list)

    ///
    /// Unbox `v` given the type information stored at `typeInfo[index]` in `info`.
    /// Returns `v` unboxed.
    ///
    @Internal
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
            case BoxedFloat32(x) =>
                setType(Types.TyFloat32, index, symInfo); Int32.toInt64(Float.floatToRawIntBits(Float32.valueOf(x).floatValue()))
            case BoxedFloat64(x) =>
                setType(Types.TyFloat64, index, symInfo);
                Double.doubleToRawLongBits(Float64.valueOf(x).doubleValue())
            case BoxedObject(_, _) =>
                setType(Types.TyObject, index, symInfo);
                // We remap to low, so this only goes wrong when dealing with more
                // than 2^32 different RelSyms
                marshallObject(v, index, info)
            case Boxed.NoValue => bug!("NoValue should never be in a position to be unboxed unboxed")
        }

    ///
    /// Boxes `v` given its type information at `typeInfo[index]` in `info`.
    /// Returns `v` boxed.
    ///
    @Internal
    pub def boxWith(v: Int64, index: UnifiedTypePos, info: Boxing[r]): Boxed \ r =
        let (_, _, typeInfo, _) = info;
        match BoxingType.getType(index, typeInfo) {
            case Types.TyBool      => Boxed.BoxedBool(not(v == 0i64))
            case Types.TyChar      => BoxedChar(unchecked_cast(Array.get(0, Character.toChars(getOrCrash(Int64.tryToInt32(v)))) as _ \ {}))
            case Types.TyInt8      => BoxedInt8(getOrCrash(Int64.tryToInt8(v)))
            case Types.TyInt16     => BoxedInt16(getOrCrash(Int64.tryToInt16(v)))
            case Types.TyInt32     => BoxedInt32(getOrCrash(Int64.tryToInt32(v)))
            case Types.TyInt64     => BoxedInt64(v)
            case Types.TyFloat32   => BoxedFloat32(Float.intBitsToFloat(getOrCrash(Int64.tryToInt32(v))))
            case Types.TyFloat64   => BoxedFloat64(Double.longBitsToDouble(v))
            case Types.TyObject    => deMarshalObject(v, index, info)
            case Types.TyUnknown   => bug!("Unormalizing value, which has never been normalized")
        }

}
