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
/// The purpose of this file is to record the transformation from Boxed values to Int64.
///
/// The interpreter works with tuples as Vectors of Int64. We therefore map all objects to
/// an Int64. For all primitive data types, this is straightforward as they all fit in an
/// Int64. For objects, we store them in a map from the Boxed object to the Int64, which the
/// interpreter can work with.
///
/// Since data can flow between relations in the interpreter, we need to somehow keep track
/// of where the different Int64's came from. Consider the following program
/// ```
/// B(1 :: Nil, 42)
/// A(x, y) :- B(x, y)
/// ```
/// The term `1 :: Nil` would map to e.g. `0`, resulting in the program
/// ```
/// B(0, 42)
/// A(x, y) :- B(x, y)
/// ```
/// At some point, `0` must be transformed back to `1 :: Nil`. For this program, `Boxing`
/// would be
/// ```
/// (
///    Vector#{MutList#{1 :: Nil}, MutList#{}},
///    Vector#{BPlusTree#{1 :: Nil -> 0}, BPlusTree#{}},
///    Vector#{Array#{Types.Object, Types.Int32}},
///    Vector#{Lock1, Lock2},
/// )
/// ```
/// We then store the fact that the type information about `A[0]` and `B[0]` are stored 
/// at `UnifiedTypePos` `0` and `A[1]` and `B[1]` are stored at position 1. We only need
/// to keep the `MutList` and `BPlusTree` for objects, as transforming the simple types 
/// requires less information.
///
mod Fixpoint3.BoxingType {
    use Fixpoint3.AtomicCounter
    use Fixpoint3.ReadWriteLock
    use Fixpoint3.Boxed
    use Fixpoint3.Ast.Ram.RamId

    ///
    /// `Types` represent the different types a program can contain.
    ///
    /// `Unknown` is a temporary placeholder for the current type. For instance, we cannot
    /// know the type of `x` in the body atom `P(x)` until we meet a value, (for instance
    /// `P(1)`)
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
    /// `typeInfo[i]` records which type of values are associated with the information stored at `i`. 
    /// If we have yet to meet any values associated with position `i`, `typeInfo[i]` will be `Unknown`.
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
