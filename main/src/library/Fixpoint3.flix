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

pub mod Fixpoint3 {
    import java.lang.Object
    import java.util.concurrent.locks.{ReadWriteLock => JReadWriteLock}

    use Fixpoint3.Ast.Shared.PredSym

    ///
    /// Represents a boxed value.
    ///
    /// `NoValue` is used for relational predicates which do not map to anything and therefore
    /// contain no value.
    ///
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
            Object,
            (Object, Object) -> Comparison
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

    ///
    /// `Counter` is a wrapper around `Ref` for a simple counter.
    ///
    /// Its primary use is for generating identifiers.
    ///
    /// Not thread-safe.
    ///
    pub struct Counter[r: Region] {
        mut state: Int32
    }

    ///
    /// A trait for types that contain predicate symbols.
    ///
    pub trait PredSymsOf[t] {
        pub def predSymsOf(x: t): Set[PredSym]
    }

    ///
    /// A wrapper around a Java read-write-lock.
    ///
    enum ReadWriteLock[_: Region](JReadWriteLock)

    ///
    /// A trait for types with predicate symbols that can be substituted.
    ///
    pub trait SubstitutePredSym[t] {
        pub def substitute(x: t, s: Map[PredSym, PredSym]): t
    }

    ///
    /// The purpose of `UniqueInts` is to assign unique numbers/identifiers to elements.
    ///
    /// Example of use: When generating joins automatically we have data of type
    /// `(RelSym, Search)`. These needs to be placed somewhere in memory. For
    /// a pair `(rel, search)` calling `createIndex((rel, search))` will return its
    /// index if it has been seen before and otherwise return the new index
    /// which henceforth will be returned for subsequent queries of `(rel, search)`.
    ///
    /// `ReverseUniqueInts` allows using an index, `i`, assigned by `UniqueInts`, and get
    /// the value, which `UniqueInts` assigned index `i`.
    ///
    pub struct UniqueInts[k: Type, r: Region] {
        map: MutMap[k, Int32, r],
        counter: Counter[r]
    }

}
