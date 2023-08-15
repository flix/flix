/*
 *  Copyright 2021 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// Represents a boxed value.
///
pub enum Boxed with Sendable {
    case BoxedBool(Bool)
    case BoxedChar(Char)
    case BoxedInt8(Int8)
    case BoxedInt16(Int16)
    case BoxedInt32(Int32)
    case BoxedInt64(Int64)
    case BoxedFloat32(Float32)
    case BoxedFloat64(Float64)
    case BoxedObject(
        ##java.lang.Object,
        (##java.lang.Object, ##java.lang.Object) -> Comparison
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
    type Effect = Pure
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
    }
}
