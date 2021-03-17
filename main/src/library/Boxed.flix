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
pub enum Boxed {
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
        (##java.lang.Object, ##java.lang.Object) -> Comparison,
        ##java.lang.Object -> String)
}

instance Eq[Boxed] {

    pub def eq(b1: Boxed, b2: Boxed): Bool = match (b1, b2) {
        case (BoxedBool(v1),            BoxedBool(v2))          => v1 == v2
        case (BoxedChar(v1),            BoxedChar(v2))          => v1 == v2
        case (BoxedInt8(v1),            BoxedInt8(v2))          => v1 == v2
        case (BoxedInt16(v1),           BoxedInt16(v2))         => v1 == v2
        case (BoxedInt32(v1),           BoxedInt32(v2))         => v1 == v2
        case (BoxedInt64(v1),           BoxedInt64(v2))         => v1 == v2
        case (BoxedFloat32(v1),         BoxedFloat32(v2))       => v1 == v2
        case (BoxedFloat64(v1),         BoxedFloat64(v2))       => v1 == v2
        case (BoxedObject(v1, cmp1, _), BoxedObject(v2, _, _))  => cmp1(v1, v2) == EqualTo
        case _ => ?bug
    }

    pub def neq(b1: Boxed, b2: Boxed): Bool = not (b1 == b2)

}

instance Order[Boxed] {

    pub def compare(b1: Boxed, b2: Boxed): Comparison = match (b1, b2) {
        case (BoxedBool(v1),            BoxedBool(v2))          => v1 <=> v2
        case (BoxedChar(v1),            BoxedChar(v2))          => v1 <=> v2
        case (BoxedInt8(v1),            BoxedInt8(v2))          => v1 <=> v2
        case (BoxedInt16(v1),           BoxedInt16(v2))         => v1 <=> v2
        case (BoxedInt32(v1),           BoxedInt32(v2))         => v1 <=> v2
        case (BoxedInt64(v1),           BoxedInt64(v2))         => v1 <=> v2
        case (BoxedFloat32(v1),         BoxedFloat32(v2))       => v1 <=> v2
        case (BoxedFloat64(v1),         BoxedFloat64(v2))       => v1 <=> v2
        case (BoxedObject(v1, cmp1, _), BoxedObject(v2, _, _))  => cmp1(v1, v2)
        case _                                                  => ?bug
    }

    pub def less(x: Boxed, y: Boxed): Bool = match (x <=> y) {
        case LessThan => true
        case _ => false
    }
    pub def lessEqual(x: Boxed, y: Boxed): Bool = match (x <=> y) {
        case LessThan => true
        case EqualTo => true
        case _ => false
    }
    pub def greater(x: Boxed, y: Boxed): Bool = match (x <=> y) {
        case GreaterThan => true
        case _ => false
    }
    pub def greaterEqual(x: Boxed, y: Boxed): Bool = match (x <=> y) {
        case GreaterThan => true
        case EqualTo => true
        case _ => false
    }

}

instance ToString[Boxed] {
    pub def toString(b: Boxed): String = match b {
        case BoxedBool(v)           => ToString.toString(v)
        case BoxedChar(v)           => ToString.toString(v)
        case BoxedInt8(v)           => ToString.toString(v)
        case BoxedInt16(v)          => ToString.toString(v)
        case BoxedInt32(v)          => ToString.toString(v)
        case BoxedInt64(v)          => ToString.toString(v)
        case BoxedFloat32(v)        => ToString.toString(v)
        case BoxedFloat64(v)        => ToString.toString(v)
        case BoxedObject(v, _, to)  => to(v)
    }
}
