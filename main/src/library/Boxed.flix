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
