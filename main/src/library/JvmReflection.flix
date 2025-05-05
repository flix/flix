/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

import java.lang.{Object => JObject}

pub enum JvmValue {
    case JvmChar(Char)
    case JvmBool(Bool)
    case JvmInt8(Int8)
    case JvmInt16(Int16)
    case JvmInt32(Int32)
    case JvmInt64(Int64)
    case JvmFloat32(Float32)
    case JvmFloat64(Float64)
    case JvmObject(JObject)
}

pub enum JvmType {
    case JvmChar
    case JvmBool
    case JvmInt8
    case JvmInt16
    case JvmInt32
    case JvmInt64
    case JvmFloat32
    case JvmFloat64
    case JvmObject
}
