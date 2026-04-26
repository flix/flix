/*
 * Copyright 2026 Flix Authors
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

pub mod Json {

    ///
    /// Represents a JSON value.
    ///
    /// JSON numbers are stored as `BigDecimal` for lossless round-trip;
    /// JSON objects are stored as `Map[String, Json]`, which gives
    /// canonical (sorted) key order on output.
    ///
    pub enum Json with Eq, ToString {
        case JArray(Vector[Json])
        case JBool(Bool)
        case JNull
        case JNumber(BigDecimal)
        case JObject(Map[String, Json])
        case JString(String)
    }

}
