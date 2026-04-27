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

pub mod Json.ToJson {

    use Json.Json
    use Json.Json.{JArray, JBool, JNull, JNumber, JObject, JString}

    ///
    /// A trait for types that can be encoded as a `Json` value.
    ///
    pub trait ToJson[a] {
        pub def toJson(x: a): Json
    }

    instance ToJson[Json] {
        pub def toJson(x: Json): Json = x
    }

    instance ToJson[Unit] {
        pub def toJson(_x: Unit): Json = JNull
    }

    instance ToJson[Bool] {
        pub def toJson(x: Bool): Json = JBool(x)
    }

    instance ToJson[Char] {
        pub def toJson(x: Char): Json = JString(Char.toString(x))
    }

    instance ToJson[String] {
        pub def toJson(x: String): Json = JString(x)
    }

    instance ToJson[Int8] {
        pub def toJson(x: Int8): Json = JNumber(Int8.toBigDecimal(x))
    }

    instance ToJson[Int16] {
        pub def toJson(x: Int16): Json = JNumber(Int16.toBigDecimal(x))
    }

    instance ToJson[Int32] {
        pub def toJson(x: Int32): Json = JNumber(Int32.toBigDecimal(x))
    }

    instance ToJson[Int64] {
        pub def toJson(x: Int64): Json = JNumber(Int64.toBigDecimal(x))
    }

    instance ToJson[BigInt] {
        pub def toJson(x: BigInt): Json = JNumber(BigInt.toBigDecimal(x))
    }

    instance ToJson[BigDecimal] {
        pub def toJson(x: BigDecimal): Json = JNumber(x)
    }

    /// NaN and infinities are not representable in JSON (RFC 8259) and become `JNull`.
    instance ToJson[Float32] {
        pub def toJson(x: Float32): Json = match Float32.tryToBigDecimal(x) {
            case Some(d) => JNumber(d)
            case None    => JNull
        }
    }

    /// NaN and infinities are not representable in JSON (RFC 8259) and become `JNull`.
    instance ToJson[Float64] {
        pub def toJson(x: Float64): Json = match Float64.tryToBigDecimal(x) {
            case Some(d) => JNumber(d)
            case None    => JNull
        }
    }

    instance ToJson[Option[a]] with ToJson[a] {
        pub def toJson(x: Option[a]): Json = match x {
            case Some(v) => ToJson.toJson(v)
            case None    => JNull
        }
    }

    instance ToJson[Vector[a]] with ToJson[a] {
        pub def toJson(xs: Vector[a]): Json = JArray(Vector.map(ToJson.toJson, xs))
    }

    instance ToJson[List[a]] with ToJson[a] {
        pub def toJson(xs: List[a]): Json = JArray(List.toVector(List.map(ToJson.toJson, xs)))
    }

    /// Object keys are encoded by `ToString.toString` of the Map's key type.
    instance ToJson[Map[k, v]] with ToString[k], ToJson[v] {
        pub def toJson(m: Map[k, v]): Json =
            JObject(Map.foldLeftWithKey(
                (acc, k, v) -> Map.insert(ToString.toString(k), ToJson.toJson(v), acc),
                Map#{},
                m
            ))
    }

    instance ToJson[(a1, a2)] with ToJson[a1], ToJson[a2] {
        pub def toJson(x: (a1, a2)): Json =
            let (v1, v2) = x;
            JArray(Vector#{ToJson.toJson(v1), ToJson.toJson(v2)})
    }

    instance ToJson[(a1, a2, a3)] with ToJson[a1], ToJson[a2], ToJson[a3] {
        pub def toJson(x: (a1, a2, a3)): Json =
            let (v1, v2, v3) = x;
            JArray(Vector#{ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3)})
    }

    instance ToJson[(a1, a2, a3, a4)] with ToJson[a1], ToJson[a2], ToJson[a3], ToJson[a4] {
        pub def toJson(x: (a1, a2, a3, a4)): Json =
            let (v1, v2, v3, v4) = x;
            JArray(Vector#{ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3), ToJson.toJson(v4)})
    }

    instance ToJson[(a1, a2, a3, a4, a5)] with ToJson[a1], ToJson[a2], ToJson[a3], ToJson[a4], ToJson[a5] {
        pub def toJson(x: (a1, a2, a3, a4, a5)): Json =
            let (v1, v2, v3, v4, v5) = x;
            JArray(Vector#{
                ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3),
                ToJson.toJson(v4), ToJson.toJson(v5)
            })
    }

    instance ToJson[(a1, a2, a3, a4, a5, a6)] with ToJson[a1], ToJson[a2], ToJson[a3], ToJson[a4], ToJson[a5], ToJson[a6] {
        pub def toJson(x: (a1, a2, a3, a4, a5, a6)): Json =
            let (v1, v2, v3, v4, v5, v6) = x;
            JArray(Vector#{
                ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3),
                ToJson.toJson(v4), ToJson.toJson(v5), ToJson.toJson(v6)
            })
    }

    instance ToJson[(a1, a2, a3, a4, a5, a6, a7)] with ToJson[a1], ToJson[a2], ToJson[a3], ToJson[a4], ToJson[a5], ToJson[a6], ToJson[a7] {
        pub def toJson(x: (a1, a2, a3, a4, a5, a6, a7)): Json =
            let (v1, v2, v3, v4, v5, v6, v7) = x;
            JArray(Vector#{
                ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3),
                ToJson.toJson(v4), ToJson.toJson(v5), ToJson.toJson(v6),
                ToJson.toJson(v7)
            })
    }

    instance ToJson[(a1, a2, a3, a4, a5, a6, a7, a8)] with ToJson[a1], ToJson[a2], ToJson[a3], ToJson[a4], ToJson[a5], ToJson[a6], ToJson[a7], ToJson[a8] {
        pub def toJson(x: (a1, a2, a3, a4, a5, a6, a7, a8)): Json =
            let (v1, v2, v3, v4, v5, v6, v7, v8) = x;
            JArray(Vector#{
                ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3),
                ToJson.toJson(v4), ToJson.toJson(v5), ToJson.toJson(v6),
                ToJson.toJson(v7), ToJson.toJson(v8)
            })
    }

    instance ToJson[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with ToJson[a1], ToJson[a2], ToJson[a3], ToJson[a4], ToJson[a5], ToJson[a6], ToJson[a7], ToJson[a8], ToJson[a9] {
        pub def toJson(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Json =
            let (v1, v2, v3, v4, v5, v6, v7, v8, v9) = x;
            JArray(Vector#{
                ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3),
                ToJson.toJson(v4), ToJson.toJson(v5), ToJson.toJson(v6),
                ToJson.toJson(v7), ToJson.toJson(v8), ToJson.toJson(v9)
            })
    }

    instance ToJson[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with ToJson[a1], ToJson[a2], ToJson[a3], ToJson[a4], ToJson[a5], ToJson[a6], ToJson[a7], ToJson[a8], ToJson[a9], ToJson[a10] {
        pub def toJson(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Json =
            let (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) = x;
            JArray(Vector#{
                ToJson.toJson(v1), ToJson.toJson(v2), ToJson.toJson(v3),
                ToJson.toJson(v4), ToJson.toJson(v5), ToJson.toJson(v6),
                ToJson.toJson(v7), ToJson.toJson(v8), ToJson.toJson(v9),
                ToJson.toJson(v10)
            })
    }

}
