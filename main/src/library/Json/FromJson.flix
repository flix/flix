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

pub mod Json.FromJson {

    use Json.Json
    use Json.Json.{JArray, JBool, JNull, JNumber, JObject, JString}
    use Json.JsonError.JsonError
    use Json.JsonError.JsonError.{MissingKey, TypeMismatch}

    ///
    /// A trait for types that can be decoded from a `Json` value.
    ///
    pub trait FromJson[a] {
        pub def fromJson(j: Json): Result[JsonError, a]
    }

    ///
    /// Looks up `k` in `j`, which must be a `JObject`.
    ///
    /// Returns the raw `Json` value at `k`, `MissingKey(k)` if `k` is absent,
    /// or `TypeMismatch` if `j` is not a `JObject`.
    ///
    pub def getAtKey(k: String, j: Json): Result[JsonError, Json] = match j {
        case JObject(m) => match Map.get(k, m) {
            case Some(v) => Ok(v)
            case None    => Err(MissingKey(k))
        }
        case _ => Err(TypeMismatch("JObject"))
    }

    ///
    /// Looks up `k` in `j`, which must be a `JObject`.
    ///
    /// Returns `Some(v)` if present, `None` if absent, or `TypeMismatch` if
    /// `j` is not a `JObject`.
    ///
    pub def getAtKeyOpt(k: String, j: Json): Result[JsonError, Option[Json]] = match j {
        case JObject(m) => Ok(Map.get(k, m))
        case _          => Err(TypeMismatch("JObject"))
    }

    ///
    /// Looks up `k` in `j` and decodes the value via `FromJson[a]`.
    ///
    /// Errors with `MissingKey(k)` if `k` is absent or with the inner decode
    /// error if the value is present but not decodable as `a`.
    ///
    pub def decodeAtKey(k: String, j: Json): Result[JsonError, a] with FromJson[a] =
        Result.flatMap(FromJson.fromJson, getAtKey(k, j))

    ///
    /// Looks up `k` in `j` and, if present, decodes the value via `FromJson[a]`.
    ///
    /// Returns `Ok(None)` if `k` is absent, `Ok(Some(v))` if present and
    /// decodable, or the decode error otherwise.
    ///
    pub def decodeAtKeyOpt(k: String, j: Json): Result[JsonError, Option[a]] with FromJson[a] =
        match getAtKeyOpt(k, j) {
            case Err(e)      => Err(e)
            case Ok(None)    => Ok(None)
            case Ok(Some(v)) => Result.map(x -> Some(x), FromJson.fromJson(v))
        }

    instance FromJson[Json] {
        pub def fromJson(j: Json): Result[JsonError, Json] = Ok(j)
    }

    instance FromJson[Unit] {
        pub def fromJson(j: Json): Result[JsonError, Unit] = match j {
            case JNull => Ok(())
            case _     => Err(TypeMismatch("Unit"))
        }
    }

    instance FromJson[Bool] {
        pub def fromJson(j: Json): Result[JsonError, Bool] = match j {
            case JBool(b) => Ok(b)
            case _        => Err(TypeMismatch("Bool"))
        }
    }

    instance FromJson[Char] {
        pub def fromJson(j: Json): Result[JsonError, Char] = match j {
            case JString(s) if String.length(s) == 1 => Ok(String.charAt(0, s))
            case _                                   => Err(TypeMismatch("Char"))
        }
    }

    instance FromJson[String] {
        pub def fromJson(j: Json): Result[JsonError, String] = match j {
            case JString(s) => Ok(s)
            case _          => Err(TypeMismatch("String"))
        }
    }

    /// Fractional values are truncated towards 0; out-of-range values error.
    instance FromJson[Int8] {
        pub def fromJson(j: Json): Result[JsonError, Int8] = match j {
            case JNumber(d) => match BigDecimal.tryToInt8(d) {
                case Some(i) => Ok(i)
                case None    => Err(TypeMismatch("Int8"))
            }
            case _ => Err(TypeMismatch("Int8"))
        }
    }

    /// Fractional values are truncated towards 0; out-of-range values error.
    instance FromJson[Int16] {
        pub def fromJson(j: Json): Result[JsonError, Int16] = match j {
            case JNumber(d) => match BigDecimal.tryToInt16(d) {
                case Some(i) => Ok(i)
                case None    => Err(TypeMismatch("Int16"))
            }
            case _ => Err(TypeMismatch("Int16"))
        }
    }

    /// Fractional values are truncated towards 0; out-of-range values error.
    instance FromJson[Int32] {
        pub def fromJson(j: Json): Result[JsonError, Int32] = match j {
            case JNumber(d) => match BigDecimal.tryToInt32(d) {
                case Some(i) => Ok(i)
                case None    => Err(TypeMismatch("Int32"))
            }
            case _ => Err(TypeMismatch("Int32"))
        }
    }

    /// Fractional values are truncated towards 0; out-of-range values error.
    instance FromJson[Int64] {
        pub def fromJson(j: Json): Result[JsonError, Int64] = match j {
            case JNumber(d) => match BigDecimal.tryToInt64(d) {
                case Some(i) => Ok(i)
                case None    => Err(TypeMismatch("Int64"))
            }
            case _ => Err(TypeMismatch("Int64"))
        }
    }

    /// Fractional values are truncated towards 0.
    instance FromJson[BigInt] {
        pub def fromJson(j: Json): Result[JsonError, BigInt] = match j {
            case JNumber(d) => Ok(BigDecimal.toBigInt(d))
            case _          => Err(TypeMismatch("BigInt"))
        }
    }

    instance FromJson[BigDecimal] {
        pub def fromJson(j: Json): Result[JsonError, BigDecimal] = match j {
            case JNumber(d) => Ok(d)
            case _          => Err(TypeMismatch("BigDecimal"))
        }
    }

    /// Out-of-range values error; precision loss may occur within range.
    instance FromJson[Float32] {
        pub def fromJson(j: Json): Result[JsonError, Float32] = match j {
            case JNumber(d) => match BigDecimal.tryToFloat32(d) {
                case Some(f) => Ok(f)
                case None    => Err(TypeMismatch("Float32"))
            }
            case _ => Err(TypeMismatch("Float32"))
        }
    }

    /// Out-of-range values error; precision loss may occur within range.
    instance FromJson[Float64] {
        pub def fromJson(j: Json): Result[JsonError, Float64] = match j {
            case JNumber(d) => match BigDecimal.tryToFloat64(d) {
                case Some(f) => Ok(f)
                case None    => Err(TypeMismatch("Float64"))
            }
            case _ => Err(TypeMismatch("Float64"))
        }
    }

    /// `JNull` decodes to `None`; any other value is decoded with `FromJson[a]`
    /// and wrapped in `Some`.
    instance FromJson[Option[a]] with FromJson[a] {
        pub def fromJson(j: Json): Result[JsonError, Option[a]] = match j {
            case JNull => Ok(None)
            case _     => Result.map(x -> Some(x), FromJson.fromJson(j))
        }
    }

    instance FromJson[Vector[a]] with FromJson[a] {
        pub def fromJson(j: Json): Result[JsonError, Vector[a]] = match j {
            case JArray(v) => Vector.traverse(FromJson.fromJson, v)
            case _         => Err(TypeMismatch("JArray"))
        }
    }

    instance FromJson[List[a]] with FromJson[a] {
        pub def fromJson(j: Json): Result[JsonError, List[a]] = match j {
            case JArray(v) => Result.map(Vector.toList, Vector.traverse(FromJson.fromJson, v))
            case _         => Err(TypeMismatch("JArray"))
        }
    }

    /// Object keys are decoded via `FromString[k]`; a key that fails to parse
    /// errors with `TypeMismatch`.
    instance FromJson[Map[k, v]] with FromString[k], Order[k], FromJson[v] {
        pub def fromJson(j: Json): Result[JsonError, Map[k, v]] = match j {
            case JObject(m) =>
                Map.foldLeftWithKey((acc, k, jv) -> match acc {
                    case Err(e) => Err(e)
                    case Ok(m1) => match FromString.fromString(k) {
                        case None     => Err(TypeMismatch("Map key"))
                        case Some(kp) => match FromJson.fromJson(jv) {
                            case Err(e2) => Err(e2)
                            case Ok(vp)  => Ok(Map.insert(kp, vp, m1))
                        }
                    }
                }, Ok(Map#{}), m)
            case _ => Err(TypeMismatch("JObject"))
        }
    }

    instance FromJson[(a1, a2)] with FromJson[a1], FromJson[a2] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2)] = match j {
            case JArray(v) if Vector.length(v) == 2 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v))
                ) yield (x1, x2)
            case _ => Err(TypeMismatch("JArray of length 2"))
        }
    }

    instance FromJson[(a1, a2, a3)] with FromJson[a1], FromJson[a2], FromJson[a3] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3)] = match j {
            case JArray(v) if Vector.length(v) == 3 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v));
                    x3 <- FromJson.fromJson(Vector.get(2, v))
                ) yield (x1, x2, x3)
            case _ => Err(TypeMismatch("JArray of length 3"))
        }
    }

    instance FromJson[(a1, a2, a3, a4)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4)] = match j {
            case JArray(v) if Vector.length(v) == 4 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v));
                    x3 <- FromJson.fromJson(Vector.get(2, v));
                    x4 <- FromJson.fromJson(Vector.get(3, v))
                ) yield (x1, x2, x3, x4)
            case _ => Err(TypeMismatch("JArray of length 4"))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5)] = match j {
            case JArray(v) if Vector.length(v) == 5 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v));
                    x3 <- FromJson.fromJson(Vector.get(2, v));
                    x4 <- FromJson.fromJson(Vector.get(3, v));
                    x5 <- FromJson.fromJson(Vector.get(4, v))
                ) yield (x1, x2, x3, x4, x5)
            case _ => Err(TypeMismatch("JArray of length 5"))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6)] = match j {
            case JArray(v) if Vector.length(v) == 6 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v));
                    x3 <- FromJson.fromJson(Vector.get(2, v));
                    x4 <- FromJson.fromJson(Vector.get(3, v));
                    x5 <- FromJson.fromJson(Vector.get(4, v));
                    x6 <- FromJson.fromJson(Vector.get(5, v))
                ) yield (x1, x2, x3, x4, x5, x6)
            case _ => Err(TypeMismatch("JArray of length 6"))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7)] = match j {
            case JArray(v) if Vector.length(v) == 7 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v));
                    x3 <- FromJson.fromJson(Vector.get(2, v));
                    x4 <- FromJson.fromJson(Vector.get(3, v));
                    x5 <- FromJson.fromJson(Vector.get(4, v));
                    x6 <- FromJson.fromJson(Vector.get(5, v));
                    x7 <- FromJson.fromJson(Vector.get(6, v))
                ) yield (x1, x2, x3, x4, x5, x6, x7)
            case _ => Err(TypeMismatch("JArray of length 7"))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7, a8)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7], FromJson[a8] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7, a8)] = match j {
            case JArray(v) if Vector.length(v) == 8 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v));
                    x3 <- FromJson.fromJson(Vector.get(2, v));
                    x4 <- FromJson.fromJson(Vector.get(3, v));
                    x5 <- FromJson.fromJson(Vector.get(4, v));
                    x6 <- FromJson.fromJson(Vector.get(5, v));
                    x7 <- FromJson.fromJson(Vector.get(6, v));
                    x8 <- FromJson.fromJson(Vector.get(7, v))
                ) yield (x1, x2, x3, x4, x5, x6, x7, x8)
            case _ => Err(TypeMismatch("JArray of length 8"))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7], FromJson[a8], FromJson[a9] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7, a8, a9)] = match j {
            case JArray(v) if Vector.length(v) == 9 =>
                forM (
                    x1 <- FromJson.fromJson(Vector.get(0, v));
                    x2 <- FromJson.fromJson(Vector.get(1, v));
                    x3 <- FromJson.fromJson(Vector.get(2, v));
                    x4 <- FromJson.fromJson(Vector.get(3, v));
                    x5 <- FromJson.fromJson(Vector.get(4, v));
                    x6 <- FromJson.fromJson(Vector.get(5, v));
                    x7 <- FromJson.fromJson(Vector.get(6, v));
                    x8 <- FromJson.fromJson(Vector.get(7, v));
                    x9 <- FromJson.fromJson(Vector.get(8, v))
                ) yield (x1, x2, x3, x4, x5, x6, x7, x8, x9)
            case _ => Err(TypeMismatch("JArray of length 9"))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7], FromJson[a8], FromJson[a9], FromJson[a10] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] = match j {
            case JArray(v) if Vector.length(v) == 10 =>
                forM (
                    x1  <- FromJson.fromJson(Vector.get(0, v));
                    x2  <- FromJson.fromJson(Vector.get(1, v));
                    x3  <- FromJson.fromJson(Vector.get(2, v));
                    x4  <- FromJson.fromJson(Vector.get(3, v));
                    x5  <- FromJson.fromJson(Vector.get(4, v));
                    x6  <- FromJson.fromJson(Vector.get(5, v));
                    x7  <- FromJson.fromJson(Vector.get(6, v));
                    x8  <- FromJson.fromJson(Vector.get(7, v));
                    x9  <- FromJson.fromJson(Vector.get(8, v));
                    x10 <- FromJson.fromJson(Vector.get(9, v))
                ) yield (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
            case _ => Err(TypeMismatch("JArray of length 10"))
        }
    }

}
