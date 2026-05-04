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
    use Json.JsonError.JsonError.Decode
    use Json.JsonError.prependStep
    use Json.JsonError.DecodeErrorKind.{MissingKey, TypeMismatch}
    use Json.JsonPath.JsonPath
    use Json.JsonPath.JsonStep

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
    /// or `TypeMismatch` if `j` is not a `JObject`. Both errors carry the
    /// empty path (the failure is at the level of `j` itself).
    ///
    pub def getAtKey(k: String, j: Json): Result[JsonError, Json] = match j {
        case JObject(m) => match Map.get(k, m) {
            case Some(v) => Ok(v)
            case None    => Err(missingKey(k))
        }
        case _ => Err(typeMismatch("JObject"))
    }

    ///
    /// Looks up `k` in `j`, which must be a `JObject`.
    ///
    /// Returns `Some(v)` if present, `None` if absent, or `TypeMismatch` if
    /// `j` is not a `JObject`.
    ///
    pub def getAtKeyOpt(k: String, j: Json): Result[JsonError, Option[Json]] = match j {
        case JObject(m) => Ok(Map.get(k, m))
        case _          => Err(typeMismatch("JObject"))
    }

    ///
    /// Looks up `k` in `j` and decodes the value via `FromJson[a]`.
    ///
    /// If `k` is absent, errors with `MissingKey(k)` at the empty path. If `k`
    /// is present but the value fails to decode, the inner error's path is
    /// prepended with `Key(k)`.
    ///
    pub def decodeAtKey(k: String, j: Json): Result[JsonError, a] with FromJson[a] =
        match getAtKey(k, j) {
            case Err(e) => Err(e)
            case Ok(v)  => match FromJson.fromJson(v) {
                case Err(e) => Err(prependStep(JsonStep.Key(k), e))
                case Ok(x)  => Ok(x)
            }
        }

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
            case Ok(Some(v)) => match FromJson.fromJson(v) {
                case Err(e) => Err(prependStep(JsonStep.Key(k), e))
                case Ok(x)  => Ok(Some(x))
            }
        }

    ///
    /// Looks up index `i` in `j`, which must be a `JArray`, and decodes the
    /// element via `FromJson[a]`.
    ///
    /// If `i` is out of range, errors with `TypeMismatch` at the empty path.
    /// If the element is present but fails to decode, the inner error's path
    /// is prepended with `Index(i)`.
    ///
    pub def decodeAtIndex(i: Int32, j: Json): Result[JsonError, a] with FromJson[a] = match j {
        case JArray(v) =>
            if (i < 0 or i >= Vector.length(v))
                Err(typeMismatch("JArray index ${i}"))
            else match FromJson.fromJson(Vector.get(i, v)) {
                case Err(e) => Err(prependStep(JsonStep.Index(i), e))
                case Ok(x)  => Ok(x)
            }
        case _ => Err(typeMismatch("JArray"))
    }

    ///
    /// Constructs a `Decode` error with `TypeMismatch(d)` at the empty path.
    ///
    def typeMismatch(d: String): JsonError = Decode(Json.JsonPath.empty(), TypeMismatch(d))

    ///
    /// Constructs a `Decode` error with `MissingKey(k)` at the empty path.
    ///
    def missingKey(k: String): JsonError = Decode(Json.JsonPath.empty(), MissingKey(k))

    ///
    /// Returns `true` if `d` is integer-valued — i.e. it has no fractional part.
    ///
    /// Used by the integer `FromJson` instances to accept JSON numbers like
    /// `42` and `42.0` while rejecting `42.5`.
    ///
    def isIntegerValued(d: BigDecimal): Bool = BigDecimal.floor(d) == d

    ///
    /// Decodes the elements of `v` via `FromJson[a]`, prepending `Index(i)` to
    /// any inner error path. Stops on the first failure.
    ///
    def traverseIndexed(v: Vector[Json]): Result[JsonError, Vector[a]] with FromJson[a] =
        Vector.traverse(
            match (i, x) -> Result.mapErr(prependStep(JsonStep.Index(i)), FromJson.fromJson(x)),
            Vector.mapWithIndex((i, x) -> (i, x), v))

    instance FromJson[Json] {
        pub def fromJson(j: Json): Result[JsonError, Json] = Ok(j)
    }

    instance FromJson[Unit] {
        pub def fromJson(j: Json): Result[JsonError, Unit] = match j {
            case JNull => Ok(())
            case _     => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Unit")))
        }
    }

    instance FromJson[Bool] {
        pub def fromJson(j: Json): Result[JsonError, Bool] = match j {
            case JBool(b) => Ok(b)
            case _        => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Bool")))
        }
    }

    // No `FromJson[Char]`: `Char` is a single UTF-16 code unit (BMP only), but JSON
    // strings carry full Unicode. Use `String` for JSON-bound character data.

    instance FromJson[String] {
        pub def fromJson(j: Json): Result[JsonError, String] = match j {
            case JString(s) => Ok(s)
            case _          => Err(Decode(Json.JsonPath.empty(), TypeMismatch("String")))
        }
    }

    /// Integer-valued JSON numbers (e.g. `42`, `42.0`) decode; genuine fractions
    /// (e.g. `42.5`) and out-of-range values error.
    instance FromJson[Int8] {
        pub def fromJson(j: Json): Result[JsonError, Int8] = match j {
            case JNumber(d) if isIntegerValued(d) => match BigDecimal.tryToInt8(d) {
                case Some(i) => Ok(i)
                case None    => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int8")))
            }
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int8")))
        }
    }

    /// Integer-valued JSON numbers (e.g. `42`, `42.0`) decode; genuine fractions
    /// (e.g. `42.5`) and out-of-range values error.
    instance FromJson[Int16] {
        pub def fromJson(j: Json): Result[JsonError, Int16] = match j {
            case JNumber(d) if isIntegerValued(d) => match BigDecimal.tryToInt16(d) {
                case Some(i) => Ok(i)
                case None    => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int16")))
            }
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int16")))
        }
    }

    /// Integer-valued JSON numbers (e.g. `42`, `42.0`) decode; genuine fractions
    /// (e.g. `42.5`) and out-of-range values error.
    instance FromJson[Int32] {
        pub def fromJson(j: Json): Result[JsonError, Int32] = match j {
            case JNumber(d) if isIntegerValued(d) => match BigDecimal.tryToInt32(d) {
                case Some(i) => Ok(i)
                case None    => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int32")))
            }
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int32")))
        }
    }

    /// Integer-valued JSON numbers (e.g. `42`, `42.0`) decode; genuine fractions
    /// (e.g. `42.5`) and out-of-range values error.
    instance FromJson[Int64] {
        pub def fromJson(j: Json): Result[JsonError, Int64] = match j {
            case JNumber(d) if isIntegerValued(d) => match BigDecimal.tryToInt64(d) {
                case Some(i) => Ok(i)
                case None    => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int64")))
            }
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Int64")))
        }
    }

    /// Integer-valued JSON numbers (e.g. `42`, `42.0`) decode; genuine fractions
    /// (e.g. `42.5`) error.
    instance FromJson[BigInt] {
        pub def fromJson(j: Json): Result[JsonError, BigInt] = match j {
            case JNumber(d) if isIntegerValued(d) => Ok(BigDecimal.toBigInt(d))
            case _                                => Err(Decode(Json.JsonPath.empty(), TypeMismatch("BigInt")))
        }
    }

    instance FromJson[BigDecimal] {
        pub def fromJson(j: Json): Result[JsonError, BigDecimal] = match j {
            case JNumber(d) => Ok(d)
            case _          => Err(Decode(Json.JsonPath.empty(), TypeMismatch("BigDecimal")))
        }
    }

    /// Out-of-range values error; precision loss may occur within range.
    instance FromJson[Float32] {
        pub def fromJson(j: Json): Result[JsonError, Float32] = match j {
            case JNumber(d) => match BigDecimal.tryToFloat32(d) {
                case Some(f) => Ok(f)
                case None    => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Float32")))
            }
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Float32")))
        }
    }

    /// Out-of-range values error; precision loss may occur within range.
    instance FromJson[Float64] {
        pub def fromJson(j: Json): Result[JsonError, Float64] = match j {
            case JNumber(d) => match BigDecimal.tryToFloat64(d) {
                case Some(f) => Ok(f)
                case None    => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Float64")))
            }
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("Float64")))
        }
    }

    /// `JNull` decodes to `None`; any other value is decoded with `FromJson[a]`
    /// and wrapped in `Some`. The path is left unchanged — `Option` does not
    /// introduce a structural step.
    instance FromJson[Option[a]] with FromJson[a] {
        pub def fromJson(j: Json): Result[JsonError, Option[a]] = match j {
            case JNull => Ok(None)
            case _     => Result.map(x -> Some(x), FromJson.fromJson(j))
        }
    }

    instance FromJson[Vector[a]] with FromJson[a] {
        pub def fromJson(j: Json): Result[JsonError, Vector[a]] = match j {
            case JArray(v) => traverseIndexed(v)
            case _         => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray")))
        }
    }

    instance FromJson[List[a]] with FromJson[a] {
        pub def fromJson(j: Json): Result[JsonError, List[a]] = match j {
            case JArray(v) => Result.map(Vector.toList, traverseIndexed(v))
            case _         => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray")))
        }
    }

    /// Object keys are decoded via `FromString[k]`; a key that fails to parse
    /// errors with `TypeMismatch("Map key")` at path `Key(stringKey)`. Value
    /// decode failures carry path `Key(stringKey)` followed by the inner path.
    instance FromJson[Map[k, v]] with FromString[k], Order[k], FromJson[v] {
        pub def fromJson(j: Json): Result[JsonError, Map[k, v]] = match j {
            case JObject(m) =>
                Map.foldLeftWithKey((acc, k, jv) -> match acc {
                    case Err(e) => Err(e)
                    case Ok(m1) => match FromString.fromString(k) {
                        case None     => Err(Decode(Json.JsonPath.fromList(JsonStep.Key(k) :: Nil), TypeMismatch("Map key")))
                        case Some(kp) => match FromJson.fromJson(jv) {
                            case Err(e2) => Err(prependStep(JsonStep.Key(k), e2))
                            case Ok(vp)  => Ok(Map.insert(kp, vp, m1))
                        }
                    }
                }, Ok(Map#{}), m)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JObject")))
        }
    }

    instance FromJson[(a1, a2)] with FromJson[a1], FromJson[a2] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2)] = match j {
            case JArray(v) if Vector.length(v) == 2 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)))
                ) yield (x1, x2)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 2")))
        }
    }

    instance FromJson[(a1, a2, a3)] with FromJson[a1], FromJson[a2], FromJson[a3] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3)] = match j {
            case JArray(v) if Vector.length(v) == 3 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3 <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)))
                ) yield (x1, x2, x3)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 3")))
        }
    }

    instance FromJson[(a1, a2, a3, a4)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4)] = match j {
            case JArray(v) if Vector.length(v) == 4 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3 <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)));
                    x4 <- Result.mapErr(prependStep(JsonStep.Index(3)), FromJson.fromJson(Vector.get(3, v)))
                ) yield (x1, x2, x3, x4)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 4")))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5)] = match j {
            case JArray(v) if Vector.length(v) == 5 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3 <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)));
                    x4 <- Result.mapErr(prependStep(JsonStep.Index(3)), FromJson.fromJson(Vector.get(3, v)));
                    x5 <- Result.mapErr(prependStep(JsonStep.Index(4)), FromJson.fromJson(Vector.get(4, v)))
                ) yield (x1, x2, x3, x4, x5)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 5")))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6)] = match j {
            case JArray(v) if Vector.length(v) == 6 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3 <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)));
                    x4 <- Result.mapErr(prependStep(JsonStep.Index(3)), FromJson.fromJson(Vector.get(3, v)));
                    x5 <- Result.mapErr(prependStep(JsonStep.Index(4)), FromJson.fromJson(Vector.get(4, v)));
                    x6 <- Result.mapErr(prependStep(JsonStep.Index(5)), FromJson.fromJson(Vector.get(5, v)))
                ) yield (x1, x2, x3, x4, x5, x6)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 6")))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7)] = match j {
            case JArray(v) if Vector.length(v) == 7 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3 <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)));
                    x4 <- Result.mapErr(prependStep(JsonStep.Index(3)), FromJson.fromJson(Vector.get(3, v)));
                    x5 <- Result.mapErr(prependStep(JsonStep.Index(4)), FromJson.fromJson(Vector.get(4, v)));
                    x6 <- Result.mapErr(prependStep(JsonStep.Index(5)), FromJson.fromJson(Vector.get(5, v)));
                    x7 <- Result.mapErr(prependStep(JsonStep.Index(6)), FromJson.fromJson(Vector.get(6, v)))
                ) yield (x1, x2, x3, x4, x5, x6, x7)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 7")))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7, a8)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7], FromJson[a8] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7, a8)] = match j {
            case JArray(v) if Vector.length(v) == 8 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3 <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)));
                    x4 <- Result.mapErr(prependStep(JsonStep.Index(3)), FromJson.fromJson(Vector.get(3, v)));
                    x5 <- Result.mapErr(prependStep(JsonStep.Index(4)), FromJson.fromJson(Vector.get(4, v)));
                    x6 <- Result.mapErr(prependStep(JsonStep.Index(5)), FromJson.fromJson(Vector.get(5, v)));
                    x7 <- Result.mapErr(prependStep(JsonStep.Index(6)), FromJson.fromJson(Vector.get(6, v)));
                    x8 <- Result.mapErr(prependStep(JsonStep.Index(7)), FromJson.fromJson(Vector.get(7, v)))
                ) yield (x1, x2, x3, x4, x5, x6, x7, x8)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 8")))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7], FromJson[a8], FromJson[a9] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7, a8, a9)] = match j {
            case JArray(v) if Vector.length(v) == 9 =>
                forM (
                    x1 <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2 <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3 <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)));
                    x4 <- Result.mapErr(prependStep(JsonStep.Index(3)), FromJson.fromJson(Vector.get(3, v)));
                    x5 <- Result.mapErr(prependStep(JsonStep.Index(4)), FromJson.fromJson(Vector.get(4, v)));
                    x6 <- Result.mapErr(prependStep(JsonStep.Index(5)), FromJson.fromJson(Vector.get(5, v)));
                    x7 <- Result.mapErr(prependStep(JsonStep.Index(6)), FromJson.fromJson(Vector.get(6, v)));
                    x8 <- Result.mapErr(prependStep(JsonStep.Index(7)), FromJson.fromJson(Vector.get(7, v)));
                    x9 <- Result.mapErr(prependStep(JsonStep.Index(8)), FromJson.fromJson(Vector.get(8, v)))
                ) yield (x1, x2, x3, x4, x5, x6, x7, x8, x9)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 9")))
        }
    }

    instance FromJson[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with FromJson[a1], FromJson[a2], FromJson[a3], FromJson[a4], FromJson[a5], FromJson[a6], FromJson[a7], FromJson[a8], FromJson[a9], FromJson[a10] {
        pub def fromJson(j: Json): Result[JsonError, (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] = match j {
            case JArray(v) if Vector.length(v) == 10 =>
                forM (
                    x1  <- Result.mapErr(prependStep(JsonStep.Index(0)), FromJson.fromJson(Vector.get(0, v)));
                    x2  <- Result.mapErr(prependStep(JsonStep.Index(1)), FromJson.fromJson(Vector.get(1, v)));
                    x3  <- Result.mapErr(prependStep(JsonStep.Index(2)), FromJson.fromJson(Vector.get(2, v)));
                    x4  <- Result.mapErr(prependStep(JsonStep.Index(3)), FromJson.fromJson(Vector.get(3, v)));
                    x5  <- Result.mapErr(prependStep(JsonStep.Index(4)), FromJson.fromJson(Vector.get(4, v)));
                    x6  <- Result.mapErr(prependStep(JsonStep.Index(5)), FromJson.fromJson(Vector.get(5, v)));
                    x7  <- Result.mapErr(prependStep(JsonStep.Index(6)), FromJson.fromJson(Vector.get(6, v)));
                    x8  <- Result.mapErr(prependStep(JsonStep.Index(7)), FromJson.fromJson(Vector.get(7, v)));
                    x9  <- Result.mapErr(prependStep(JsonStep.Index(8)), FromJson.fromJson(Vector.get(8, v)));
                    x10 <- Result.mapErr(prependStep(JsonStep.Index(9)), FromJson.fromJson(Vector.get(9, v)))
                ) yield (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
            case _ => Err(Decode(Json.JsonPath.empty(), TypeMismatch("JArray of length 10")))
        }
    }

}
