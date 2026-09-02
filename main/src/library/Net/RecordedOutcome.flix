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

pub mod Net.RecordedOutcome {

    use IoError.ErrorKind
    use IoError.IoError
    use Net.HttpResponse.HttpResponse
    use Net.RecordedBody
    use Util.Json
    use Util.Json.FromJson
    use Util.Json.FromJson.decodeAtKey
    use Util.Json.Json.{JObject, JString}
    use Util.Json.JsonError
    use Util.Json.JsonError.DecodeErrorKind.TypeMismatch
    use Util.Json.JsonError.JsonError.Decode
    use Util.Json.JsonPath.empty
    use Util.Json.ToJson

    ///
    /// A recorded HTTP outcome — success or transport error.
    ///
    pub enum RecordedOutcome {
        case Success({
            status  = Int32,
            url     = String,
            headers = Map[String, List[String]],
            body    = RecordedBody
        })
        case TransportError({kind = String, message = String})
    }

    /// Encoded as a tagged object discriminated by `"type"`: `"success"` or `"error"`.
    instance ToJson[RecordedOutcome] {
        pub def toJson(x: RecordedOutcome): Json = match x {
            case RecordedOutcome.Success(s) =>
                JObject(Map#{
                    "type"    => JString("success"),
                    "status"  => ToJson.toJson(s#status),
                    "url"     => ToJson.toJson(s#url),
                    "headers" => ToJson.toJson(s#headers),
                    "body"    => ToJson.toJson(s#body)
                })
            case RecordedOutcome.TransportError(e) =>
                JObject(Map#{
                    "type"    => JString("error"),
                    "kind"    => JString(e#kind),
                    "message" => JString(e#message)
                })
        }
    }

    instance FromJson[RecordedOutcome] {
        pub def fromJson(j: Json): Result[JsonError, RecordedOutcome] =
            forM (
                tag     <- decodeAtKey("type", j);
                outcome <- match tag {
                    case "success" => forM (
                        status  <- decodeAtKey("status", j);
                        url     <- decodeAtKey("url", j);
                        headers <- decodeAtKey("headers", j);
                        body    <- decodeAtKey("body", j)
                    ) yield RecordedOutcome.Success({status = status, url = url, headers = headers, body = body})
                    case "error"   => forM (
                        kind    <- decodeAtKey("kind", j);
                        message <- decodeAtKey("message", j)
                    ) yield RecordedOutcome.TransportError({kind = kind, message = message})
                    case other     => Err(Decode(empty(), TypeMismatch("RecordedOutcome tag '${other}'")))
                }
            ) yield outcome
    }

    ///
    /// Converts a live `Result[IoError, HttpResponse]` to a `RecordedOutcome`.
    ///
    pub def fromResult(result: Result[IoError, HttpResponse]): RecordedOutcome =
        match result {
            case Ok(HttpResponse(x)) =>
                RecordedOutcome.Success({
                    status  = x#status,
                    url     = x#url,
                    headers = x#headers,
                    body    = RecordedBody.fromBody(x#body)
                })
            case Err(IoError(kind, msg)) =>
                RecordedOutcome.TransportError({kind = "${kind}", message = msg})
        }

    ///
    /// Converts a `RecordedOutcome` back to a live `Result[IoError, HttpResponse]`.
    ///
    pub def toResult(outcome: RecordedOutcome): Result[IoError, HttpResponse] \ Abort =
        match outcome {
            case RecordedOutcome.Success(x) =>
                Ok(HttpResponse({
                    status  = x#status,
                    url     = x#url,
                    headers = x#headers,
                    body    = RecordedBody.toBody(x#body)
                }))
            case RecordedOutcome.TransportError(x) =>
                Err(IoError(ErrorKind.Other, x#message))
        }

}
