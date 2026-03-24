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

pub mod Net.RecordedBody {

    use Net.Body
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
    /// A serializable body: text stored as-is, bytes stored as Base64.
    ///
    pub enum RecordedBody {
        case TextBody(String)
        case BytesBody(String)
    }

    /// Encoded as a tagged object: `{"type": "text"|"bytes", "value": "..."}`.
    instance ToJson[RecordedBody] {
        pub def toJson(x: RecordedBody): Json = match x {
            case RecordedBody.TextBody(s)  => JObject(Map#{"type" => JString("text"),  "value" => JString(s)})
            case RecordedBody.BytesBody(s) => JObject(Map#{"type" => JString("bytes"), "value" => JString(s)})
        }
    }

    instance FromJson[RecordedBody] {
        pub def fromJson(j: Json): Result[JsonError, RecordedBody] =
            forM (
                tag   <- decodeAtKey("type", j);
                value <- decodeAtKey("value", j);
                body  <- match tag {
                    case "text"  => Ok(RecordedBody.TextBody(value))
                    case "bytes" => Ok(RecordedBody.BytesBody(value))
                    case other   => Err(Decode(empty(), TypeMismatch("RecordedBody tag '${other}'")))
                }
            ) yield body
    }

    ///
    /// Converts a live `Body` to a `RecordedBody`.
    ///
    /// Text bodies are stored as-is. Byte bodies are Base64-encoded.
    ///
    pub def fromBody(body: Body): RecordedBody = match body {
        case Body.Text(s)  => RecordedBody.TextBody(s)
        case Body.Bytes(v) => RecordedBody.BytesBody(Codec.encodeBase64(v))
    }

    ///
    /// Converts a `RecordedBody` back to a live `Body`.
    ///
    pub def toBody(body: RecordedBody): Body = match body {
        case RecordedBody.TextBody(s)  => Body.Text(s)
        case RecordedBody.BytesBody(b) => match Codec.decodeBase64(b) {
            case Ok(v)  => Body.Bytes(v)
            case Err(_) => bug!("Invalid Base64 in cassette body")
        }
    }

}
