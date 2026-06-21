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

pub mod Net.RecordedRequest {

    use Net.HttpRequest.HttpRequest
    use Net.RecordedBody
    use Util.Json
    use Util.Json.FromJson
    use Util.Json.FromJson.decodeAtKey
    use Util.Json.Json.JObject
    use Util.Json.JsonError
    use Util.Json.ToJson

    ///
    /// A recorded HTTP request (serializable subset of HttpRequest).
    ///
    pub enum RecordedRequest({
        method  = String,
        url     = String,
        headers = Map[String, List[String]],
        body    = Option[RecordedBody]
    })

    instance ToJson[RecordedRequest] {
        pub def toJson(x: RecordedRequest): Json =
            let RecordedRequest(r) = x;
            JObject(Map#{
                "method"  => ToJson.toJson(r#method),
                "url"     => ToJson.toJson(r#url),
                "headers" => ToJson.toJson(r#headers),
                "body"    => ToJson.toJson(r#body)
            })
    }

    instance FromJson[RecordedRequest] {
        pub def fromJson(j: Json): Result[JsonError, RecordedRequest] =
            forM (
                method  <- decodeAtKey("method", j);
                url     <- decodeAtKey("url", j);
                headers <- decodeAtKey("headers", j);
                body    <- decodeAtKey("body", j)
            ) yield RecordedRequest({method = method, url = url, headers = headers, body = body})
    }

    ///
    /// Converts a live `HttpRequest` to a `RecordedRequest`.
    ///
    pub def fromHttpRequest(req: HttpRequest): RecordedRequest =
        let HttpRequest(x) = req;
        RecordedRequest({
            method  = "${x#method}",
            url     = x#url,
            headers = x#headers,
            body    = Option.map(RecordedBody.fromBody, x#body)
        })

}
