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

pub mod Net.HttpInteraction {

    use Net.RecordedOutcome
    use Net.RecordedRequest
    use Util.Json
    use Util.Json.FromJson
    use Util.Json.FromJson.decodeAtKey
    use Util.Json.Json.JObject
    use Util.Json.JsonError
    use Util.Json.ToJson

    ///
    /// A single recorded HTTP interaction.
    ///
    pub enum HttpInteraction({
        request = RecordedRequest,
        outcome = RecordedOutcome
    })

    instance ToJson[HttpInteraction] {
        pub def toJson(x: HttpInteraction): Json =
            let HttpInteraction(i) = x;
            JObject(Map#{
                "request" => ToJson.toJson(i#request),
                "outcome" => ToJson.toJson(i#outcome)
            })
    }

    instance FromJson[HttpInteraction] {
        pub def fromJson(j: Json): Result[JsonError, HttpInteraction] =
            forM (
                request <- decodeAtKey("request", j);
                outcome <- decodeAtKey("outcome", j)
            ) yield HttpInteraction({request = request, outcome = outcome})
    }

}
