/*
 * Copyright 2025 Magnus Madsen
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

pub mod Net.SimpleHttp.Response {

    use Net.SimpleHttp.Response
    use Net.SimpleHttp.Response.Response

    pub def status(r: Response): Int32 = match r {
        case Response(x) => x#status
    }
    pub def headers(r: Response): Map[String, String] = match r {
        case Response(x) => x#headers
    }
    pub def body(r: Response): String = match r {
        case Response(x) => x#body
    }

}
