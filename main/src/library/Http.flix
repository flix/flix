/*
 *  Copyright 2024 Holger Dal Mogensen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// The effect used to interact with the HTTP protocol.
///
eff Http {
    pub def get(url: String, headers: Map[String, String]): Http.Response
}

mod Http {

    ///
    /// A response from an HTTP request.
    ///
    pub enum Response({status = Int32, statusText = String, headers = Map[String, String], body = String})

    mod Response {
        pub def status(r: Response): Int32 = match r {
            case Response(x) => x#status
        }
        pub def statusText(r: Response): String = match r {
            case Response(x) => x#statusText
        }
        pub def headers(r: Response): Map[String, String] = match r {
            case Response(x) => x#headers
        }
        pub def body(r: Response): String = match r {
            case Response(x) => x#body
        }
    }

}
