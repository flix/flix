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

pub mod Checksum {

    import java.security.MessageDigest

    ///
    /// Returns the SHA-256 digest of the given bytes `v`.
    ///
    pub def sha256(v: Vector[Int8]): Vector[Int8] =
        unsafe IO {
            let md = MessageDigest.getInstance("SHA-256");
            Array.toVector(md.digest(v))
        }

}
