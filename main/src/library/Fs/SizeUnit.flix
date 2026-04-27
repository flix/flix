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

pub mod Fs.SizeUnit {

    ///
    /// Represents a size unit.
    ///
    pub enum SizeUnit with Eq, ToString {
        case Bytes,
        case KiloBytes,
        case MegaBytes,
        case GigaBytes,
        case TeraBytes
    }

    ///
    /// Returns the number of bytes for the given size unit `u`.
    ///
    pub def toBytes(u: SizeUnit): Int64 = match u {
        case SizeUnit.Bytes     => 1i64,
        case SizeUnit.KiloBytes => 1_024i64,
        case SizeUnit.MegaBytes => 1_024i64 * 1_024i64,
        case SizeUnit.GigaBytes => 1_024i64 * 1_024i64 * 1_024i64,
        case SizeUnit.TeraBytes => 1_024i64 * 1_024i64 * 1_024i64 * 1_024i64
    }

}
