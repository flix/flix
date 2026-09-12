/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.SizeUnit {

    ///
    /// Represents a size unit.
    ///
    pub enum SizeUnit with Eq, ToString {
        case Bytes
        case KiloBytes
        case MegaBytes
        case GigaBytes
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
