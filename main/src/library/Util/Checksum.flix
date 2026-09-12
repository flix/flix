/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Util.Checksum {

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
