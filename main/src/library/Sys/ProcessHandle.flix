/*
 *  Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Sys.ProcessHandle {

    import java.lang.{Process => JProcess}

    ///
    /// A handle for a running process.
    ///
    pub enum ProcessHandle(JProcess)

}
