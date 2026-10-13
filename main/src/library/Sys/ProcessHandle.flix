/*
 *  Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Type of handles to running processes, and its functions.
///
pub mod Sys.ProcessHandle {

    import java.lang.{Process => JProcess}

    ///
    /// Type of handles to running processes.
    ///
    pub enum ProcessHandle(JProcess)

}
