/*
 *  Copyright 2024 Maksim Gusev
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
/// Handles the created Process.
///
///
/// The underlying value is an opaque runtime handle.
///
enum ProcessHandle(Int64)

pub mod ProcessHandle {

    use IoError.ErrorKind
    use IoError.IoError

    ///
    /// Releases the process handle, removing it from the runtime handle table.
    ///
    /// Note: This does not stop/kill the underlying process.
    ///
    pub def release(ph: ProcessHandle): Result[IoError, Unit] \ IO =
        let ProcessHandle(id) = ph;
        let (ok, msg) = %%PROCESS_RELEASE%%(id);
        if (ok) Ok(()) else Err(IoError(ErrorKind.Other, msg))

}
