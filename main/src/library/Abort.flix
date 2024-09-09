/*
 *  Copyright 2024 Magnus Madsen
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
/// Aborts the current computation with an error message.
///
pub eff Abort {
    ///
    /// Immediately aborts the current computation with the given error message `m`.
    ///
    /// The computation cannot be resumed.
    ///
    pub def abort(m: String): Void
}

mod Abort {

    ///
    /// Runs the given function `f` which has an `Abort` effect.
    ///
    /// Returns `Ok(f())` if the computation completes successfully without aborting.
    /// Returns `Err(m)`  if the computation aborts with error message `m`.
    ///
    pub def run(f: Unit -> a \ Abort): Result[String, a] =
        try {
            Ok(f())
        } with Abort {
            def abort(m, _k) = Result.Err(m)
        }

}
