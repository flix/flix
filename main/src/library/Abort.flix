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
/// An effect used to abort computation with an error message.
///
pub eff Abort {

    ///
    /// Immediately aborts the current computation with the given error message `m`.
    ///
    /// The computation cannot be resumed.
    ///
    def abort(m: String): Void

}

mod Abort {

    ///
    /// Handle the `Abort` effect of the given function `f`.
    ///
    /// Returns a function that
    ///     - returns `Ok(f())` if the computation completes successfully without aborting.
    ///     - returns `Err(m)`  if the computation aborts with error message `m`.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[String, b] \ (ef - Abort) = x ->
        try {
            Ok(f(x))
        } with Abort {
            def abort(m, _k) = Result.Err(m)
        }

    ///
    /// Runs the `Abort` effect of the given function `f`.
    ///
    /// Returns `Ok(f())` if the computation completes successfully without aborting.
    /// Returns `Err(m)`  if the computation aborts with error message `m`.
    ///
    pub def runWithResult(f: Unit -> a \ ef): Result[String, a] \ (ef - Abort) = handle(f)()

}
