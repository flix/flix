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
    def abort(m: RichString): Void

}

pub mod Abort {

    import java.lang.System

    ///
    /// Handles the `Abort` effect of the given function `f` by printing the error message and exiting.
    ///
    /// In other words, re-interprets the `Abort` effect using the `IO` effect.
    ///
    /// Note: The returned function can still return normally if `abort` is never called by `f`.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Abort) + IO = x ->
        run {
            f(x)
        } with handler Abort {
            def abort(m, _k) = {
                System.err.println(RichString.toString(m));
                unchecked_cast(System.exit(1) as b)
            }
        }

    ///
    /// Handles the `Abort` effect of the given function `f` by converting aborts to Result values.
    ///
    /// Returns a function that
    ///
    /// Returns `Ok(f())` if the computation completes successfully without aborting.
    /// Returns `Err(m)`  if the computation aborts with error message `m`.
    ///
    pub def handleWithResult(f: a -> b \ ef): a -> Result[RichString, b] \ (ef - Abort) = x ->
        run {
            Ok(f(x))
        } with handler Abort {
            def abort(m, _k) = Result.Err(m)
        }


    ///
    /// Runs the `Abort` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Abort` effect using the `IO` effect.
    ///
    /// Note: This function can still return normally if `abort` is never called by `f`.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Abort) + IO = handle(f)()

    ///
    /// Runs the `Abort` effect of the given function `f`.
    ///
    /// Returns `Ok(f())` if the computation completes successfully without aborting.
    /// Returns `Err(m)`  if the computation aborts with error message `m`.
    ///
    pub def runWithResult(f: Unit -> a \ ef): Result[RichString, a] \ (ef - Abort) = handleWithResult(f)()

}
