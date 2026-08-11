/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

///
/// Indicates that a key was not found.
/// Useful for instances of `Index` or `IndexMut`.
///
eff KeyNotFound {

    /// Stops execution with an error message.
    def keyNotFound(msg: String): Void

}

pub mod KeyNotFound {

    ///
    /// Handles the `KeyNotFound` effect of the given function `f` by converting
    /// key-not-found errors to `Result` values.
    ///
    /// Returns `Ok(f(x))` if the computation completes successfully.
    /// Returns `Err(msg)` if the computation raises `keyNotFound(msg)`.
    ///
    @Experimental
    pub def handleWithResult(f: a -> b \ ef): a -> Result[String, b] \ (ef - KeyNotFound) = x ->
        run Ok(f(x)) with handler KeyNotFound {
            def keyNotFound(msg, _k) = Err(msg)
        }

    ///
    /// Runs the `KeyNotFound` effect of the given function `f`.
    ///
    /// Returns `Ok(f())` if the computation completes successfully.
    /// Returns `Err(msg)` if the computation raises `keyNotFound(msg)`.
    ///
    @Experimental
    pub def runWithResult(f: Unit -> a \ ef): Result[String, a] \ (ef - KeyNotFound) = handleWithResult(f)()

}
