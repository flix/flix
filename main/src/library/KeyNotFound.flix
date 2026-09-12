/*
 * Copyright 2024 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
