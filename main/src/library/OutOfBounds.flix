/*
 * Copyright 2024 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Indicates that an index was out of bounds.
/// Useful for instances of `Index` or `IndexMut`.
///
eff OutOfBounds {

    /// Stops execution with an error message.
    def outOfBounds(msg: String): Void

}

pub mod OutOfBounds {

    ///
    /// Handles the `OutOfBounds` effect of the given function `f` by converting
    /// out-of-bounds errors to `Result` values.
    ///
    /// Returns `Ok(f(x))` if the computation completes successfully.
    /// Returns `Err(msg)` if the computation raises `outOfBounds(msg)`.
    ///
    @Experimental
    pub def handleWithResult(f: a -> b \ ef): a -> Result[String, b] \ (ef - OutOfBounds) = x ->
        run Ok(f(x)) with handler OutOfBounds {
            def outOfBounds(msg, _k) = Err(msg)
        }

    ///
    /// Runs the `OutOfBounds` effect of the given function `f`.
    ///
    /// Returns `Ok(f())` if the computation completes successfully.
    /// Returns `Err(msg)` if the computation raises `outOfBounds(msg)`.
    ///
    @Experimental
    pub def runWithResult(f: Unit -> a \ ef): Result[String, a] \ (ef - OutOfBounds) = handleWithResult(f)()

}
