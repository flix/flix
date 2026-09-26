/*
 *  Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Sys.Exit {

    use Sys.Exit

    import java.lang.System

    ///
    /// An effect used to exit the JVM.
    ///
    pub eff Exit {

        ///
        /// Immediately exits the JVM with the specified `exitCode`.
        ///
        def exit(exitCode: Int32): Void

    }

    ///
    /// Handles the `Exit` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Exit` effect using the `IO` effect.
    ///
    /// Note: The returned function can still return normally if `exit` is never called by `f`.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Exit) + IO = x ->
        run {
            f(x)
        } with handler Exit {
            def exit(ec, _k) = unchecked_cast(System.exit(ec) as b)
        }

    ///
    /// Runs the `Exit` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Exit` effect using the `IO` effect.
    ///
    /// Note: This function can still return normally if `exit` is never called by `f`.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Exit) + IO = handle(f)()

}
