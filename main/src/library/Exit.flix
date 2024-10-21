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
/// An effect used to exit the JVM.
///
eff Exit {

    ///
    /// Immediately exits the JVM with the specified `exitCode`.
    ///
    pub def exit(exitCode: Int32): Void

}

mod Exit {

    import java.lang.System

    ///
    /// Runs the `Exit` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Exit` effect using the `Sys` effect.
    ///
    /// Note: This function can still return normally if `exit` is never called by `f`.
    ///
    pub def run(f: Unit -> a \ ef): a \ ef - Exit + Sys  = {
        // Currently try-with typing is broken, so fix it with checked_ecast
        unchecked_cast(try {
            f()
        } with Exit {
            def exit(ec, _k) = unsafe {
                unchecked_cast(System.exit(ec) as a)
            }
        } as a \ (ef - Exit) + Sys)
    }

}
