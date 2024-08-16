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
/// Indicates that a key was invalid.
/// Useful for instances of `Index` or `IndexMut`.
///
eff InvalidKey {

    /// Stops execution with an error message.
    pub def error(msg: String): Void

}

mod InvalidKey {

    /// Removes the `InvalidKey` effect from a function `f`, if `f` `do` `InvalidKey`.
    pub def asRes(f: a -> b \ ef): (a -> Result[String, b] \ ef - InvalidKey) = {
        // TODO: remove annotations and casts once try-with typing works.
        x -> (checked_ecast(try (Ok(f(x)): _ \ ef) with InvalidKey {
            def error(msg, _) = Err(msg)
        }): _ \ ef - InvalidKey)
    }

}
