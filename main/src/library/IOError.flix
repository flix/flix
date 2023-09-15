/*
 * Copyright 2023 Magnus Madsen
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
pub enum IOError with ToString {
    case Generic(String)
}

mod IOError {

    use IOError.Generic

    ///
    /// A version of Result.tryCatch that runs an IO based operation and returns
    /// an IOError on failure.
    ///
    /// Returns `Ok(x)` if `f` was invoked without throwing an exception.
    ///
    /// If `f` throws a Java `Exception`, `Err(e)` is returned
    /// where `e` is the error message.
    ///
    pub def tryCatch(f: Unit -> a \ IO): Result[IOError, a] \ IO =
        try {
            Ok(f())
        } catch {
            case e: ##java.lang.Exception =>
                import java.lang.Throwable.getMessage(): String \ {};
                Err(Generic(getMessage(e)))
        }

}
