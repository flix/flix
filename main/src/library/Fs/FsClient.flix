/*
 * Copyright 2026 Flix Authors
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

mod Fs.FsClient {

    use IoError.IoError
    use RichString.{text, bold, cyan, green, red, gray}

    ///
    /// Logs a pre-operation message at `Debug` level.
    ///
    pub def logPre(op: String, path: String): Unit \ Logger =
        Logger.log(Severity.Debug,
            gray("-->") + text(" ") + bold(op) + text(" ") + cyan(path))

    ///
    /// Logs a post-operation message: `Debug` on success, `Warn` on error.
    ///
    pub def logPost(op: String, path: String, describe: a -> String, result: Result[IoError, a]): Unit \ Logger =
        match result {
            case Ok(v)    =>
                Logger.log(Severity.Debug,
                    gray("<--") + text(" ") + green("OK") + text(" ") +
                    bold(op) + text(" ") + cyan(path) +
                    gray(" (${describe(v)})"))
            case Err(err) =>
                Logger.log(Severity.Warn,
                    gray("<--") + text(" ") + red("ERR") + text(" ") +
                    bold(op) + text(" ") + cyan(path) +
                    text(" ") + red("${err}"))
        }

}
