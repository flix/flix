/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

def main(): Unit \ { Sys, Net, IO } = run {
    let args = Environment.getArgs();
    match List.head(args) {
        case None =>
            Logger.fatal("No URL provided")

        case Some(url) =>
            let opts = { targetFile = "index.html" };
            WGet.download(opts, url)
    }

} with Environment.runWithIO
with alertOnHttpError
with alertOnFileWriteError
with Logger.runWithIO

def alertOnHttpError(f: Unit -> Unit \ ef): Unit \ (ef - Http) + { Logger, Net, IO } = match Http.runWithIO(f) {
    case Err(e) => Logger.fatal(e)
    case Ok(()) => ()
}

def alertOnFileWriteError(f: Unit -> Unit \ ef): Unit \ (ef - FileWrite) + { Logger, IO } = match FileWrite.runWithIO(f) {
    case Err(e) => Logger.fatal(e)
    case Ok(()) => ()
}
