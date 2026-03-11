/*
 *  Copyright 2025 Ry Wiese
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
use IoError.ErrorKind
use IoError.IoError

import java.io.IOException
import java.io.OutputStream

///
/// A trait for types which are resources that can be written to.
///
trait Writable[t] {

    ///
    /// Type of each element that can be written to the resource.
    ///
    type Elm: Type

    ///
    /// Effect associated with writing to the underlying resource.
    ///
    type Aef: Eff

    ///
    /// Reads `k` items from `b` and writes them into the underlying resource.
    ///
    /// Returns `Ok(k)` to signify that `k` items were successfully read and written to `r`.
    ///
    /// Guarantees that `0 <= k <= length(b)`.
    ///
    /// Returns `Err(e)` if some underlying I/O error occurs.
    ///
    pub def write(buffer: Array[Writable.Elm[t], r], writer: t): Result[IoError, Int32] \ r + Writable.Aef[t]

}

pub mod Writable {
    import java.lang.System

    ///
    /// Writes `string` to `writer`.
    ///
    pub def writeString(string: String, writer: t): Result[IoError, Unit] \ Writable.Aef[t] with Writable[t] where Writable.Elm[t] ~ Char =
        region rc {
            let buffer = String.toArray(rc, string);
            forM(
                charsWritten <- Writable.write(buffer, writer);
                _ <- if (charsWritten < String.length(string)) {
                    writeString(string |> String.drop(charsWritten), writer)
                } else Ok(())
            ) yield ()
        }

    ///
    /// Writes a new line separator to `writer`.
    ///
    /// The new line separator is platform dependent, and is determined by the "line.separator" system property.
    ///
    pub def writeNewLine(writer: t): Result[IoError, Unit] \ IO + Writable.Aef[t] with Writable[t] where Writable.Elm[t] ~ Char =
        let newLine = System.getProperty("line.separator");
        writeString(newLine, writer)

    ///
    /// Writes `line` to `writer` and appends a new line separator.
    ///
    /// The new line separator is platform dependent, and is determined by the "line.separator" system property.
    ///
    pub def writeLine(line: String, writer: t): Result[IoError, Unit] \ IO + Writable.Aef[t] with Writable[t] where Writable.Elm[t] ~ Char =
        forM(
            _ <- writeString(line, writer);
            _ <- writeNewLine(writer)
        ) yield ()

}

instance Writable[OutputStream] {

    type Elm = Int8

    type Aef = IO

    pub def write(b: Array[Int8, r], r: OutputStream): Result[IoError, Int32] \ r + IO =
        try {
            r.write(b);
            checked_ecast(Ok(Array.length(b)))
        } catch {
            case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

}
