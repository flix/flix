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

///
/// An error that can occur when performing I/O operations involving the file system.
///
/// The first element describes the general kind of error that occurred.
/// This can be used to determine the correct action to take to handle specific kinds of errors.
///
/// The second element is a string that describes the error, meant to be used for debugging purposes.
///
pub enum IoError(IoError.ErrorKind, String) with Eq, ToString

instance Formattable[IoError] {
    pub def format(e: IoError): RichString = match e {
        case IoError.IoError(kind, msg) =>
            Formattable.format(kind) + RichString.fromString(": ") + RichString.fromString(msg)
    }
}

pub mod IoError {

    pub enum ErrorKind with Eq, ToString {
        case AlreadyExists,
        case ConnectionFailed,
        case Interrupted,
        case InvalidPath,
        case InvalidInput,
        case InvalidData,
        case NotFound,
        case NotFile,
        case NotDirectory,
        case PermissionDenied,
        case Timeout,
        case UnexpectedEof,
        case Unsupported,
        case UnknownHost,
        case Other
    }

    ///
    /// Converts an integer error kind code to an `ErrorKind`.
    ///
    /// This mapping is part of the portable runtime contract for IO primops and syscalls.
    ///
    pub def errorKindFromCode(code: Int32): ErrorKind = match code {
        case 0  => ErrorKind.AlreadyExists
        case 1  => ErrorKind.ConnectionFailed
        case 2  => ErrorKind.Interrupted
        case 3  => ErrorKind.InvalidPath
        case 4  => ErrorKind.InvalidInput
        case 5  => ErrorKind.InvalidData
        case 6  => ErrorKind.NotFound
        case 7  => ErrorKind.NotFile
        case 8  => ErrorKind.NotDirectory
        case 9  => ErrorKind.PermissionDenied
        case 10 => ErrorKind.Timeout
        case 11 => ErrorKind.UnexpectedEof
        case 12 => ErrorKind.Unsupported
        case 13 => ErrorKind.UnknownHost
        case 14 => ErrorKind.Other
        case _  => ErrorKind.Other
    }

    ///
    /// Converts an `ErrorKind` to its integer error kind code.
    ///
    /// This mapping is part of the portable runtime contract for IO primops and syscalls.
    ///
    pub def errorKindToCode(kind: ErrorKind): Int32 = match kind {
        case ErrorKind.AlreadyExists      => 0
        case ErrorKind.ConnectionFailed   => 1
        case ErrorKind.Interrupted        => 2
        case ErrorKind.InvalidPath        => 3
        case ErrorKind.InvalidInput       => 4
        case ErrorKind.InvalidData        => 5
        case ErrorKind.NotFound           => 6
        case ErrorKind.NotFile            => 7
        case ErrorKind.NotDirectory       => 8
        case ErrorKind.PermissionDenied   => 9
        case ErrorKind.Timeout            => 10
        case ErrorKind.UnexpectedEof      => 11
        case ErrorKind.Unsupported        => 12
        case ErrorKind.UnknownHost        => 13
        case ErrorKind.Other              => 14
    }

    instance Formattable[ErrorKind] {
        pub def format(k: ErrorKind): RichString = RichString.fromString(ToString.toString(k))
    }

}
