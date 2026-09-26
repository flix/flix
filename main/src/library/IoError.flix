/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod IoError {

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

    pub enum ErrorKind with Eq, ToString {
        case AlreadyExists
        case ChecksumMismatch
        case Conflict
        case ConnectionFailed
        case Interrupted
        case InvalidPath
        case InvalidInput
        case InvalidData
        case NotFound
        case NotFile
        case NotDirectory
        case PermissionDenied
        case TransferLimitExceeded
        case Timeout
        case UnexpectedEof
        case Unsupported
        case UnknownHost
        case Other
    }

    instance Formattable[ErrorKind] {
        pub def format(k: ErrorKind): RichString = RichString.fromString(ToString.toString(k))
    }

}
