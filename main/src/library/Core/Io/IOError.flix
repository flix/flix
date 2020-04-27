/*
 * Copyright 2020 Rasmus Larsen
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

use Core/Io/File.File;

namespace Core/Io/IOError {
    ///
    /// Represents an error when operating in an I/O context.
    ///
    pub enum IOError {
        case ElementNotFound(String)
        case FileNotFound(File)
        case Generic(String)
        case IllegalState
        case IncorrectBufferSize
        case MalformedZip
        case StreamClosed
    }
}