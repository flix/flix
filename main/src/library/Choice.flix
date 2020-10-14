/*
 * Copyright 2020 Magnus Madsen
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

/**
 * The Choice data type.
 */
pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
    case Absent
    case Present(a)
}

namespace Choice {

    ///
    /// Returns the value in `c` which must be `Present`.
    ///
    pub def unbox[a, isAbsent :# Bool](c: Choice[a, false, isAbsent]): a = choose c {
        case Present(v) => v
    }

    ///
    /// Returns the given choice `c` as an `Option`.
    ///
//    pub def toOption[a, isAbsent :# Bool, isPresent :# Bool](c: Choice[a, isAbsent, isPresent]): Option[a] = choose c {
//        case Absent     => None
//        case Present(v) => Some(v)
//    }

}
