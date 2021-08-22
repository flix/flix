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
namespace Object {

    ///
    /// Returns `true` if the given value `x` is `null`.
    ///
    pub def isNull(x: a): Bool =
        import java.util.Objects:isNull(##java.lang.Object);
        isNull(x as ##java.lang.Object) as & Pure

    ///
    /// Returns `None` if the given value `x` is `null`, otherwise returns `Some(x)`.
    ///
    pub def toOption(x: a): Option[a] =
        if (isNull(x)) None else Some(x)

}
