/*
 * Copyright 2017 Liam Palmer
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
/// The Tuple type.
///

namespace Tuple2 {

    ///
    /// Returns `true` if and only if `t1` and `t2` and equal.
    ///
    pub def eq[a, b](t1: (a, b), t2: (a, b)): Bool =
        let (a1, b1) = t1;
        let (a2, b2) = t2;
            a1 == a2 && b1 == b2
}

namespace Tuple3 {

    ///
    /// Returns `true` if and only if `t1` and `t2` and equal.
    ///
    pub def eq[a, b, c](t1: (a, b, c), t2: (a, b, c)): Bool =
        let (a1, b1, c1) = t1;
        let (a2, b2, c2) = t2;
            a1 == a2 && b1 == b2 && c1 == c2
}

namespace Tuple4 {

    ///
    /// Returns `true` if and only if `t1` and `t2` and equal.
    ///
    pub def eq[a, b, c, d](t1: (a, b, c, d), t2: (a, b, c, d)): Bool =
        let (a1, b1, c1, d1) = t1;
        let (a2, b2, c2, d2) = t2;
            a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2
}
