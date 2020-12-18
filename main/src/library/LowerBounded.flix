/*
 *  Copyright 2020 Magnus Madsen
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

///
/// A type class for partially ordered types that have a lower bound.
///
pub class LowerBounded[a] {
    ///
    /// Returns the smallest value of `a`.
    ///
    pub def bot(): a
}

instance LowerBounded[(a1, a2)] with [a1 : LowerBounded, a2 : LowerBounded] {
    def bot(): (a1, a2) = (LowerBounded.bot(), LowerBounded.bot())
}

instance LowerBounded[(a1, a2, a3)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded] {
    def bot(): (a1, a2, a3) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}
