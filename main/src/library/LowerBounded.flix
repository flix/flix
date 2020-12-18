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

instance LowerBounded[(a1, a2, a3, a4)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded, a4 : LowerBounded] {
    def bot(): (a1, a2, a3, a4) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}

instance LowerBounded[(a1, a2, a3, a4, a5)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded, a4 : LowerBounded, a5 : LowerBounded] {
    def bot(): (a1, a2, a3, a4, a5) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}

instance LowerBounded[(a1, a2, a3, a4, a5, a6)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded, a4 : LowerBounded, a5 : LowerBounded, a6 : LowerBounded] {
    def bot(): (a1, a2, a3, a4, a5, a6) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}

instance LowerBounded[(a1, a2, a3, a4, a5, a6, a7)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded, a4 : LowerBounded, a5 : LowerBounded, a6 : LowerBounded, a7 : LowerBounded] {
    def bot(): (a1, a2, a3, a4, a5, a6, a7) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}

instance LowerBounded[(a1, a2, a3, a4, a5, a6, a7, a8)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded, a4 : LowerBounded, a5 : LowerBounded, a6 : LowerBounded, a7 : LowerBounded, a8 : LowerBounded] {
    def bot(): (a1, a2, a3, a4, a5, a6, a7, a8) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}

instance LowerBounded[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded, a4 : LowerBounded, a5 : LowerBounded, a6 : LowerBounded, a7 : LowerBounded, a8 : LowerBounded, a9 : LowerBounded] {
    def bot(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}

instance LowerBounded[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with [a1 : LowerBounded, a2 : LowerBounded, a3 : LowerBounded, a4 : LowerBounded, a5 : LowerBounded, a6 : LowerBounded, a7 : LowerBounded, a8 : LowerBounded, a9 : LowerBounded, a10 : LowerBounded] {
    def bot(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot(), LowerBounded.bot())
}
