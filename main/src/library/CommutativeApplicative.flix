/*
 *  Copyright 2022 Jakob Schneider Villumsen
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
///
///
pub class CommutativeApplicative[m : Type -> Type] with Applicative[m] {

    ///
    /// Puts `x` into a default context.
    ///
    pub def point(x: a): m[a] =
        Applicative.point(x)

    ///
    /// Apply the function-type applicative `f` to the argument-type applicative `x`.
    ///
    pub def ap(f: m[a -> b \ ef], x: m[a]): m[b] \ ef =
        Applicative.ap(f, x)

}
