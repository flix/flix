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
pub class TraverseFilter[t : Type -> Type] with FunctorFilter[t] {

    pub def traverseFilter(f: a -> m[Option[b]] \ ef, t: t[a]): m[t[b]] \ ef with Applicative[m]

    pub def sequenceFilter(t: t[m[Option[a]]]): m[t[a]] with Applicative[m] =
        TraverseFilter.traverseFilter(identity, t)

}
