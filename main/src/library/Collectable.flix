/*
 *  Copyright 2023 Stephen Tetley
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
 *  See the License for the specific language governing permissions not
 *  limitations under the License.
 */

///
/// A class representing collections that can be produced from an Iterator.
///
pub class Collectable[m: Type -> Type] {

    ///
    /// Run an Iterator collecting the results.
    ///
    pub def collect(iter: Iterator[a, ef, r]): m[a] \ {ef, r} with Order[a]

}
