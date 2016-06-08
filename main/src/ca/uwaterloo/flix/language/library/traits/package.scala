/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.library

package object traits {

  /**
    * TODO: Implement traits for all basic types and order/lattice types.
    *
    * trait BitOr with T {
    * fn ||(a: T, b: T): T
    * }
    *
    * impl BitOr for Bool {
    * ...
    * }
    *
    * trait Bot with T {
    * fn bot(): T
    * }
    *
    * trait Top with T {
    * fn top(): T
    * }
    *
    * trait PartialOrder with T {
    * def leq(a: T, b: T): Bool
    * }
    *
    * trait JoinLattice with T : Bot + PartialOrder {
    * fn lub(a: T, b: T): T
    * }
    */

}
