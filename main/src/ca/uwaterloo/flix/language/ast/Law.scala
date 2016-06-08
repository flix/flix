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

package ca.uwaterloo.flix.language.ast

sealed trait Law

object Law {

  case object Associativity extends Law

  case object Commutativity extends Law

  case object Reflexivity extends Law

  case object AntiSymmetry extends Law

  case object Transitivity extends Law

  case object LeastElement extends Law

  case object UpperBound extends Law

  case object LeastUpperBound extends Law

  case object GreatestElement extends Law

  case object LowerBound extends Law

  case object GreatestLowerBound extends Law

  case object Strict extends Law

  case object Monotone extends Law

  case object HeightNonNegative extends Law

  case object HeightStrictlyDecreasing extends Law

}
