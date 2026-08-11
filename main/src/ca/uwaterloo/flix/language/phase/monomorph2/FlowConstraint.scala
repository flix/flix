/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

/**
  * A positional assignment of a [[MonoArg]] to each of a [[MonoVar]]'s type-parameter slots,
  * i.e. in a [[FlowConstraint]], `args(i)` is what flows into `dst`'s ith type-parameter.
  */
case class Instantiation(args: List[MonoArg])

/**
  * A component-wise flow constraint: `args` flows into the type-parameter slots of `dst`.
  */
case class FlowConstraint(args: Instantiation, dst: MonoVar)
