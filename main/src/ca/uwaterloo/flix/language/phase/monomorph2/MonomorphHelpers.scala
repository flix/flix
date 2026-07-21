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

import ca.uwaterloo.flix.language.ast.Type

import scala.annotation.tailrec

/**
  * Generic helpers shared across the constraint-based monomorphization pipeline.
  */
private[monomorph2] object MonomorphHelpers {

  /** Walks `tpe`'s `Type.Apply` chain once, returning its head and its args in left-to-right order. */
  def flattenApply(tpe: Type): (Type, List[Type]) = {
    @tailrec
    def loop(t: Type, argsAcc: List[Type]): (Type, List[Type]) = t match {
      case Type.Apply(t1, t2, _) => loop(t1, t2 :: argsAcc)
      case head => (head, argsAcc)
    }
    loop(tpe, Nil)
  }
}
