/*
 * Copyright 2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.util.tc.Show.ShowableSyntax

object Scheme {

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme)(implicit genSym: GenSym): Type = Type.refreshTypeVars(sc.quantifiers, sc.base)

}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Type.Var], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = s"∀(${quantifiers.mkString(", ")}). ${base.show}"

}
