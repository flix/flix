/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}

object BaseEffects {

  /**
   * A pre-computed map from classes to effects.
   */
  private val m: Map[String, Set[Symbol.EffectSym]] = Map(
    classOf[java.lang.ProcessBuilder].getName -> Set(Symbol.Exec)
  )

  /**
   * Returns the base effect set of the given Java `clazz`.
   */
  def of(clazz: Class[?], loc: SourceLocation): Type = {
    val effs = m.getOrElse(clazz.getName, Set.empty).toList
    val tpes = effs.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
    Type.mkUnion(tpes, loc)
  }

}
