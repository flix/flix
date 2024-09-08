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

import java.lang.reflect.Method

object BaseEffects {

  /**
    * A pre-computed map from classes to effects.
    */
  private val classEffs: Map[String, Set[Symbol.EffectSym]] = Map(
    classOf[java.lang.ProcessBuilder].getName -> Set(Symbol.Exec)
  )

  /**
    * A pre-computed map from methods to effects.
    */
  private val methodEffs: Map[Method, Set[Symbol.EffectSym]] = Map(
    classOf[java.lang.System].getMethod("currentTimeMillis") -> Set(Symbol.Clock)
  )

  /**
    * Returns the effects of the given Java class `c`.
    */
  def of(c: Class[?], loc: SourceLocation): Type = {
    val effs = classEffs.getOrElse(c.getName, Set.empty).toList
    val tpes = effs.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
    Type.mkUnion(tpes, loc)
  }

  /**
    * Returns the effects of the given Java method `m`.
    */
  def of(m: Method, loc: SourceLocation): Type = {
    val effs = methodEffs.getOrElse(m, Set.empty).toList
    val tpes = effs.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
    Type.mkUnion(tpes, loc)
  }

}
