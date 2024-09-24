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

import java.lang.reflect.{Constructor, Method}

object BaseEffects {

  /** A pre-computed map from constructors to effects. */
  private val constructorEffs: Map[Constructor[?], Set[Symbol.EffectSym]] = Map.empty ++
    classOf[java.net.URL].getConstructors.map(c => (c, Set(Symbol.Net)))

  /** A pre-computed map from methods to effects. */
  private val methodEffs: Map[Method, Set[Symbol.EffectSym]] = Map(
    classOf[java.lang.System].getMethod("currentTimeMillis") -> Set(Symbol.Time)
  )

  /**
   * A pre-computed map from classes to effects.
   *
   * If there is are specific effect(s) for a constructor or method then we use the effects for the entire class.
   */
  private val classEffs: Map[Class[?], Set[Symbol.EffectSym]] = Map(
    classOf[java.lang.ProcessBuilder] -> Set(Symbol.Exec)
  )

  /** Returns the base effects of calling the given constructor `c`. */
  def getConstructorEffs(c: Constructor[?], loc: SourceLocation): Type = constructorEffs.get(c) match {
    case None =>
      // Case 1: No effects for the constructor. Try the class map.
      classEffs.get(c.getDeclaringClass) match {
        case None =>
          // Case 1.1: We use the IO effect by default.
          Type.IO
        case Some(effs) =>
          // Case 1.2: We use the class effects.
          val tpes = effs.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
          Type.mkUnion(tpes, loc)
      }
    case Some(effs) =>
      // Case 2: We found the effects for the constructor.
      val tpes = effs.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
      Type.mkUnion(tpes, loc)
  }

  /** Returns the base effects of calling the given method `m`. */
  def getMethodEffs(m: Method, loc: SourceLocation): Type = methodEffs.get(m) match {
    case None =>
      // Case 1: No effects for the method. Try the class map.
      classEffs.get(m.getDeclaringClass) match {
        case None =>
          // Case 1.1: We use the IO effect by default.
          Type.IO
        case Some(effs) =>
          // Case 1.2: We use the class effects.
          val tpes = effs.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
          Type.mkUnion(tpes, loc)
      }
    case Some(effs) =>
      // Case 2: We found the effects for the method.
      val tpes = effs.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
      Type.mkUnion(tpes, loc)
  }

}
