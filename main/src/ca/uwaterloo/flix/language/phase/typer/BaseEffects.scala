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
import ca.uwaterloo.flix.util.{InternalCompilerException, LocalResource}
import org.json4s.JArray
import org.json4s.JsonAST.*
import org.json4s.jvalue2monadic
import org.json4s.native.JsonMethods.parse

import java.lang.reflect.{Constructor, Method}

object BaseEffects {

  /**
    * A pre-computed map from constructors to effects.
    */
  private val constructorEffs: Map[Constructor[?], Set[Symbol.EffectSym]] = Map.empty ++
    classOf[java.net.URL].getConstructors.map(c => (c, Set(Symbol.Net))) // TODO: Load from JSON

  /**
    * A pre-computed map from methods to effects.
    */
  private val methodEffs: Map[Method, Set[Symbol.EffectSym]] = Map(
    classOf[java.lang.System].getMethod("currentTimeMillis") -> Set(Symbol.Time) // TODO: Load from JSON
  )

  /**
    * A pre-computed map from classes to effects.
    *
    * If there is are specific effect(s) for a constructor or method then we use the effects for the entire class.
    */
  private val classEffs: Map[Class[?], Set[Symbol.EffectSym]] = loadClassEffs()

  /**
    * Returns the base effects of calling the given constructor `c`.
    */
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

  /**
    * Returns the base effects of calling the given method `m`.
    */
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

  /**
    * Load class effects from JSON.
    */
  private def loadClassEffs(): Map[Class[?], Set[Symbol.EffectSym]] = {
    val data = LocalResource.get("/src/ca/uwaterloo/flix/language/phase/typer/BaseEffects.ClassEffs.json")
    val json = parse(data)

    val classEffs = json \\ "classes" match {
      case JArray(a) => a.map(parseClassAndEffSet)
      case _ => throw InternalCompilerException("Unexpected non-array value of the 'classes' field.", SourceLocation.Unknown)
    }

    classEffs.toMap
  }

  /**
    * Parses a JSON object of the form:
    *
    * {{{
    * {
    *   "class": "java.lang.ProcessBuilder",
    *   "effects": [
    *     "Exec"
    *   ]
    * }
    * }}}
    *
    * into a pair (classOf[ProcessBuilder], Set(Exec)).
    */
  private def parseClassAndEffSet(json: JValue): (Class[?], Set[Symbol.EffectSym]) = {
    val className = json \\ "class" match {
      case JString(s) => s
      case _ => throw InternalCompilerException("Unexpected non-string value of the 'class' field.", SourceLocation.Unknown)
    }
    val effects = json \\ "effects" match {
      case JArray(a) => a.map {
        case JString(eff) => eff
        case _ => throw InternalCompilerException("Unexpected non-string value of an effect.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected non-array value of the 'effects' field.", SourceLocation.Unknown)
    }

    (Class.forName(className), effects.map(Symbol.parseBaseEff).toSet)
  }

}
