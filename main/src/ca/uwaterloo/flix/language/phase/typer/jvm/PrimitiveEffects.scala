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
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.language.ast.jvm.JavaMethod
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{InternalCompilerException, LocalResource}
import org.json4s.JsonAST.*
import org.json4s.jvalue2monadic
import org.json4s.native.JsonMethods.parse

import java.lang.constant.ClassDesc

object PrimitiveEffects {

  /** The path to the package effects. */
  private val PackageEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/jvm/PrimitiveEffects.Packages.json"

  /** The path to the class effects. */
  private val ClassEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/jvm/PrimitiveEffects.Classes.json"

  /** The path to the constructor effects. */
  private val ConstructorEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/jvm/PrimitiveEffects.Constructors.json"

  /** The path to the method effects. */
  private val MethodEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/jvm/PrimitiveEffects.Methods.json"

  /**
    * A pre-computed map from package names to effects.
    */
  private val packageEffs: Map[String, Set[Symbol.EffSym]] = loadPackageEffs()

  /**
    * A pre-computed map from classes to effects.
    *
    * If there is are specific effect(s) for a constructor or method then we use the effects for the entire class.
    */
  private val classEffs: Map[ClassDesc, Set[Symbol.EffSym]] = loadClassEffs()

  /**
    * A pre-computed map from classes to the effects of their constructors.
    *
    * The effects apply to every constructor of the class.
    */
  private val constructorEffs: Map[ClassDesc, Set[Symbol.EffSym]] = loadConstructorEffs()

  /**
    * A pre-computed map from methods, identified by their declaring class and name, to effects.
    *
    * The effects apply to every overload of the method.
    */
  private val methodEffs: Map[(ClassDesc, String), Set[Symbol.EffSym]] = loadMethodEffs()

  /**
    * Returns the primitive effects of calling the given constructor `c`.
    */
  def getConstructorEffs(c: JavaMethod, loc: SourceLocation): Type = constructorEffs.get(c.ref.owner) match {
    case None =>
      // Case 1: No effects for the constructor. Try the class map.
      getClassAndPackageEffs(c.ref.owner, loc)
    case Some(effs) =>
      // Case 2: We found the effects for the constructor.
      toEffSet(effs, loc)
  }

  /**
    * Returns the primitive effects of calling the given method `m`.
    */
  def getMethodEffs(m: JavaMethod, loc: SourceLocation): Type = methodEffs.get((m.ref.owner, m.ref.name)) match {
    case None =>
      // Case 1: No effects for the method. Try the class map.
      getClassAndPackageEffs(m.ref.owner, loc)
    case Some(effs) =>
      // Case 2: We found the effects for the method.
      toEffSet(effs, loc)
  }

  /**
    * Returns the primitive effects of the class `c` if they exist.
    * Defaults to [[getPackageEffs]] if nothing was found.
    */
  private def getClassAndPackageEffs(c: ClassDesc, loc: SourceLocation): Type = {
    classEffs.get(c) match {
      case None =>
        // Case 1.1: No effects for the class. Try the package.
        getPackageEffs(c.packageName(), loc)
      case Some(effs) =>
        // Case 1.2: We use the class effects.
        toEffSet(effs, loc)
    }
  }

  /**
    * Returns the primitive effs of the package named `p`.
    * Defaults to [[Type.IO]] if nothing was found.
    */
  private def getPackageEffs(p: String, loc: SourceLocation): Type = {
    packageEffs.get(p) match {
      case None =>
        // Case 1.1.1: No effects for the package. Use the IO effect by default.
        Type.IO
      case Some(effs) =>
        // Case 1.1.2: We use the package effects.
        toEffSet(effs, loc)
    }
  }

  /**
    * Returns the set of effects represented by `effs`.
    */
  private def toEffSet(effs: Set[Symbol.EffSym], loc: SourceLocation): Type = {
    val tpes = effs.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym, Kind.Eff), loc))
    Type.mkUnion(tpes, loc)
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "packages": {
    *     "java.lang.net": "Net, IO"
    *   }
    * }
    * }}}
    */
  private def loadPackageEffs(): Map[String, Set[Symbol.EffSym]] = {
    val data = LocalResource.get(PackageEffsPath)
    val json = parse(data)

    val m = json \\ "packages" match {
      case JObject(l) => l.map {
        case (packageName, JString(s)) =>
          val effSet = parseEffSet(s)
          (packageName, effSet)
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "classes": {
    *     "java.lang.ProcessBuilder": "Exec, FsRead",
    *     "java.lang.reflect.Method": "Sys"
    *   }
    * }
    * }}}
    */
  private def loadClassEffs(): Map[ClassDesc, Set[Symbol.EffSym]] = {
    val data = LocalResource.get(ClassEffsPath)
    val json = parse(data)

    val m = json \\ "classes" match {
      case JObject(l) => l.map {
        case (className, JString(s)) =>
          val desc = ClassDesc.of(className)
          val effSet = parseEffSet(s)
          (desc, effSet)
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "constructors": {
    *     "java.lang.ProcessBuilder": "Exec",
    *     "java.net.URL": "Net"
    *   }
    * }
    * }}}
    *
    * Note: The effect set applies to *ALL* constructors of the class.
    */
  private def loadConstructorEffs(): Map[ClassDesc, Set[Symbol.EffSym]] = {
    val data = LocalResource.get(ConstructorEffsPath)
    val json = parse(data)

    val m = json \\ "constructors" match {
      case JObject(l) => l.map {
        case (className, JString(s)) =>
          val desc = ClassDesc.of(className)
          val effSet = parseEffSet(s)
          (desc, effSet)
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "methods": {
    *     "java.lang.System::exit": "Sys"
    *   }
    * }
    * }}}
    *
    * Note: The class must be the class that *declares* the method, not a subclass that inherits it,
    * since a resolved [[JavaMethod]] refers to its declaring class. TestPrimitiveEffects enforces this.
    *
    * Note: The effect set applies to *ALL* overloads of the method.
    */
  private def loadMethodEffs(): Map[(ClassDesc, String), Set[Symbol.EffSym]] = {
    val data = LocalResource.get(MethodEffsPath)
    val json = parse(data)

    val m = json \\ "methods" match {
      case JObject(l) => l.map {
        case (classNameAndMethod, JString(s)) =>
          val cc = classNameAndMethod.indexOf("::")
          val className = classNameAndMethod.substring(0, cc)
          val methodName = classNameAndMethod.substring(cc + 2)
          val desc = ClassDesc.of(className)
          val effSet = parseEffSet(s)
          ((desc, methodName), effSet)
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
   * Returns the given comma-separated string of effect symbols as a set of [[Symbol.EffSym]].
   *
   * Returns the empty set if the string is empty.
   */
  private def parseEffSet(s: String): Set[Symbol.EffSym] = {
    if (s.trim.isEmpty)
      Set.empty
    else
      s.split(",").map(_.trim).map(Symbol.parsePrimitiveEff).toSet
  }
}
