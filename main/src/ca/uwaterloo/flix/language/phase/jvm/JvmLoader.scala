/*
 * Copyright 2017 Magnus Madsen
 * Copyright 2025 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.BytecodeAst.*
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.runtime.TestFn
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmUtils}

import java.lang.reflect.{InvocationTargetException, Method}

object JvmLoader {

  case class LoaderResult(
                           main: Option[Array[String] => Unit],
                           tests: Map[Symbol.DefnSym, TestFn],
                           sources: Map[Source, SourceLocation]
                         )

  /**
    * Loads `classes` if enabled by [[Flix.options.loadClassFiles]] and returns handles to the methods.
    *
    * Additionally computes the total byte size of `classes`
    */
  def run(root: Root)(implicit flix: Flix): LoaderResult = {
    if (flix.options.loadClassFiles) {
      val (main, tests) = load(root)
      LoaderResult(main, tests, root.sources)
    } else {
      LoaderResult(None, Map.empty, root.sources)
    }
  }

  /** Returns the tests of `root`. */
  private def wrapTest(method: Method): () => AnyRef = {
    val parameterCount = method.getParameterCount
    val argsArray = Array(null: AnyRef)
    val argumentCount = argsArray.length
    if (argumentCount != parameterCount) {
      throw InternalCompilerException(s"Expected a method of $argumentCount parameters, but ${method.getName} has $parameterCount.", SourceLocation.Unknown)
    }

    () => {
      // Perform the method call using reflection.
      try {
        val result = method.invoke(null, argsArray *)
        result
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }
  }

  /** Loads the classes of `root` into the JVM. Returns `(main, tests)` reflected methods, based on `root`. */
  private def load(root: Root)(implicit flix: Flix): (Option[Array[String] => Unit], Map[Symbol.DefnSym, TestFn]) = {
    // Load each class into the JVM in a fresh class loader.
    implicit val loadedClasses: Map[JvmName, Class[?]] = loadAll(root.classes.values)

    val tests = MapOps.mapValuesWithKey(root.tests) {
      case (sym, defn) => TestFn(sym, defn.isSkip, wrapTest(loadMethod(defn.className, defn.methodName)))
    }
    val main = root.main.map {
      case defn => wrapMain(loadMethod(defn.className, defn.methodName))
    }

    (main, tests)
  }

  /** Returns the [[Method]] object of the main function of `root` if it is defined. */
  private def wrapMain(method: Method): Array[String] => Unit = {
    val parameterCount = method.getParameterCount
    val argumentCount = 1 // A single Array[String] argument.
    if (argumentCount != parameterCount) {
      throw InternalCompilerException(s"Expected a main method of $argumentCount parameters, but ${method.getName} has $parameterCount.", SourceLocation.Unknown)
    }

    (args: Array[String]) =>
      try {
        // Call the method, passing the argument array.
        method.invoke(null, args)
        ()
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
  }

  /** Returns the [[Method]] object for `className.methodName`. */
  private def loadMethod(className: JvmName, methodName: String)(implicit loadedClasses: Map[JvmName, Class[?]]): Method = {
    val mainClass = loadedClasses.getOrElse(className, throw InternalCompilerException(s"Cannot find class '${className.toBinaryName}'.", SourceLocation.Unknown))
    findMethod(mainClass, methodName).getOrElse(throw InternalCompilerException(s"Cannot find '$methodName' method of '${className.toBinaryName}'.", SourceLocation.Unknown))
  }

  /** Returns a Method for `clazz.methodName` if possible. */
  private def findMethod(clazz: Class[?], methodName: String): Option[Method] = {
    JvmUtils.getMethods(clazz).find {
      case method => method.getName == methodName && !method.isSynthetic
    }
  }

  /** Loads the given JVM `classes` using a custom class loader. */
  private def loadAll(classes: Iterable[JvmClass])(implicit flix: Flix): Map[JvmName, Class[?]] = {
    // Compute a map from binary names (strings) to JvmClasses.
    val m = classes.foldLeft(Map.empty[String, JvmClass]) {
      case (macc, jvmClass) => macc + (jvmClass.name.toBinaryName -> jvmClass)
    }

    // Instantiate the Flix class loader with this map.
    val loader = new FlixClassLoader(m)

    // Attempt to load each class using its internal name.
    classes.foldLeft(Map.empty[JvmName, Class[?]]) {
      case (macc, jvmClass) =>
        // Attempt to load class.
        val loadedClass = loader.loadClass(jvmClass.name.toBinaryName)
        macc + (jvmClass.name -> loadedClass)
    }
  }

}
