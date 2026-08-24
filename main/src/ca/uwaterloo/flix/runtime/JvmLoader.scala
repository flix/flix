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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{CrashHandler, Flix}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmClass
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException, JvmUtils}

import java.lang.constant.ClassDesc
import java.lang.reflect.{InvocationTargetException, Method}

/**
  * Loads the classes of a [[CompilationResult]] into the JVM.
  *
  * This is not part of the compiler pipeline: `Flix.codeGen` stops at bytecode.
  * Callers that want to *run* the compiled program (or its tests) invoke [[load]] explicitly.
  */
object JvmLoader {

  /**
    * Loads the classes of `result` into a fresh class loader and returns reflected handles to `main` and the tests.
    *
    * The class loader falls back to `result.flix.jarLoader` for classes from external JARs.
    *
    * A failure to load (or to find an entry point) is a compiler bug and is reported via [[CrashHandler]].
    * Exceptions thrown by the *program* itself, when `main` or a test is invoked, are not caught here.
    */
  def load(result: CompilationResult): LoadedProgram = try {
    implicit val flix: Flix = result.flix
    val root = result.root

    // Load each class into the JVM in a fresh class loader.
    implicit val loadedClasses: Map[ClassDesc, Class[?]] = loadAll(root.classes.values, flix.jarLoader)

    val tests = MapOps.mapValuesWithKey(root.tests) {
      case (sym, defn) => TestFn(sym, defn.isSkip, wrapTest(loadMethod(defn.className, defn.methodName)))
    }
    val main = root.main.map {
      case defn => wrapMain(loadMethod(defn.className, defn.methodName))
    }

    LoadedProgram(main, tests)
  } catch {
    case ex: Throwable =>
      CrashHandler.handleCrash(ex)(result.flix)
      throw ex
  }

  /** Wraps the reflected test `method` (of type `Unit -> t`) into a thunk. */
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

  /** Wraps the reflected main `method` (of type `Array[String] -> Unit`) into a function. */
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
  private def loadMethod(className: ClassDesc, methodName: String)(implicit loadedClasses: Map[ClassDesc, Class[?]]): Method = {
    val mainClass = loadedClasses.getOrElse(className, throw InternalCompilerException(s"Cannot find class '${ClassDescs.binaryNameOf(className)}'.", SourceLocation.Unknown))
    findMethod(mainClass, methodName).getOrElse(throw InternalCompilerException(s"Cannot find '$methodName' method of '${ClassDescs.binaryNameOf(className)}'.", SourceLocation.Unknown))
  }

  /** Returns a Method for `clazz.methodName` if possible. */
  private def findMethod(clazz: Class[?], methodName: String): Option[Method] = {
    JvmUtils.getMethods(clazz).find {
      case method => method.getName == methodName && !method.isSynthetic
    }
  }

  /** Loads the given JVM `classes` using a custom class loader that falls back to `jarLoader`. */
  private def loadAll(classes: Iterable[JvmClass], jarLoader: ClassLoader): Map[ClassDesc, Class[?]] = {
    // Compute a map from binary names (strings) to JvmClasses.
    val m = classes.foldLeft(Map.empty[String, JvmClass]) {
      case (macc, jvmClass) => macc + (ClassDescs.binaryNameOf(jvmClass.name) -> jvmClass)
    }

    // Instantiate the Flix class loader with this map.
    val loader = new FlixClassLoader(m, jarLoader)

    // Attempt to load each class using its binary name.
    classes.foldLeft(Map.empty[ClassDesc, Class[?]]) {
      case (macc, jvmClass) =>
        // Attempt to load class.
        val loadedClass = loader.loadClass(ClassDescs.binaryNameOf(jvmClass.name))
        macc + (jvmClass.name -> loadedClass)
    }
  }

}
