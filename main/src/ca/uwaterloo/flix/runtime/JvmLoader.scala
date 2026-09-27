/*
 * Copyright 2017 Magnus Madsen
 * Copyright 2025 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{CrashHandler, Flix}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.jvm.ClassDescs
import ca.uwaterloo.flix.language.phase.jvm.JvmClass
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, NativeImage}

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
    * Throws [[UnsupportedOperationException]] if running inside a GraalVM native image, which
    * refuses `ClassLoader.defineClass` at run time.
    *
    * A failure to load (or to find an entry point) is a compiler bug and is reported via [[CrashHandler]].
    * Exceptions thrown by the *program* itself, when `main` or a test is invoked, are not caught here.
    */
  def load(result: CompilationResult): LoadedProgram = {
    // A native image refuses ClassLoader.defineClass, so bail out before the crash handler below.
    if (NativeImage.GraalEnabled) {
      val msg = "Loading a compiled program is not supported in the native image. You must run the flix.jar in the JVM for this action."
      throw new UnsupportedOperationException(msg)
    }

    try {
      implicit val flix: Flix = result.flix
      val root = result.root

      // Load each class into the JVM in a fresh class loader.
      implicit val loadedClasses: Map[ClassDesc, Class[?]] = loadAll(root.classes.values, flix.jarLoader)

      // The methods are looked up here, once, so that a missing one is reported at load time.
      val tests = MapOps.mapValuesWithKey(root.tests) {
        case (sym, defn) =>
          val method = loadMethod(defn.className, defn.methodName)
          TestFn(sym, defn.isSkip, () => invoke(method))
      }
      val main = root.main.map {
        case defn =>
          val method = loadMethod(defn.className, defn.methodName)
          (args: Array[String]) => invoke(method, args)
      }

      LoadedProgram(main, tests)
    } catch {
      case ex: Throwable =>
        CrashHandler.handleCrash(ex)(result.flix)
        throw ex
    }
  }

  /** Invokes the static `method` with `args`, rethrowing any exception the program itself throws. */
  private def invoke(method: Method, args: AnyRef*): Unit = {
    try {
      method.invoke(null, args *)
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
  private def findMethod(clazz: Class[?], methodName: String): Option[Method] =
    clazz.getMethods.find(method => method.getName == methodName && !method.isSynthetic)

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
