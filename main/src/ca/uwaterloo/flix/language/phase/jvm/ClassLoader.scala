/*
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
import ca.uwaterloo.flix.language.ast.ReducedAst.Root
import ca.uwaterloo.flix.language.ast.{ReducedAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmUtils}

import java.lang.reflect.{InvocationTargetException, Method}

object ClassLoader {

  case class LoaderResult(
                           main: Option[Array[String] => Unit],
                           defs: Map[Symbol.DefnSym, () => AnyRef],
                           byteSize: Int
                         )

  /**
    * Loads `classes` if enabled by [[Flix.options.loadClassFiles]] and returns handles to the methods.
    *
    * Additionally computes the total byte size of `classes`
    *
    * Loader relies on two kind of classes to load the entrypoints of `root`
    *   - for every entrypoint, there should exist a matching namespace function (see [[BackendObjType.Namespace.ShimMethod]])
    *   - if the main function is defined, a main class should be defined (see [[BackendObjType.Main.MainMethod]])
    */
  def run(root: Root, classes: List[JvmClass])(implicit flix: Flix): (Root, LoaderResult) = flix.phase("Loader") {
    implicit val r: Root = root

    // Collect code size for performance tracking.
    val outputBytes = classes.map(_.bytecode.length).sum

    if (flix.options.loadClassFiles) {
      val main = load(classes)
      (root, LoaderResult(main, getCompiledDefs(root), outputBytes))
    } else {
      (root, LoaderResult(None, Map.empty, outputBytes))
    }
  }(AstPrinter.DebugNoOp())

  /** Returns the non-closure, executable jvm functions of `root`. */
  private def getCompiledDefs(root: Root): Map[Symbol.DefnSym, () => AnyRef] = {
    root.defs.filter(_._2.cparams.isEmpty).map {
      case (sym, _) =>
        val args: Array[AnyRef] = Array(null)
        (sym, () => link(sym, root)(args))
    }
  }

  /** Returns a function object for `sym`. */
  private def link(sym: Symbol.DefnSym, root: Root): java.util.function.Function[Array[AnyRef], AnyRef] = {
    val defn = root.defs(sym)
    // Check that the method has been initialized.
    if (defn.method == null) throw InternalCompilerException(s"Linking error: '$sym' has an uninitialized method.", SourceLocation.Unknown)

    (args: Array[AnyRef]) => {
      // Convert and verify `args`.
      val argsArray = if (args.isEmpty) Array(null: AnyRef) else args
      val parameterCount = defn.method.getParameterCount
      val argumentCount = argsArray.length
      if (argumentCount != parameterCount) {
        throw new RuntimeException(s"Expected $parameterCount arguments, but got: $argumentCount for method ${defn.method.getName}.")
      }

      // Perform the method call using reflection.
      try {
        val result = defn.method.invoke(null, argsArray *)
        result
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }
  }

  /**
    * Loads all the generated classes into the JVM and decorates the AST.
    * The main functions of `Main.class` is returned if it exists.
    */
  private def load(classes: List[JvmClass])(implicit flix: Flix, root: Root): Option[Array[String] => Unit] = {
    // Load each class into the JVM in a fresh class loader.
    implicit val loadedClasses: Map[JvmName, Class[?]] = loadAll(classes)

    // Computes a map from classes and method names to method objects.
    // TODO: We should not load all method objects here. Only a subset. Need some notion of entry points.
    implicit val allMethods: Map[Class[?], Map[String, Method]] = loadedClasses.foldLeft(Map.empty[Class[?], Map[String, Method]]) {
      case (macc, (_, clazz)) => macc + (clazz -> methodsOf(clazz))
    }

    // Decorate each entrypoint def in the AST.
    for ((sym, defn) <- root.defs if root.entryPoints.contains(sym)) {
      // Assign the method object to the definition.
      defn.method = loadDef(defn)
    }

    loadMain(root)
  }

  /** Returns the [[Method]] object of `defn`. */
  private def loadDef(defn: ReducedAst.Def)(implicit loadedClasses: Map[JvmName, Class[?]], allMethods: Map[Class[?], Map[String, Method]]): Method = {
    val ns = BackendObjType.Namespace(defn.sym.namespace)
    val methodName = ns.ShimMethod(defn).name

    loadMethod(ns.jvmName, methodName)
  }

  /** Returns the [[Method]] object of the main function of `root` if it is defined. */
  private def loadMain(root: Root)(implicit loadedClasses: Map[JvmName, Class[?]], allMethods: Map[Class[?], Map[String, Method]]): Option[Array[String] => Unit] = {
    if (root.getMain.isEmpty) return None

    val mainMethod = loadMethod(BackendObjType.Main.jvmName, BackendObjType.Main.MainMethod.name)

    // This is a specialized version of the link function in BytecodeHandler.
    def mainFunction(args: Array[String]): Unit = {
      try {
        // Call the method, passing the argument array.
        mainMethod.invoke(null, args)
        ()
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }

    Some(mainFunction)
  }

  /** Returns the [[Method]] object for def `methodName` in `className`. */
  private def loadMethod(className: JvmName, methodName: String)(implicit loadedClasses: Map[JvmName, Class[?]], allMethods: Map[Class[?], Map[String, Method]]): Method = {
    val mainClass = loadedClasses.getOrElse(className, throw InternalCompilerException(s"Class not found: '${className.toBinaryName}'.", SourceLocation.Unknown))
    val mainMethods = allMethods.getOrElse(mainClass, throw InternalCompilerException(s"methods for '${className.toBinaryName}' not found.", SourceLocation.Unknown))
    mainMethods.getOrElse(methodName, throw InternalCompilerException(s"Cannot find '$methodName' method of '${className.toBinaryName}'", SourceLocation.Unknown))
  }

  /** Returns a map from names to method objects for the given class `clazz`. */
  private def methodsOf(clazz: Class[?]): Map[String, Method] = {
    JvmUtils.getMethods(clazz).foldLeft(Map.empty[String, Method]) {
      case (macc, method) =>
        if (method.isSynthetic) macc
        else macc + (method.getName -> method)
    }
  }

  /** Loads the given JVM `classes` using a custom class loader. */
  private def loadAll(classes: List[JvmClass])(implicit flix: Flix): Map[JvmName, Class[?]] = {
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
