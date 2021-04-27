/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.{Def, Root}
import ca.uwaterloo.flix.util.{InternalCompilerException, Verbosity}

import java.lang.reflect.Method

/**
  * Loads all the generated classes into the JVM and decorates the AST.
  */
object Bootstrap {

  /**
    * Loads all the generated classes into the JVM and decorates the AST.
    */
  def bootstrap(classes: Map[JvmName, JvmClass])(implicit flix: Flix, root: Root): Unit = {
    //
    // Load each class into the JVM in a fresh class loader.
    //
    val loadedClasses: Map[JvmName, Class[_]] = ??? // TODO(JLS): BytecodeLoader.loadAll(classes)

    //
    // Print the number of loaded classes, if debugging and verbosity is enabled.
    //
    if (flix.options.debug && flix.options.verbosity == Verbosity.Verbose) {
      Console.println(s"Loaded: ${loadedClasses.size} classes.")
    }

    flix.subphase("LoadMethods") {
      //
      // Computes a map from classes and method names to method objects.
      //
      // TODO: We should not load all method objects here. Only a subset. Need some notion of entry points.
      val allMethods = loadedClasses.foldLeft(Map.empty[Class[_], Map[String, Method]]) {
        case (macc, (_, clazz)) => macc + (clazz -> methodsOf(clazz))
      }

      //
      // Decorate each defn in the ast with its method object.
      //
      for ((sym, defn) <- root.defs; if SjvmOps.nonLaw(defn)) {
        // Retrieve the namespace info of sym.
        val nsInfo = SjvmOps.getNamespace(sym)

        // Retrieve the JVM name associated with the namespace.
        val nsJvmName: JvmName = ??? // TODO(JLS): SjvmOps.getNamespaceClassType(nsInfo).name

        // Retrieve the reflective class object.
        val nsClass = loadedClasses.getOrElse(nsJvmName, throw InternalCompilerException(s"Unknown namespace: '$nsJvmName'."))

        // Retrieve the method name of the symbol.
        val methodName: String = ??? // TODO(JLS): SjvmOps.getDefMethodNameInNamespaceClass(sym)

        // Retrieve the method object.
        val method = allMethods.get(nsClass) match {
          case None => throw InternalCompilerException(s"Class not found: '$nsClass'.")
          case Some(m) => m.get(methodName) match {
            case None => throw InternalCompilerException(s"Method not found: '$methodName'.")
            case Some(r) => r
          }
        }

        // And finally assign the method object to the definition.
        defn.method = method
      }
    }
  }

  /**
    * Returns a map from names to method objects for the given class `clazz`.
    */
  private def methodsOf(clazz: Class[_]): Map[String, Method] = {
    clazz.getMethods.foldLeft(Map.empty[String, Method]) {
      case (macc, method) =>
        if (method.isSynthetic)
          macc
        else
          macc + (method.getName -> method)
    }
  }

}
