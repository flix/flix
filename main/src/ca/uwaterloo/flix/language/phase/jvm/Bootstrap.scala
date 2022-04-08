package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.reflect.{InvocationTargetException, Method}

/**
  * Loads all the generated classes into the JVM and decorates the AST.
  */
object Bootstrap {

  /**
    * Loads all the generated classes into the JVM and decorates the AST.
    */
  def bootstrap(classes: Map[JvmName, JvmClass])(implicit flix: Flix, root: Root): Option[Array[String] => Unit] = {
    //
    // Load each class into the JVM in a fresh class loader.
    //
    val loadedClasses = BytecodeLoader.loadAll(classes)

    //
    // Print the number of loaded classes, if debugging and verbosity is enabled.
    //
    if (flix.options.debug) {
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
      for ((sym, defn) <- root.defs) {
        // Retrieve the namespace info of sym.
        val nsInfo = JvmOps.getNamespace(sym)

        // Retrieve the JVM name associated with the namespace.
        val nsJvmName = JvmOps.getNamespaceClassType(nsInfo).name

        // Retrieve the reflective class object.
        val nsClass = loadedClasses.getOrElse(nsJvmName, throw InternalCompilerException(s"Unknown namespace: '$nsJvmName'."))

        // Retrieve the method name of the symbol.
        val methodName = JvmOps.getDefMethodNameInNamespaceClass(sym)

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
      root.entryPoint.map { _ =>
        val mainName = JvmOps.getMainClassType().name
        val mainClass = loadedClasses.getOrElse(mainName, throw InternalCompilerException(s"Class not found: '${mainName.toInternalName}'."))
        val mainMethods = allMethods.getOrElse(mainClass, throw InternalCompilerException(s"methods for '${mainName.toInternalName}' not found."))
        val mainMethod = mainMethods.getOrElse("main", throw InternalCompilerException(s"Cannot find 'main' method of '${mainName.toInternalName}'"))
        (args: Array[String]) => {
          try {
            // Call the method passing the argument array.
            mainMethod.invoke(null, args)
          } catch {
            case e: InvocationTargetException =>
              // Rethrow the underlying exception.
              throw e.getTargetException
          }
        }
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
