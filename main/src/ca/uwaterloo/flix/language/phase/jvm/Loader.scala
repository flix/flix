package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.Root
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.{InternalCompilerException, JvmUtils}

import java.lang.reflect.{InvocationTargetException, Method}

/**
  * Loads all the generated classes into the JVM and decorates the AST.
  */
object Loader {

  /**
    * Loads all the generated classes into the JVM and decorates the AST.
    * The main functions of `Main.class` is returned if it exists.
    */
  def load(classes: Map[JvmName, JvmClass])(implicit flix: Flix, root: Root): Option[Array[String] => Unit] = {
    //
    // Load each class into the JVM in a fresh class loader.
    //
    val loadedClasses = BytecodeLoader.loadAll(classes)

    //
    // Computes a map from classes and method names to method objects.
    //
    // TODO: We should not load all method objects here. Only a subset. Need some notion of entry points.
    val allMethods = loadedClasses.foldLeft(Map.empty[Class[?], Map[String, Method]]) {
      case (macc, (_, clazz)) => macc + (clazz -> methodsOf(clazz))
    }

    //
    // Decorate each defn in the ast with its method object unless its a closure.
    //
    for ((sym, defn) <- root.defs if root.reachable.contains(sym)) {
      // Retrieve the namespace name
      val nsName = JvmOps.getNamespaceName(sym)

      // Retrieve the reflective class object.
      val nsClass = loadedClasses.getOrElse(nsName, throw InternalCompilerException(s"Unknown namespace: '$nsName'.", sym.loc))

      // Retrieve the method name of the symbol.
      val methodName = JvmOps.getDefMethodNameInNamespaceClass(defn)

      // Retrieve the method object.
      val method = allMethods.get(nsClass) match {
        case None => throw InternalCompilerException(s"Class not found: '$nsClass'.", sym.loc)
        case Some(m) => m.get(methodName) match {
          case None => throw InternalCompilerException(s"Method not found: '$methodName'.", sym.loc)
          case Some(r) => r
        }
      }

      // And finally assign the method object to the definition.
      defn.method = method
    }

    if (root.getMain.isDefined) {
      val mainName = JvmName.Main
      val mainInternalName = mainName.toInternalName
      val mainClass = loadedClasses.getOrElse(mainName, throw InternalCompilerException(s"Class not found: '$mainInternalName'.", SourceLocation.Unknown))
      val mainMethods = allMethods.getOrElse(mainClass, throw InternalCompilerException(s"methods for '$mainInternalName' not found.", SourceLocation.Unknown))
      val mainMethod = mainMethods.getOrElse("main", throw InternalCompilerException(s"Cannot find 'main' method of '$mainInternalName'", SourceLocation.Unknown))

      // This is a specialized version of the link function in JvmBackend
      def mainFunction(args: Array[String]): Unit = {
        try {
          // Call the method passing the argument array.
          mainMethod.invoke(null, args)
          ()
        } catch {
          case e: InvocationTargetException =>
            // Rethrow the underlying exception.
            throw e.getTargetException
        }
      }

      Some(mainFunction)
    } else None
  }

  /**
    * Returns a map from names to method objects for the given class `clazz`.
    */
  private def methodsOf(clazz: Class[?]): Map[String, Method] = {
    JvmUtils.getMethods(clazz).foldLeft(Map.empty[String, Method]) {
      case (macc, method) =>
        if (method.isSynthetic)
          macc
        else
          macc + (method.getName -> method)
    }
  }

}
