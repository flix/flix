package ca.uwaterloo.flix.language.phase.jvm

import java.lang.reflect.Method

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import ca.uwaterloo.flix.util.{InternalCompilerException, Verbosity}

/**
  * Loads all the generated classes into the JVM and decorates the AST.
  */
object Bootstrap {

  /**
    * Loads all the generated classes into the JVM and decorates the AST.
    */
  def bootstrap(classes: Map[JvmName, JvmClass])(implicit flix: Flix, root: Root): Unit = flix.subphase("BootstrapClasses") {
    //
    // Load each class into the JVM in a fresh class loader.
    //
    val loadedClasses = BytecodeLoader.loadAll(classes)

    //
    // Print the number of loaded classes, if debugging and verbosity is enabled.
    //
    if (flix.options.debug && flix.options.verbosity == Verbosity.Verbose) {
      Console.println(s"Loaded: ${loadedClasses.size} classes.")
    }

    //
    // Decorate each defn in the ast with its method object.
    //
    for ((sym, defn) <- root.defs; if JvmOps.nonLaw(defn)) {
      // Retrieve the namespace info of sym.
      val nsInfo = JvmOps.getNamespace(sym)

      // Retrieve the JVM name associated with the namespace.
      val nsJvmName = JvmOps.getNamespaceClassType(nsInfo).name

      // Retrieve the reflective class object.
      val nsClass = loadedClasses.getOrElse(nsJvmName, throw InternalCompilerException(s"Unknown namespace: '$nsJvmName'."))

      // Retrieve the method name of the symbol.
      val methodName = JvmOps.getDefMethodNameInNamespaceClass(sym)

      // Retrieve the method object.
      // TODO: Magnus: This has O(n^2) complexity. We should fix that.
      val method = findMethod(methodName, nsClass.getMethods)

      // And finally assign the method object to the definition.
      defn.method = method
    }

  }

  /**
    * Returns the method named `needle` in the `haystack`.
    */
  private def findMethod(needle: String, haystack: Array[Method]): Method = {
    haystack.find(_.getName == needle) match {
      case None =>
        throw InternalCompilerException(s"Method not found: '$needle'.")
      case Some(m) => m
    }
  }

}
