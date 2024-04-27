package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.NewObjectCompletion
import ca.uwaterloo.flix.language.ast.{SourceLocation, Ast, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object NewObjectCompleter extends Completer {

  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[Completion] = {
    println("NewObjectCompleter.getCompletions fired!")
    println("Checking debug code")
    val regex = raw"\s*n?e?w?\s+(?:##)?(?:.*\s+)*(.*)".r
    context.prefix match {
      case regex(clazz) =>
        println("Entered good path")
        println(s"Previous word is: ${context.previousWord}")
        val path = clazz.split('.').toList
        // Get completions for if we are currently typing the next package/class and if we have just finished typing a package
        val classNames = javaClassCompletionsFromPrefix(path)(root) ++ javaClassCompletionsFromPrefix(path.dropRight(1))(root)
        classNames.foreach(println)
        try {
          classNames.map(Class.forName).map(newObjectCompletion).filter(_.isDefined).map(_.get)
        } catch {
          case _: ClassNotFoundException => Nil
        }
      case _ => Nil
    }
  }

  private def newObjectCompletion(clazz: Class[_])(implicit flix: Flix): Option[NewObjectCompletion] = {
    val label = clazz.getSimpleName

    if (isAbstract(clazz)) {
      val completion = clazz.getMethods
        .filter(isAbstract)
        .zipWithIndex
        .map { case (m, i) => (m, i + 1) }
        .map(toCompletion)
        .mkString(System.lineSeparator())

      Some(NewObjectCompletion(label, s"$label {${System.lineSeparator()}$completion}"))
    } else
      None
  }

  private def isAbstract(clazz: Class[_]): Boolean = {
    val hasAbstractModifier = java.lang.reflect.Modifier.isAbstract(clazz.getModifiers)
    val isInterface = clazz.isInterface
    isInterface || hasAbstractModifier
  }

  private def isAbstract(method: java.lang.reflect.Method): Boolean = {
    java.lang.reflect.Modifier.isAbstract(method.getModifiers)
  }

  private def toCompletion(methodWithIndex: (java.lang.reflect.Method, Int))(implicit flix: Flix): String = {
    val (method, i) = methodWithIndex
    val name = method.getName
    val params = method.getParameters
      .map(p => s"${p.getName}: ${toTypeCompletion(p.getType)}")
      .mkString(", ")
    val result = toTypeCompletion(method.getReturnType)
    s"def $name($params): $result = $${$i:???}"
  }

  private def toTypeCompletion(clazz: Class[_])(implicit flix: Flix): String = {
    FormatType.formatType(Type.mkNative(clazz, SourceLocation.Unknown))
  }

  /**
    * Gets completions from a java path prefix
    */
  private def javaClassCompletionsFromPrefix(prefix: List[String])(implicit root: TypedAst.Root): Iterable[String] = {
    root.names(prefix).map(clazz => {
      prefix match {
        case Nil =>
          println("got nil case in javaClassCompletionsFromPrefix")
          println(prefix)
          clazz
        case v =>
          println(s"got case v with class $clazz")
          println(prefix)
          v.mkString("", ".", s".$clazz")

      }
    }
    )
  }
}
