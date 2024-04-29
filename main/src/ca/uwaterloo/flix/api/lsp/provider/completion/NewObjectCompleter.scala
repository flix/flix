package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.NewObjectCompletion
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object NewObjectCompleter extends Completer {

  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[NewObjectCompletion] = {
    val regex = raw"\s*n?e?w?\s+(?:##)?(?:.*\s+)*(.*)".r
    context.prefix match {
      case regex(clazz) =>
        val path = clazz.replaceFirst("##", "").split('.').toList
        // Get completions for if we are currently typing the next package/class and if we have just finished typing a package
        val packageNames = javaClassCompletionsFromPrefix(path)(root)
        val classNames = packageNames ++ javaClassCompletionsFromPrefix(path.dropRight(1))(root)
        classNames.map { c =>
            try {
              Some(Class.forName(c.replaceAll("\\[L", "")))
            } catch {
              case _: ClassNotFoundException => None
            }
          }
          .map(c => c.flatMap(newObjectCompletion(context)))
          .filter(_.isDefined)
          .map(_.get)
      case _ => Nil
    }
  }

  private def newObjectCompletion(context: CompletionContext)(clazz: Class[_])(implicit flix: Flix): Option[NewObjectCompletion] = {
    val name = clazz.getName

    if (isAbstract(clazz)) {
      val completion = clazz.getMethods
        .filter(isAbstract)
        .zipWithIndex
        .map { case (m, i) => (m, i + 1) }
        .map(toMethodCompletion(name))
        .mkString(System.lineSeparator())

      Some(NewObjectCompletion(name, s"$name {${System.lineSeparator()}$completion${System.lineSeparator()}}"))
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

  private def toMethodCompletion(className: String)(methodWithIndex: (java.lang.reflect.Method, Int))(implicit flix: Flix): String = {
    val (method, i) = methodWithIndex
    val name = method.getName
    val params = method.getParameters
      .map(p => s"${p.getName}: ${toTypeCompletion(p.getType)}")
      .prepended(s"_this: ##$className")
      .mkString(", ")
    val result = toTypeCompletion(method.getReturnType)
    val indentation = "    "
    indentation + s"pub def $name($params): $result = $${$i:???}${System.lineSeparator()}"
  }

  private def toTypeCompletion(clazz: Class[_])(implicit flix: Flix): String = {
    val tpe = Type.getFlixType(clazz)
    val isNative = tpe match {
      case Type.Cst(TypeConstructor.Native(_), _) => true
      case _ => false
    }
    val prepend = if (isNative) "##" else ""
    // TODO: Handle arrays
    prepend ++ FormatType.formatType(tpe)
  }

  /**
    * Gets completions from a java path prefix
    */
  private def javaClassCompletionsFromPrefix(prefix: List[String])(implicit root: TypedAst.Root): Iterable[String] = {
    root.names(prefix).map(clazz => {
      prefix match {
        case Nil => clazz
        case v =>
          println(v.mkString("", ".", s".$clazz"))
          v.mkString("", ".", s".$clazz")
      }
    }
    )
  }
}
