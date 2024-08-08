package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.{ImportCompletion, NewObjectCompletion}
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object NewObjectCompleter extends Completer {

  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
    val regex = raw"\s*n?e?w?\s+(?:##)?(?:.*\s+)*(.*)".r
    context.prefix match {
      case regex(clazz) =>
        val path = clazz.replaceFirst("##", "").split('.').toList
        // Get completions for if we are currently typing the next package/class and if we have just finished typing a package
        val names = javaClassCompletionsFromPrefix(path)(root) ++ javaClassCompletionsFromPrefix(path.dropRight(1))(root)
        mkCompletions(context, names)
      case _ => Nil
    }
  }

  private def mkCompletions(context: CompletionContext, names: Iterable[String])(implicit flix: Flix) = {
    names.map { name =>
        try {
          val clazz = Class.forName(name.replaceAll("\\[L", ""))
          newObjectCompletion(context, clazz)
        } catch {
          case _: ClassNotFoundException =>
            // A package/class was found by javaClassCompletionsFromPrefix but it is not yet a valid
            // class, so we change it to a ClassCompletion so VSCode can assist the user in finding the
            // correct package/class.
            Some(ImportCompletion(name))
        }
      }
      .filter(_.isDefined)
      .map(_.get)
  }

  private def newObjectCompletion(context: CompletionContext, clazz: Class[_])(implicit flix: Flix): Option[NewObjectCompletion] = {
    val name = clazz.getName
    val prependHash = if (context.prefix.contains(s"##")) "" else "##"

    if (isAbstractClass(clazz) && isPublicClass(clazz)) {
      val completion = clazz.getMethods
        .filter(isAbstractMethod)
        .filter(isPublicMethod)
        .zipWithIndex
        .map { case (m, i) => (m, i + 1) }
        .map(toMethodCompletion(name))
        .mkString(System.lineSeparator())

      Some(NewObjectCompletion(name, s"$prependHash$name {${System.lineSeparator()}${System.lineSeparator()}$completion${System.lineSeparator()}}"))
    } else
      None
  }

  private def isAbstractClass(clazz: Class[_]): Boolean = {
    val hasAbstractModifier = java.lang.reflect.Modifier.isAbstract(clazz.getModifiers)
    val isInterface = clazz.isInterface
    isInterface || hasAbstractModifier
  }

  private def isAbstractMethod(method: java.lang.reflect.Method): Boolean = {
    java.lang.reflect.Modifier.isAbstract(method.getModifiers)
  }

  private def isPublicClass(clazz: Class[_]): Boolean = {
    java.lang.reflect.Modifier.isPublic(clazz.getModifiers)
  }

  private def isPublicMethod(method: java.lang.reflect.Method): Boolean = {
    java.lang.reflect.Modifier.isPublic(method.getModifiers)
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
    indentation + s"def $name($params): $result \\ IO = $${$i:???}${System.lineSeparator()}"
  }

  private def toTypeCompletion(clazz: Class[_])(implicit flix: Flix): String = {
    val tpe = Type.getFlixType(clazz)
    val isNative = tpe match {
      case Type.Cst(TypeConstructor.Native(_), _) => true
      case _ => false
    }
    val prependHash = if (isNative) s"##" else ""
    // TODO: Handle arrays
    prependHash ++ FormatType.formatType(tpe)
  }

  /**
    * Gets completions from a java path prefix
    */
  private def javaClassCompletionsFromPrefix(prefix: List[String])(implicit root: TypedAst.Root): Iterable[String] = {
    root.names(prefix).map(clazz => {
      prefix match {
        case Nil => clazz
        case v => v.mkString("", ".", s".$clazz")
      }
    }
    )
  }
}
