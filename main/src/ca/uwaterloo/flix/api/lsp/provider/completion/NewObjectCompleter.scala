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
    val check = context.prefix match {
      case regex(clazz) =>
        println("Entered good path")
        val path = clazz.split('.').toList
        // Get completions for if we are currently typing the next package/class and if we have just finished typing a package
        val classNames = javaClassCompletionsFromPrefix(path)(root) ++ javaClassCompletionsFromPrefix(path.dropRight(1))(root)
        classNames.foreach(println)
      case _ => Nil
    }

    println(s"context.prefix: ${context.prefix}")
    println(s"trying to match context.prefix: ${regex.matches(context.prefix)}")
    println(check)

    val newPattern = raw".*\s*n?e?w?\s?.*".r
    if (!newPattern.matches(context.prefix)) {
      println("No matches for newPattern")
      Nil
    } else {
      println("Found match for newPattern")
      val wordPattern = "ne?w?".r
      val currentWordIsNew = wordPattern.matches(context.word)

      println(root.uses.filter { case (_, b) => b.exists(p => p.isInstanceOf[Ast.UseOrImport.Import]) })

      root.uses.foldLeft(List.empty[NewObjectCompletion]) {
        case (acc, (_, useOrImport)) => newObjectCompletions(useOrImport, currentWordIsNew) ::: acc
      }
    }
  }

  private def newObjectCompletions(useOrImports: List[Ast.UseOrImport], currentWordIsNew: Boolean)(implicit flix: Flix): List[NewObjectCompletion] = {
    useOrImports.foldLeft(List.empty[NewObjectCompletion]) {
      case (acc, x) => x match {
        case _: Ast.UseOrImport.Use => acc
        case imprt: Ast.UseOrImport.Import => newObjectCompletion(imprt, currentWordIsNew) match {
          case Some(v) => v :: acc
          case None => acc
        }
      }
    }
  }

  private def newObjectCompletion(imprt: Ast.UseOrImport.Import, currentWordIsNew: Boolean)(implicit flix: Flix): Option[NewObjectCompletion] = imprt match {
    case Ast.UseOrImport.Import(clazz, alias, _) =>
      val label = if (alias.name.isBlank) clazz.getSimpleName else alias.name
      val includeNew = if (currentWordIsNew) "new " else ""

      if (isAbstract(clazz)) {
        val completion = clazz.getMethods
          .filter(isAbstract)
          .zipWithIndex
          .map { case (m, i) => (m, i + 1) }
          .map(toCompletion)
          .mkString(System.lineSeparator())
        Some(NewObjectCompletion(label, s"$includeNew $label {${System.lineSeparator()}$completion}"))
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
