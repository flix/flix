package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.NewObjectCompletion
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object NewObjectCompleter extends Completer {

  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[Completion] = {
    if (context.previousWord != "new") {
      Nil
    } else {
      val clazzName = if (context.word.startsWith("##")) context.word.drop(2) else context.word
      try {
        // TODO: Check that clazzName is type alias or imported with alias.
        newObjectCompletion(Class.forName(clazzName))
      } catch {
        case _: ClassNotFoundException => Nil
      }
    }
  }

  private def newObjectCompletion(clazz: Class[_])(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Option[NewObjectCompletion] = {
    // val label = if (alias.name.isBlank) clazz.getSimpleName else alias.name
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

  private def toCompletion(methodWithIndex: (java.lang.reflect.Method, Int))(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): String = {
    val (method, i) = methodWithIndex
    val name = method.getName
    val params = method.getParameters
      .map(p => s"${p.getName}: ${toTypeCompletion(p.getType)}")
      .mkString(", ")
    val result = toTypeCompletion(method.getReturnType)
    s"def $name($params): $result = $${$i:???}"
  }

  private def toTypeCompletion(clazz: Class[_])(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): String = {
    FormatType.formatType(Type.mkNative(clazz, SourceLocation.Unknown))
  }
}
