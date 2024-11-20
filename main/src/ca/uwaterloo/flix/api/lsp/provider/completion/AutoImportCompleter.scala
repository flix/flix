package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.AutoImportCompletion
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.AnchorPosition
import ca.uwaterloo.flix.language.errors.ResolutionError

object AutoImportCompleter {

  /**
   * Returns a list of import completions to auto complete the class name and import the java class.
   *
   * Example:
   *  If we have an undefined name which is the prefix of an existing java class
   *
   *  {{{
   *    let s = Mat // undefined name error
   *  }}}
   *
   *  We propose to complete the name to `Math` and import the class `java.lang.Math`
   *
   *  {{{
   *    import java.lang.Math
   *    ...
   *    let s = Math
   *  }}}
   */
  def getCompletions(err: ResolutionError.UndefinedName)(implicit root: Root): Iterable[AutoImportCompletion] =
    javaClassCompletionsByClass(err.qn.ident.name, err.ap)

  def getCompletions(err: ResolutionError.UndefinedType)(implicit root: Root): Iterable[AutoImportCompletion] =
    javaClassCompletionsByClass(err.qn.ident.name, err.ap)

  /**
   * Gets completions from a java class prefix.
   */
  private def javaClassCompletionsByClass(prefix: String, ap: AnchorPosition)(implicit root: Root): Iterable[AutoImportCompletion] = {
    val availableClasses = root.availableClasses.byClass.m
    availableClasses.keys.filter(_.startsWith(prefix)).flatMap { className =>
      availableClasses(className).map{path =>
        val completePath = path.mkString(".") + "." + className
        AutoImportCompletion(className, completePath, ap, Some(completePath))
      }
    }
  }
}
