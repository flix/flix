package ca.uwaterloo.flix.tools.fmt.rules

import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.tools.fmt.{Doc, PrettyPrinter}

/**
  * A formatter module owns a cohesive set of formatting rules for a single
  * category of [[TreeKind]].
  *
  * A module is a pure value — it declares a [[rules]] Map and nothing else.
  * There is no registration, no side effects, and no dependency on [[PrettyPrinter]]
  * being initialised first. A module can be constructed, inspected, and tested
  * in complete isolation.
  *
  * The [[rules]] Map keys are JVM [[Class]] objects obtained via:
  *   - `someKind.getClass`   for a specific case object  (e.g. TreeKind.Expr.IfThenElse)
  *   - `classOf[SomeTrait]`  for a category catch-all    (e.g. TreeKind.Expr)
  *
  * Example:
  * {{{
  *   object ExpressionRules extends FormatterModule {
  *     val rules: Map[Class[?], SyntaxTree.Tree => Doc] = Map(
  *       TreeKind.Expr.IfThenElse.getClass -> formatIfThenElse,
  *       TreeKind.Expr.Match.getClass      -> formatMatch,
  *       classOf[TreeKind.Expr]            -> formatAnyExpr,  // catch-all
  *     )
  *
  *     private def formatIfThenElse(tree: SyntaxTree.Tree): Doc = ...
  *     private def formatMatch(tree: SyntaxTree.Tree): Doc      = ...
  *     private def formatAnyExpr(tree: SyntaxTree.Tree): Doc    = reconstruct(tree)
  *   }
  * }}}
  */
trait FormatterModule {

  val rules: PartialFunction[SyntaxTree.Tree, Doc]

  protected def treeToDoc(tree: SyntaxTree.Tree): Doc =
    PrettyPrinter.treeToDoc(tree)

  protected def reconstruct(tree: SyntaxTree.Tree): Doc =
    PrettyPrinter.reconstruct(tree)

  protected def collectTokens(tree: SyntaxTree.Tree): Seq[Token] =
    PrettyPrinter.collectTokens(tree)

}
