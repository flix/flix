package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.UniquenessError
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.language.ast.Symbol


object Uniqueness extends Phase[Root, Root]{

  def run(root: Root)(implicit flix: Flix): Validation[Root, UniquenessError] = {
    for ((sym, defn) <- root.defs ) {
      visitExp(defn.exp, Set.empty)
    }
    root.toSuccess
  }

  def visitExp(exp0 : TypedAst.Expression, dead: Set[Symbol.VarSym]) : Unit =
    exp0 match {
      case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) => {
        exp1 match {
          case TypedAst.Expression.Var(sym2, _, _, _) => visitExp(exp2, dead + sym2)
          case _ => visitExp(exp2, dead)
        }
      }
      case TypedAst.Expression.Unique(exp, tpe, eff, loc) => {

        visitExp(exp, dead)
      }
      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => {

      }
      case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => {

        visitExp(base, dead)
      }
      case TypedAst.Expression.Var(sym, tpe, eff, loc) => {
        if (dead.contains(sym))
          throw InternalCompilerException("The symbol is dead.")
      }
    }
}
