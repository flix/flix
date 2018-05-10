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
          case TypedAst.Expression.Unique(exp, tpe, eff, loc) => {
            visitExp(exp, dead)
            visitExp(exp2, dead)
          }
          case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => elms.map(e => visitExp(e, dead))
          case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => {
            visitExp(elm, dead)
            visitExp(len, dead)
          }
          case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => visitExp(base, dead + expressionToSymbol(base))
          case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
            visitExp(beginIndex, dead)
            visitExp(endIndex, dead)
            visitExp(base, dead/* + expressionToSymbol(base)*/)
          }
          case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => elms.map(e => visitExp(e, dead))
          case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => visitExp(elm, dead)
          case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => visitExp(base, dead + expressionToSymbol(base))
          case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => visitExp(base, dead + expressionToSymbol(base))
          case _ => visitExp(exp2, dead)
        }
      }

      case TypedAst.Expression.Unique(exp, tpe, eff, loc) => {
        visitExp(exp, dead)
      }

      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => {
        elms.map(e => visitExp(e, dead))
      }

      case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => {
        visitExp(elm, dead)
        visitExp(len, dead)
      }

      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => visitExp(base, dead)

      case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => {
        visitExp(base, dead)
      }

      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
        visitExp(base, dead)
        visitExp(beginIndex, dead)
        visitExp(endIndex, dead)

      }

      case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => {
        elms.map(e => visitExp(e, dead))
      }

      case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => visitExp(elm, dead)

      case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => visitExp(base, dead)

      case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => {
        visitExp(base, dead)
      }

      case TypedAst.Expression.Var(sym, tpe, eff, loc) => {
        if (dead.contains(sym))
          throw InternalCompilerException("The symbol is dead.")
      }

      case _ => ()
    }

  def expressionToSymbol(exp: TypedAst.Expression) : Symbol.VarSym = {
    exp match {
      case TypedAst.Expression.Var(sym, tpe, eff, loc) => sym
    }
  }
}
