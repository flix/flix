package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.language.errors.UniquenessError
import ca.uwaterloo.flix.language.errors.UniquenessError._
import ca.uwaterloo.flix.runtime.shell.Command.TypeOf
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.evaluator.SymVal


object Uniqueness extends Phase[Root, Root]{

  def run(root: Root)(implicit flix: Flix): Validation[Root, UniquenessError] = {
    val defsVal = root.defs.map{
      case (sym, defn) => visitDef(defn)
    }

    seqM(defsVal).map{
      case defns => root
    }
  }

  def visitDef(defn0: TypedAst.Def): Validation[TypedAst.Def, UniquenessError] = {
    visitExp(defn0.exp, Set.empty).map{
      case e => defn0
    }
  }

  /**
    * The type of environments.
    *
    * An environment is a map from variable symbols to symbolic values.
    */
  type Environment = Map[Symbol.VarSym, SymVal]


  def visitExp(exp0: TypedAst.Expression, dead: Set[Symbol.VarSym]): Validation[Set[Symbol.VarSym], UniquenessError] =
    exp0 match {
      case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) => {
        exp1 match {
          case TypedAst.Expression.Var(sym2, tpe, eff, loc) =>{
            if (dead.contains(sym2))
              UniquenessError.DeadSymbol(loc, sym.loc).toFailure

            visitExp(exp2, dead + sym2)
          }

          case TypedAst.Expression.Unique(exp, tpe, eff, loc) => {
            exp match {
              case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => {
                visitExps(elms, dead)
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => {
                visitExps(elms, dead)
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => {
                visitExp(elm, dead)
                visitExp(len, dead)
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => {
                visitExp(base, dead)
                visitExp(index, dead)
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
                visitExp(base, dead)
                visitExp(beginIndex, dead)
                visitExp(endIndex, dead)
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => {
                elms.map(e => visitExp(e, dead))
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => {
                visitExp(elm, dead)
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => {
                visitExp(base, dead)
                visitExp(exp2, dead)
              }
              case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => {
                visitExp(base, dead)
                visitExp(exp2, dead)
              }
              case _ => UniquenessError.UniquePrimitiveType(loc).toFailure
            }
          }

          case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) =>{
            visitExps(elms, dead)
          }

          case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => {
            visitExp(elm, dead)
            visitExp(len, dead)
          }

          case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => {
            visitExp(base, dead + expressionToSymbol(base))
          }

          case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
            visitExp(base, dead)
            visitExp(beginIndex, dead)
            visitExp(endIndex, dead)
            visitExp(exp2, dead + expressionToSymbol(base))
          }

          case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => {
            visitExps(elms, dead)
          }

          case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => {
            visitExp(elm, dead)
          }

          case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => {
            visitExp(base, dead + expressionToSymbol(base))
          }

          case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => {
            visitExp(base, dead)
            visitExp(exp2, dead + expressionToSymbol(base))
          }

          case _ => visitExp(exp2, dead)
        }
      }

      case TypedAst.Expression.Unique(exp, tpe, eff, loc) => {
        visitExp(exp, dead)
      }

      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => {
        visitExps(elms, dead)
      }

      case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => {
        visitExp(elm, dead)
        visitExp(len, dead)
      }

      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => {
        visitExp(base, dead)
      }

      case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => {
        visitExp(base, dead)
      }

      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
        visitExp(base, dead)
        visitExp(beginIndex, dead)
        visitExp(endIndex, dead)
      }

      case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => {
        visitExps(elms, dead)
      }

      case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => {
        visitExp(elm, dead)
      }

      case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => {
        visitExp(base, dead)
      }

      case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => {
        visitExp(base, dead)
      }

      case TypedAst.Expression.Var(sym, tpe, eff, loc) => {
        if (dead.contains(sym))
          UniquenessError.DeadSymbol(loc, sym.loc).toFailure
        else
          dead.toSuccess
      }

      case _ => dead.toSuccess
    }

  def visitExps(es: List[TypedAst.Expression], dead0: Set[Symbol.VarSym]): Validation[Set[Symbol.VarSym], UniquenessError] = {
    es match {
      case Nil => dead0.toSuccess
      case x :: xs => visitExp(x, dead0 + expressionToSymbol(x)).flatMap {
        case deadSet => visitExps(xs, deadSet)
      }
    }
  }

  //TODO Hvis der ikke er tale om en VarSym, returner da en "tom" tingenot
  def expressionToSymbol(exp: TypedAst.Expression): Symbol.VarSym = {
    //val symVal = Symbol.freshVarSym()
    exp match {
      case TypedAst.Expression.Var(sym, tpe, eff, loc) => sym
      //case _ => throw InternalCompilerException("Unexpected type.")
      //case _ =>
    }
  }
}

