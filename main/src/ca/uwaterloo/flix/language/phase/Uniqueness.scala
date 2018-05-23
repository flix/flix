package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.language.errors.UniquenessError
import ca.uwaterloo.flix.language.ast.Symbol


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
    visitExp(defn0.exp, Set.empty, Set.empty).map{
      case e => defn0
    }
  }

  def visitExp(exp0: TypedAst.Expression, dead: Set[Symbol.VarSym], uniqueSet : Set[Symbol.VarSym]): Validation[Set[Symbol.VarSym], UniquenessError] = {
    exp0 match {
      case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) => {
        exp1 match {
          case TypedAst.Expression.Var(sym2, tpe, eff, loc) => {
            if (dead.contains(sym2)){
              UniquenessError.DeadSymbol(loc).toFailure
            }
            else
              visitExp(exp2, dead + sym2, uniqueSet)
          }

          case TypedAst.Expression.Unique(exp, tpe, eff, loc) => {
            exp match {
              case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => {
                visitExps(elms, dead, uniqueSet + sym)
              }
              case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => {
                if (!isPrimitive(tpe, false)){
                  val expList = elms :+ exp2
                  visitExps(expList, dead, uniqueSet + sym)
                }
                else{
                  visitExp(exp2, dead, uniqueSet)
                }
              } 
              case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => {
                val expList = List(elm, len, exp2)
                visitExps(expList, dead, uniqueSet + sym)
              }
              case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => {
                val expList = List(base, index, exp2)
                visitExps(expList, dead, uniqueSet + sym)
              }
              case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
                val expList = List(base, beginIndex, endIndex, exp2)
                visitExps(expList, dead, uniqueSet + sym)
              }
              case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => {
                val expList = elms :+ exp2
                visitExps(expList, dead, uniqueSet + sym + sym)

              }
              case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => {
                val expList = List(elm, exp2)
                visitExps(expList, dead, uniqueSet + sym)
              }
              case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => {
                val expList = List(base, exp2)
                visitExps(expList, dead, uniqueSet + sym)
              }
              case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => {
                val expList = List(base, exp2)
                visitExps(expList, dead, uniqueSet + sym)
              }
              case _ => visitExp(exp2, dead, uniqueSet)
            }
          }

          case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => {
            if (!isPrimitive(tpe, false)){
              val expList = elms :+ exp2
              visitExps(expList, dead, uniqueSet + sym)
            }
            else{
              visitExp(exp2, dead, uniqueSet)
            }
          }

          case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => {
            val expList = List(elm, len)
            visitExps(expList, dead, uniqueSet)
          }

          case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => {
            val expList = List(base, index)
            visitExps(expList, dead, uniqueSet)
          }

          case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
            val expList = List(base, beginIndex, endIndex, exp2)
            visitExps(expList, dead, uniqueSet)
          }

          case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => {
            if (!isPrimitive(tpe, false)){
              val expList = elms :+ exp2
              visitExps(expList, dead, uniqueSet + sym)
            }
            else{
              visitExp(exp2, dead, uniqueSet)
            }
          }

          case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => {
            visitExp(elm, dead, uniqueSet)
          }

          case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => {
            visitExp(base, dead + expressionToSymbol(base), uniqueSet)
          }

          case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => {
            visitExp(base, dead, uniqueSet)
          }

          case _ => visitExp(exp2, dead, uniqueSet)
        }
      }

      case TypedAst.Expression.Unique(exp, tpe, eff, loc) => {
        visitExp(exp, dead, uniqueSet + expressionToSymbol(exp))
      }

      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => {
        val expList = List(base, index)
        visitExps(expList, dead, uniqueSet)
      }

      case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => {
        visitExp(base, dead, uniqueSet)
      }

      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => {
        visitExp(base, dead, uniqueSet)
        visitExp(beginIndex, dead, uniqueSet)
        visitExp(endIndex, dead, uniqueSet)
      }

      case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => {
        visitExp(base, dead, uniqueSet)
      }

      case TypedAst.Expression.VectorSlice(base, beginIndex, endIndex, tpe, eff, loc) => {
        visitExp(base, dead, uniqueSet)
      }

      case TypedAst.Expression.Var(sym, tpe, eff, loc) => {
        if (dead.contains(sym))
          UniquenessError.DeadSymbol(loc).toFailure
        else if (uniqueSet.contains(sym))
          (dead + sym).toSuccess
        else
          dead.toSuccess
      }

      case _ => dead.toSuccess
    }
  }

  def visitExps(es: List[TypedAst.Expression], dead0: Set[Symbol.VarSym], uniqueSet: Set[Symbol.VarSym]): Validation[Set[Symbol.VarSym], UniquenessError] = {
    es match {
      case Nil => dead0.toSuccess
      case x :: xs => visitExp(x, dead0, uniqueSet).flatMap {
        case deadSet => visitExps(xs, deadSet, uniqueSet)
      }
    }
  }

  def expressionToSymbol(exp: TypedAst.Expression): Symbol.VarSym = {
    exp match {
      case TypedAst.Expression.Var(sym, tpe, eff, loc) => sym
      case _ => throw InternalCompilerException("Unexpected type.")
    }
  }

  def isPrimitive(tpe: Type, isChecked: Boolean): Boolean = {
    if (isChecked)
      return false
    tpe match {
      case Type.Int8 => true
      case Type.Int16 => true
      case Type.Int32 => true
      case Type.Int64 => true
      case Type.Unit => true
      case Type.Float32 => true
      case Type.Float64 => true
      case Type.Bool => true
      case Type.Char => true
      case Type.Apply(Type.Array, t) => isPrimitive(t, true)
      case Type.Apply(Type.Apply(Type.Vector, t), _) => isPrimitive(t, true)
      case _ => false
    }
  }
}

