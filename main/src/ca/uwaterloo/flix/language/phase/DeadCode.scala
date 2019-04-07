package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.DeadCodeError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym

object DeadCode extends Phase[Root, Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, DeadCodeError] = flix.phase("DeadCode") {

    val newDefs = traverse(root.defs)  {
      case (sym, defn) => visitDef(defn).map(x => sym -> x)
    }


    mapN(newDefs) {
      case defs => root.copy(defs = defs.toMap)
    }
  }

  private def visitDef(defn: TypedAst.Def): Validation[TypedAst.Def, DeadCodeError] =
    mapN(visitExp(defn.exp)._1) {
      case _ => defn
    }

  private def visitExp(e: TypedAst.Expression): (Validation[TypedAst.Expression, DeadCodeError], Set[Symbol.EnumSym]) =
    e match {
      case TypedAst.Expression.Lambda(fparam, exp, tpe, eff, loc) => (
        if (freeVar(e).contains(fparam.sym)) {
          e.toSuccess
        } else {
          DeadCodeError(fparam.sym.loc, "Lambda parameter never used.").toFailure
        }, Set.empty) // TODO: Finish this
      case TypedAst.Expression.Apply(exp1, exp2, tpe, eff, loc) =>
        val applyVal1 = visitExp(exp1)
        val applyVal2 = visitExp(exp2)
        (mapN(applyVal1._1, applyVal2._1) {
          case (applyVal1, applyVal2) => e
        }, applyVal1._2 ++ applyVal2._2)
      case TypedAst.Expression.Unary(op, exp, tpe, eff, loc) =>
        val unaryVal = visitExp(exp)
        (mapN(unaryVal._1) {
          case (unaryVal) => e
        }, unaryVal._2)
      case TypedAst.Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
        val binaryVal1 = visitExp(exp1)
        val binaryVal2 = visitExp(exp2)
        (mapN(binaryVal1._1, binaryVal2._1) {
          case (binaryVal1, binaryVal2) => e
        }, binaryVal1._2 ++ binaryVal2._2)
      case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
        val letVal = visitExp(exp1)
        val expVal = visitExp(exp2)
        val freeVarVal = // TODO: Figure out naming now that we return tuples
          if (freeVar(exp2).contains(sym)) e.toSuccess else DeadCodeError(sym.loc, "Variable never used.").toFailure // TODO: Handle let recursion
        (mapN(letVal._1, expVal._1, freeVarVal) {
          case (letVal, expVal, freeVarVal) => e
        }, letVal._2 ++ expVal._2)
      case TypedAst.Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) =>
        val letRecVal1 = visitExp(exp1)
        val letRecVal2 = visitExp(exp2)
        (mapN(letRecVal1._1, letRecVal2._1) {
          case (letRecVal1, letRecVal2) => e
        }, letRecVal1._2 ++ letRecVal2._2)
      case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val condVal = visitExp(exp1)
        val thenVal = visitExp(exp2)
        val elseVal = visitExp(exp3)
        (mapN(condVal._1, thenVal._1, elseVal._1) {
          case (condExp, thenExp, elseExp) => e
        }, condVal._2 ++ thenVal._2 ++ elseVal._2)
      case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) =>
        val matchVal = visitExp(exp)
        val rulesValList = for (r <- rules; guardVal = visitExp(r.guard); expVal = visitExp(r.exp)) yield {
          (mapN(guardVal._1, expVal._1) {
            case (guardVal, expVal) => e
          }, guardVal._2 ++ expVal._2)
        }
        val rulesValTuple = rulesValList.unzip
        // TODO: Combine results
        val unusedSyms = (for (r <- rules; patVar = patternVar(r.pat); expVar = freeVar(r.exp)) yield {
          if (patVar.subsetOf(expVar)) {
            List.empty
          } else {
            patVar.diff(expVar)
          }
        }).flatten
        val patVarVal = if (unusedSyms.length == 0) e.toSuccess else DeadCodeError(unusedSyms.head.loc, "Matching variable(s) not used in expression.").toFailure
        (mapN(matchVal._1, patVarVal) {
          case (matchVal, patVarVal) => e
        }, matchVal._2)
      case TypedAst.Expression.Switch(rules, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        val expVal = visitExp(exp)
        (expVal._1, Set(sym) ++ expVal._2) // TODO: Correct? What is exp?
      case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.RecordEmpty(tpe, eff, loc) => (e.toSuccess, Set.empty)
      case TypedAst.Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
        val valueVal = visitExp(value)
        val restVal = visitExp(rest)
        (mapN(valueVal._1, restVal._1) {
          case (matchVal, patVarVal) => e
        }, valueVal._2 ++ restVal._2)
      case TypedAst.Expression.RecordRestrict(label, rest, tpe, eff, loc) => visitExp(rest)
      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        val elmVal = visitExp(elm)
        val lenVal = visitExp(len)
        (mapN(elmVal._1, lenVal._1) {
          case (matchVal, patVarVal) => e
        }, elmVal._2 ++ lenVal._2)
      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        val baseVal = visitExp(base)
        val indexVal = visitExp(index)
        (mapN(baseVal._1, indexVal._1) {
          case (matchVal, indexVal) => e
        }, baseVal._2 ++ indexVal._2)
      case TypedAst.Expression.ArrayLength(base, tpe, eff, loc) => visitExp(base)
      case TypedAst.Expression.ArrayStore(base, index, elm, tpe, eff, loc) =>
        val baseVal = visitExp(base)
        val indexVal = visitExp(index)
        val elmVal = visitExp(elm)
        (mapN(baseVal._1, indexVal._1, elmVal._1) {
          case (baseVal, indexVal, elmVal) => e
        }, baseVal._2 ++ indexVal._2 ++ elmVal._2)
      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) =>
        val baseVal = visitExp(base)
        val beginVal = visitExp(beginIndex)
        val endVal = visitExp(endIndex)
        (mapN(baseVal._1, beginVal._1, endVal._1) {
          case (baseVal, beginVal, endVal) => e
        }, baseVal._2 ++ beginVal._2 ++ endVal._2)
      case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => visitExp(elm)
      case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => visitExp(base)
      case TypedAst.Expression.VectorStore(base, index, elm, tpe, eff, loc) =>
        val baseVal = visitExp(base)
        val elmVal = visitExp(elm)
        (mapN(baseVal._1, elmVal._1) {
          case (baseVal, elmVal) => e
        }, baseVal._2 ++ elmVal._2)
      case TypedAst.Expression.VectorLength(base, tpe, eff, loc) => visitExp(base)
      case TypedAst.Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) =>
        val baseVal = visitExp(base)
        val endVal = visitExp(endIndex)
        (mapN(baseVal._1, endVal._1) {
          case (baseVal, endVal) => e
        }, baseVal._2 ++ endVal._2)
      case TypedAst.Expression.Ref(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Deref(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        val varVal = visitExp(exp1) // Is this and the below name appropriate?
        val valueVal = visitExp(exp2)
        (mapN(varVal._1, valueVal._1) {
          case (varVal, valueVal) => e
        }, varVal._2 ++ valueVal._2)
      case TypedAst.Expression.HandleWith(exp, bindings, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.Existential(fparam, exp, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.Universal(fparam, exp, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Cast(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.NativeConstructor(constructor, args, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        val exp1Val = visitExp(exp1) // TODO: Find better variable names
        val exp2Val = visitExp(exp2)
        (mapN(exp1Val._1, exp2Val._1) {
          case (exp1Val, exp2Val) => e
        }, exp1Val._2 ++ exp2Val._2)
      case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.Spawn(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.Sleep(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.FixpointConstraint(c, tpe, eff, loc) => (e.toSuccess, Set.empty) // TODO
      case TypedAst.Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
        val exp1Val = visitExp(exp1) // TODO: Find better variable names
        val exp2Val = visitExp(exp2)
        (mapN(exp1Val._1, exp2Val._1) {
          case (exp1Val, exp2Val) => e
        }, exp1Val._2 ++ exp2Val._2)
      case TypedAst.Expression.FixpointSolve(exp, tpe, eff, loc) => visitExp(exp)
      case TypedAst.Expression.FixpointProject(pred, exp, tpe, eff, loc) =>
        val predVal = visitExp(pred.exp)
        val expVal = visitExp(exp)
        (mapN(predVal._1, expVal._1) {
          case (predVal, expVal) => e
        }, predVal._2 ++ expVal._2)
      case TypedAst.Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
        val exp1Val = visitExp(exp1) // TODO: Find better variable names
        val exp2Val = visitExp(exp2)
        (mapN(exp1Val._1, exp2Val._1) {
          case (exp1Val, exp2Val) => e
        }, exp1Val._2 ++ exp2Val._2)
      case _ => (e.toSuccess, Set.empty)
    }

  private def freeVar(exp: TypedAst.Expression): Set[Symbol.VarSym] =
    exp match {
      case TypedAst.Expression.Var(sym, tpe, eff, loc) => Set(sym)
      case TypedAst.Expression.Lambda(fparam, exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Apply(exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case TypedAst.Expression.Unary(op, exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Binary(op, exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case TypedAst.Expression.Let(sym, exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case TypedAst.Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2) ++ freeVar(exp3)
      case TypedAst.Expression.Match(exp, rules, tpe, eff, loc) => freeVar(exp) ++ (for (r <- rules) yield freeVar(r.exp)).flatten.toSet // TODO: Is this correct?
      case TypedAst.Expression.Switch(rules, tpe, eff, loc) => (for (r <- rules) yield freeVar(r._1) ++ freeVar(r._2)).flatten.toSet
      case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => (for (e <- elms) yield freeVar(e)).flatten.toSet
      case TypedAst.Expression.RecordSelect(exp, label, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.RecordExtend(label, value, rest, tpe, eff, loc) => freeVar(value) ++ freeVar(rest)
      case TypedAst.Expression.RecordRestrict(label, rest, tpe, eff, loc) => freeVar(rest)
      case TypedAst.Expression.ArrayLit(elms, tpe, eff, loc) => (for (e <- elms) yield freeVar(e)).flatten.toSet
      case TypedAst.Expression.ArrayNew(elm, len, tpe, eff, loc) => freeVar(elm) ++ freeVar(len)
      case TypedAst.Expression.ArrayLoad(base, index, tpe, eff, loc) => freeVar(base) ++ freeVar(index)
      case TypedAst.Expression.ArrayStore(base, index, elm, tpe, eff, loc) => freeVar(base) ++ freeVar(index) ++ freeVar(elm)
      case TypedAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => freeVar(base) ++ freeVar(beginIndex) ++ freeVar(endIndex)
      case TypedAst.Expression.VectorLit(elms, tpe, eff, loc) => (for (e <- elms) yield freeVar(e)).flatten.toSet
      case TypedAst.Expression.VectorNew(elm, len, tpe, eff, loc) => freeVar(elm)
      case TypedAst.Expression.VectorLoad(base, index, tpe, eff, loc) => freeVar(base)
      case TypedAst.Expression.VectorStore(base, index, elm, tpe, eff, loc) => freeVar(base) ++ freeVar(elm)
      case TypedAst.Expression.VectorLength(base, tpe, eff, loc) => freeVar(base)
      case TypedAst.Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => freeVar(base) ++ freeVar(endIndex)
      case TypedAst.Expression.Ref(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Deref(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case TypedAst.Expression.HandleWith(exp, bindings, tpe, eff, loc) => freeVar(exp) ++ (for (b <- bindings) yield freeVar(b.exp)).flatten.toSet
      case TypedAst.Expression.Existential(fparam, exp, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Universal(fparam, exp, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Cast(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.NativeConstructor(constructor, args, tpe, eff, loc) => (for (a <- args) yield freeVar(a)).flatten.toSet
      case TypedAst.Expression.TryCatch(exp, rules, tpe, eff, loc) => freeVar(exp) ++ (for (r <- rules) yield freeVar(r.exp)).flatten.toSet
      case TypedAst.Expression.NativeMethod(method, args, tpe, eff, loc) => (for (a <- args) yield freeVar(a)).flatten.toSet
      case TypedAst.Expression.NewChannel(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.GetChannel(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.PutChannel(exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case TypedAst.Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        (for (r <- rules) yield freeVar(r.chan) ++ freeVar(r.exp)).flatten.toSet ++ (default match {
          case None => Set.empty
          case Some(e) => freeVar(e)
        })
      case TypedAst.Expression.Spawn(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.Sleep(exp, tpe, eff, loc) => freeVar(exp)
        // TODO: Add FixPointConstraint
      case TypedAst.Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case TypedAst.Expression.FixpointSolve(exp, tpe, eff, loc) => freeVar(exp)
      case TypedAst.Expression.FixpointProject(pred, exp, tpe, eff, loc) => freeVar(pred.exp) ++ freeVar(exp)
      case TypedAst.Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => freeVar(exp1) ++ freeVar(exp2)
      case _ => Set.empty
    }

  private def patternVar(pat: TypedAst.Pattern): Set[Symbol.VarSym] =
    pat match {
      case TypedAst.Pattern.Var(sym, tpe, loc) => Set(sym)
      case TypedAst.Pattern.Tag(sym, tag, pat, tpe, loc) => patternVar(pat) // Correct?
      case TypedAst.Pattern.Tuple(elms, tpe, loc) => (for (e <- elms) yield patternVar(e)).flatten.toSet
      case _ => Set.empty
    }
}
