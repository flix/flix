package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.LoweredAst.*
import ca.uwaterloo.flix.language.ast.LoweredAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.{LoweredAst, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object LoweredAstOps {

  /**
    * Returns the free variables in the given expression `exp0`.
    */
  def freeVars(exp0: Expr): Map[Symbol.VarSym, Type] = exp0 match {
    case Expr.Cst(_, _, _) => Map.empty

    case Expr.Var(sym, tpe, _) => Map(sym -> tpe)

    case Expr.Lambda(fparam, exp, _, _) =>
      freeVars(exp) - fparam.sym

    case LoweredAst.Expr.ApplyAtomic(_, exps, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.ApplyDef(_, exps, _, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.ApplyLocalDef(_, exps, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.ApplyOp(_, exps, _, _, _) =>
      exps.flatMap(freeVars).toMap

    case Expr.ApplySig(_, exps, _, _, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      (freeVars(exp1) ++ freeVars(exp2)) - sym

    case Expr.LocalDef(sym, fparams, exp1, exp2, _, _, _) =>
      val bound = sym :: fparams.map(_.sym)
      (freeVars(exp1) -- bound) ++ (freeVars(exp2) - sym)

    case Expr.Region(sym, _, exp, _, _, _) =>
      freeVars(exp) - sym

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.Discard(exp, _, _) =>
      freeVars(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, MatchRule(pat, guard, body)) =>
          acc ++ ((guard.map(freeVars).getOrElse(Map.empty) ++ freeVars(body)) -- freeVars(pat).keys)
      }

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, TypeMatchRule(sym, _, body)) => acc ++ (freeVars(body) - sym)
      }

    case Expr.ExtMatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, ExtMatchRule(pat, exp1, _)) =>
          acc ++ freeVars(exp1) -- freeVars(pat)
      }

    case Expr.VectorLit(elms, _, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, e) => acc ++ freeVars(e)
      }

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.VectorLength(exp, _) =>
      freeVars(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      freeVars(exp)

    case Expr.Cast(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, CatchRule(sym, _, body)) => acc ++ freeVars(body) - sym
      }

    case Expr.OldRunWith(exp1, _, _, _, _, _) =>
      freeVars(exp1)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _)) => acc ++ freeVars(exp) -- fparams.map(_.sym)
      }

    case Expr.NewChannel(exp, _, _, _) =>
      freeVars(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      freeVars(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      val d = default.map(freeVars).getOrElse(Map.empty)
      rules.foldLeft(d) {
        case (acc, SelectChannelRule(sym, chan, exp, _)) => acc ++ ((freeVars(chan) ++ freeVars(exp)) - sym)
      }

    case Expr.ParYield(frags, exp, _, _, _) =>
      val freeFragVars = frags.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, ParYieldFragment(p, e, _)) => acc ++ freeVars(p) ++ freeVars(e)
      }
      freeVars(exp) -- freeFragVars.keys

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, c) => acc ++ freeVars(c)
      }

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.FixpointQueryWithProvenance(exps, select, _, _, _, _) =>
      freeVars(exps) ++ freeVars(select)

    case Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, _, _, _, _) =>
      freeVars(exps) ++ freeVars(queryExp) ++ freeVars(selects) ++ from.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        (acc, b) => acc ++ freeVars(b)
      } ++ freeVars(where)

    case Expr.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        (acc, exp) => acc ++ freeVars(exp)
      }

    case Expr.FixpointInjectInto(exps, _, _, _, _) =>
      freeVars(exps)

    case _ =>
      throw InternalCompilerException(s"Will not be implemented", exp0.loc)

  }

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
    case Pattern.Wild(_, _) => Map.empty
    case Pattern.Var(sym, tpe, _) => Map(sym -> tpe)
    case Pattern.Cst(_, _, _) => Map.empty
    case Pattern.Tag(_, pats, _, _) =>
      pats.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, pat) => acc ++ freeVars(pat)
      }
    case Pattern.Tuple(elms, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, pat) => acc ++ freeVars(pat)
      }
    case Pattern.Record(pats, pat, _, _) =>
      val patsVal = pats.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, rfp) => acc ++ freeVars(rfp.pat)
      }
      val patVal = freeVars(pat)
      patsVal ++ patVal
  }

  /**
    * Returns the free variables in the given extensible pattern `pat0`.
    */
  private def freeVars(pat0: ExtPattern): Set[Symbol.VarSym] = pat0 match {
    case ExtPattern.Default(_) => Set.empty
    case ExtPattern.Tag(_, pats, _) => pats.toSet.flatMap((v: ExtTagPattern) => freeVars(v))
  }

  /**
    * Returns the free variables in the given ext tag pattern `v`.
    */
  private def freeVars(v: ExtTagPattern): Set[Symbol.VarSym] = v match {
    case ExtTagPattern.Wild(_, _) => Set.empty
    case ExtTagPattern.Var(sym, _, _) => Set(sym)
    case ExtTagPattern.Unit(_, _) => Set.empty
  }

  /**
    * Returns the free variables in the given constraint `constraint0`.
    */
  private def freeVars(constraint0: Constraint): Map[Symbol.VarSym, Type] = constraint0 match {
    case Constraint(cparams0, head, body, _) =>
      (freeVars(head) ++ body.flatMap(freeVars)) -- cparams0.map(_.sym)
  }

  /**
    * Returns the free variables in the given head predicate `head0`.
    */
  private def freeVars(head0: Predicate.Head): Map[Symbol.VarSym, Type] = head0 match {
    case Head.Atom(_, _, terms, _, _) =>
      terms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, term) => acc ++ freeVars(term)
      }
  }

  /**
    * Returns the free variables in the given body predicate `body0`.
    */
  private def freeVars(body0: Predicate.Body): Map[Symbol.VarSym, Type] = body0 match {
    case Body.Atom(_, _, _, _, terms, _, _) =>
      terms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, term) => acc ++ freeVars(term)
      }
    case Body.Guard(exp, _) => freeVars(exp)
    case Body.Functional(_, exp, _) => freeVars(exp)
  }

  /**
    * Returns the free variables in the given list of expressions `exp0`.
    */
  private def freeVars(exps0: List[Expr]): Map[Symbol.VarSym, Type] =
    exps0.foldLeft(Map.empty[Symbol.VarSym, Type]) {
      case (acc, exp) => acc ++ freeVars(exp)
    }

}
