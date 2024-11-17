package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.SigSymUse
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type, TypedAst}

object TypedAstOps {

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  def freeVarsOf(pat0: Pattern): Set[Symbol.VarSym] = binds(pat0).keySet

  /**
    * Returns the set of variable symbols bound by the given pattern `pat0`.
    */
  def binds(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
    case Pattern.Wild(_, _) => Map.empty
    case Pattern.Var(Binder(sym, _), tpe, _) => Map(sym -> tpe)
    case Pattern.Cst(_, _, _) => Map.empty
    case Pattern.Tag(_, pats, _, _) => pats.foldLeft(Map.empty[Symbol.VarSym, Type]) {
      case (macc, pat) => macc ++ binds(pat)
    }
    case Pattern.Tuple(elms, _, _) => elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
      case (macc, elm) => macc ++ binds(elm)
    }
    case Pattern.Record(pats, pat, _, _) =>
      val patsVal = pats.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (macc, rfp) => macc ++ binds(rfp.pat)
      }
      val patVal = binds(pat)
      patsVal ++ patVal
    case Pattern.RecordEmpty(_, _) => Map.empty
    case Pattern.Error(_, _) => Map.empty
  }

  /**
    * Creates a set of all the sigs used in the given `exp`.
    */
  def sigSymsOf(exp0: Expr): Set[Symbol.SigSym] = exp0 match {
    case Expr.Cst(_, _, _) => Set.empty
    case Expr.Var(_, _, _) => Set.empty
    case Expr.Hole(_, _, _, _) => Set.empty
    case Expr.HoleWithExp(exp, _, _, _) => sigSymsOf(exp)
    case Expr.OpenAs(_, exp, _, _) => sigSymsOf(exp)
    case Expr.Use(_, _, exp, _) => sigSymsOf(exp)
    case Expr.Lambda(_, exp, _, _) => sigSymsOf(exp)
    case Expr.ApplyClo(exp, exps, _, _, _) => sigSymsOf(exp) ++ exps.flatMap(sigSymsOf)
    case Expr.ApplyDef(_, exps, _, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expr.ApplyLocalDef(_, exps, _, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expr.ApplySig(SigSymUse(sym, _), exps, _, _, _, _) => exps.flatMap(sigSymsOf).toSet + sym
    case Expr.Unary(_, exp, _, _, _) => sigSymsOf(exp)
    case Expr.Binary(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.Let(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.Region(_, _) => Set.empty
    case Expr.Scope(_, _, exp, _, _, _) => sigSymsOf(exp)
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Expr.Stm(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.Discard(exp, _, _) => sigSymsOf(exp)
    case Expr.Match(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp) ++ rule.guard.toList.flatMap(sigSymsOf))
    case Expr.TypeMatch(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expr.RestrictableChoose(_, exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expr.ApplyTag(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expr.ApplyRestrictableTag(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expr.Tuple(elms, _, _, _) => elms.flatMap(sigSymsOf).toSet
    case Expr.RecordEmpty(_, _) => Set.empty
    case Expr.RecordSelect(exp, _, _, _, _) => sigSymsOf(exp)
    case Expr.RecordExtend(_, value, rest, _, _, _) => sigSymsOf(value) ++ sigSymsOf(rest)
    case Expr.RecordRestrict(_, rest, _, _, _) => sigSymsOf(rest)
    case Expr.ArrayLit(exps, exp, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ sigSymsOf(exp)
    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Expr.ArrayLoad(base, index, _, _, _) => sigSymsOf(base) ++ sigSymsOf(index)
    case Expr.ArrayLength(base, _, _) => sigSymsOf(base)
    case Expr.ArrayStore(base, index, elm, _, _) => sigSymsOf(base) ++ sigSymsOf(index) ++ sigSymsOf(elm)
    case Expr.StructNew(_, fields, region, _, _, _) => sigSymsOf(region) ++ fields.flatMap { case (_, v) => sigSymsOf(v) }
    case Expr.StructGet(e, _, _, _, _) => sigSymsOf(e)
    case Expr.StructPut(e1, _, e2, _, _, _) => sigSymsOf(e1) ++ sigSymsOf(e2)
    case Expr.VectorLit(exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expr.VectorLoad(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.VectorLength(exp, _) => sigSymsOf(exp)
    case Expr.Ascribe(exp, _, _, _) => sigSymsOf(exp)
    case Expr.InstanceOf(exp, _, _) => sigSymsOf(exp)
    case Expr.CheckedCast(_, exp, _, _, _) => sigSymsOf(exp)
    case Expr.UncheckedCast(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expr.UncheckedMaskingCast(exp, _, _, _) => sigSymsOf(exp)
    case Expr.Without(exp, _, _, _, _) => sigSymsOf(exp)
    case Expr.TryCatch(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expr.Throw(exp, _, _, _) => sigSymsOf(exp)
    case Expr.TryWith(exp, _, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expr.Do(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expr.InvokeConstructor(_, args, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Expr.InvokeMethod(_, exp, args, _, _, _) => sigSymsOf(exp) ++ args.flatMap(sigSymsOf)
    case Expr.InvokeStaticMethod(_, args, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Expr.GetField(_, exp, _, _, _) => sigSymsOf(exp)
    case Expr.PutField(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.GetStaticField(_, _, _, _) => Set.empty
    case Expr.PutStaticField(_, exp, _, _, _) => sigSymsOf(exp)
    case Expr.NewObject(_, _, _, _, methods, _) => methods.flatMap(method => sigSymsOf(method.exp)).toSet
    case Expr.NewChannel(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.GetChannel(exp, _, _, _) => sigSymsOf(exp)
    case Expr.PutChannel(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.SelectChannel(rules, default, _, _, _) => rules.flatMap(rule => sigSymsOf(rule.chan) ++ sigSymsOf(rule.exp)).toSet ++ default.toSet.flatMap(sigSymsOf)
    case Expr.Spawn(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.ParYield(frags, exp, _, _, _) => sigSymsOf(exp) ++ frags.flatMap(f => sigSymsOf(f.exp))
    case Expr.Lazy(exp, _, _) => sigSymsOf(exp)
    case Expr.Force(exp, _, _, _) => sigSymsOf(exp)
    case Expr.FixpointConstraintSet(_, _, _) => Set.empty
    case Expr.FixpointLambda(_, exp, _, _, _) => sigSymsOf(exp)
    case Expr.FixpointMerge(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expr.FixpointSolve(exp, _, _, _) => sigSymsOf(exp)
    case Expr.FixpointFilter(_, exp, _, _, _) => sigSymsOf(exp)
    case Expr.FixpointInject(exp, _, _, _, _) => sigSymsOf(exp)
    case Expr.FixpointProject(_, exp, _, _, _) => sigSymsOf(exp)
    case Expr.Error(_, _, _) => Set.empty
  }

  /**
    * Creates an iterable over all the instance defs in `root`.
    */
  def instanceDefsOf(root: Root): Iterable[Def] = {
    for {
      instsPerClass <- root.instances.values
      inst <- instsPerClass
      defn <- inst.defs
    } yield defn
  }

  /**
    * Returns the free variables in the given expression `exp0`.
    */
  def freeVars(exp0: Expr): Map[Symbol.VarSym, Type] = exp0 match {
    case Expr.Cst(_, _, _) => Map.empty

    case Expr.Var(sym, tpe, _) => Map(sym -> tpe)

    case Expr.Hole(_, _, _, _) => Map.empty

    case Expr.HoleWithExp(exp, _, _, _) =>
      freeVars(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      freeVars(exp)

    case Expr.Use(_, _, exp, _) =>
      freeVars(exp)

    case Expr.Lambda(fparam, exp, _, _) =>
      freeVars(exp) - fparam.bnd.sym

    case Expr.ApplyClo(exp, exps, _, _, _) =>
      exps.foldLeft(freeVars(exp)) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.ApplyDef(_, exps, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.ApplySig(_, exps, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.Unary(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.Let(bnd, exp1, exp2, _, _, _) =>
      (freeVars(exp1) ++ freeVars(exp2)) - bnd.sym

    case Expr.LocalDef(TypedAst.Binder(sym, _), fparams, exp1, exp2, _, _, _) =>
      val bound = sym :: fparams.map(_.bnd.sym)
      (freeVars(exp1) -- bound) ++ (freeVars(exp2) - sym)

    case Expr.Region(_, _) =>
      Map.empty

    case Expr.Scope(Binder(sym, _), _, exp, _, _, _) =>
      freeVars(exp) - sym

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.Discard(exp, _, _) =>
      freeVars(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, MatchRule(pat, guard, exp)) =>
          acc ++ ((guard.map(freeVars).getOrElse(Map.empty) ++ freeVars(exp)) -- freeVars(pat).keys)
      }

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, TypeMatchRule(bnd, _, exp)) => acc ++ (freeVars(exp) - bnd.sym)
      }

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      val e = freeVars(exp)
      val rs = rules.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, RestrictableChooseRule(pat, exp)) => acc ++ (freeVars(exp) -- freeVars(pat).toList)
      }
      e ++ rs

    case Expr.ApplyTag(_, exps, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.ApplyRestrictableTag(_, exps, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expr.Tuple(elms, _, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expr.RecordEmpty(_, _) => Map.empty

    case Expr.RecordSelect(exp, _, _, _, _) =>
      freeVars(exp)

    case Expr.RecordExtend(_, value, rest, _, _, _) =>
      freeVars(value) ++ freeVars(rest)

    case Expr.RecordRestrict(_, rest, _, _, _) =>
      freeVars(rest)

    case Expr.ArrayLit(elms, exp, _, _, _) =>
      elms.foldLeft(freeVars(exp)) {
        case (acc, e) => acc ++ freeVars(e)
      }

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expr.ArrayLoad(base, index, _, _, _) =>
      freeVars(base) ++ freeVars(index)

    case Expr.ArrayLength(base, _, _) =>
      freeVars(base)

    case Expr.ArrayStore(base, index, elm, _, _) =>
      freeVars(base) ++ freeVars(index) ++ freeVars(elm)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      freeVars(region) ++ fields.flatMap { case (_, v) => freeVars(v) }

    case Expr.StructGet(e, _, _, _, _) =>
      freeVars(e)

    case Expr.StructPut(e1, _, e2, _, _, _) =>
      freeVars(e1) ++ freeVars(e2)

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

    case Expr.Without(exp, _, _, _, _) =>
      freeVars(exp)

    case Expr.InstanceOf(exp, _, _) =>
      freeVars(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Expr.UncheckedMaskingCast(exp, _, _, _) =>
      freeVars(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, CatchRule(bnd, _, exp)) => acc ++ freeVars(exp) - bnd.sym
      }

    case Expr.Throw(exp, _, _, _) => freeVars(exp)

    case Expr.TryWith(exp, _, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, HandlerRule(_, fparams, exp)) => acc ++ freeVars(exp) -- fparams.map(_.bnd.sym)
      }

    case Expr.Do(_, exps, _, _, _) =>
      exps.flatMap(freeVars).toMap

    case Expr.InvokeConstructor(_, args, _, _, _) =>
      args.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expr.InvokeMethod(_, exp, args, _, _, _) =>
      args.foldLeft(freeVars(exp)) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expr.InvokeStaticMethod(_, args, _, _, _) =>
      args.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expr.GetField(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      Map.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _)) => acc ++ freeVars(exp) -- fparams.map(_.bnd.sym)
      }

    case Expr.NewChannel(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.GetChannel(exp, _, _, _) =>
      freeVars(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      val d = default.map(freeVars).getOrElse(Map.empty)
      rules.foldLeft(d) {
        case (acc, SelectChannelRule(Binder(sym, _), chan, exp)) => acc ++ ((freeVars(chan) ++ freeVars(exp)) - sym)
      }

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      val freeFragVars = frags.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, ParYieldFragment(p, e, _)) => acc ++ freeVars(p) ++ freeVars(e)
      }
      freeVars(exp) -- freeFragVars.keys

    case Expr.Lazy(exp, _, _) =>
      freeVars(exp)

    case Expr.Force(exp, _, _, _) =>
      freeVars(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, c) => acc ++ freeVars(c)
      }

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      freeVars(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      freeVars(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      freeVars(exp)

    case Expr.Error(_, _, _) =>
      Map.empty

  }

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
    case Pattern.Wild(_, _) => Map.empty
    case Pattern.Var(Binder(sym, _), tpe, _) => Map(sym -> tpe)
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

    case Pattern.RecordEmpty(_, _) => Map.empty
    case Pattern.Error(_, _) => Map.empty
  }

  /**
    * Returns the free variables in the given restrictable pattern `pat0`.
    */
  private def freeVars(pat0: RestrictableChoosePattern): Set[Symbol.VarSym] = pat0 match {
    case RestrictableChoosePattern.Tag(_, pat, _, _) => pat.flatMap(freeVars).toSet
    case RestrictableChoosePattern.Error(_, _) => Set.empty
  }

  private def freeVars(v: RestrictableChoosePattern.VarOrWild): Option[Symbol.VarSym] = v match {
    case RestrictableChoosePattern.Wild(_, _) => None
    case RestrictableChoosePattern.Var(Binder(sym, _), _, _) => Some(sym)
    case RestrictableChoosePattern.Error(_, _) => None
  }

  /**
    * Returns the free variables in the given constraint `constraint0`.
    */
  private def freeVars(constraint0: Constraint): Map[Symbol.VarSym, Type] = constraint0 match {
    case Constraint(cparams0, head, body, _) =>
      (freeVars(head) ++ body.flatMap(freeVars)) -- cparams0.map(_.bnd.sym)
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


}
