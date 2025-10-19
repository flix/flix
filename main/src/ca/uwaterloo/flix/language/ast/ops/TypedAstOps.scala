package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.shared.SymUse.SigSymUse
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}

object TypedAstOps {

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  def freeVarsOf(pat0: Pattern): Set[Symbol.VarSym] = binds(pat0).keySet

  /**
    * Returns the set of variable symbols bound by the given pattern `pat0`.
    */
  private def binds(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
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
    case Pattern.Error(_, _) => Map.empty
  }

  /**
    * Creates a set of all the sigs used in the given `exp`.
    */
  def sigSymsOf(exp0: Exp): Set[Symbol.SigSym] = exp0 match {
    case Exp.Cst(_, _, _) => Set.empty
    case Exp.Var(_, _, _) => Set.empty
    case Exp.Hole(_, _, _, _, _) => Set.empty
    case Exp.HoleWithExp(exp, _, _, _, _) => sigSymsOf(exp)
    case Exp.OpenAs(_, exp, _, _) => sigSymsOf(exp)
    case Exp.Use(_, _, exp, _) => sigSymsOf(exp)
    case Exp.Lambda(_, exp, _, _) => sigSymsOf(exp)
    case Exp.ApplyClo(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.ApplyDef(_, exps, _, _, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.ApplyLocalDef(_, exps, _, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.ApplyOp(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.ApplySig(SigSymUse(sym, _), exps, _, _, _, _, _, _) => exps.flatMap(sigSymsOf).toSet + sym
    case Exp.Unary(_, exp, _, _, _) => sigSymsOf(exp)
    case Exp.Binary(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.Let(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.LocalDef(_, _, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.Region(_, _, exp, _, _, _) => sigSymsOf(exp)
    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Exp.Stm(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.Discard(exp, _, _) => sigSymsOf(exp)
    case Exp.Match(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp) ++ rule.guard.toList.flatMap(sigSymsOf))
    case Exp.TypeMatch(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Exp.RestrictableChoose(_, exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Exp.ExtMatch(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(r => sigSymsOf(r.exp))
    case Exp.Tag(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.RestrictableTag(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.ExtTag(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.Tuple(elms, _, _, _) => elms.flatMap(sigSymsOf).toSet
    case Exp.RecordSelect(exp, _, _, _, _) => sigSymsOf(exp)
    case Exp.RecordExtend(_, value, rest, _, _, _) => sigSymsOf(value) ++ sigSymsOf(rest)
    case Exp.RecordRestrict(_, rest, _, _, _) => sigSymsOf(rest)
    case Exp.ArrayLit(exps, exp, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ sigSymsOf(exp)
    case Exp.ArrayNew(exp1, exp2, exp3, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Exp.ArrayLoad(base, index, _, _, _) => sigSymsOf(base) ++ sigSymsOf(index)
    case Exp.ArrayLength(base, _, _) => sigSymsOf(base)
    case Exp.ArrayStore(base, index, elm, _, _) => sigSymsOf(base) ++ sigSymsOf(index) ++ sigSymsOf(elm)
    case Exp.StructNew(_, fields, region, _, _, _) => sigSymsOf(region) ++ fields.flatMap { case (_, v) => sigSymsOf(v) }
    case Exp.StructGet(e, _, _, _, _) => sigSymsOf(e)
    case Exp.StructPut(e1, _, e2, _, _, _) => sigSymsOf(e1) ++ sigSymsOf(e2)
    case Exp.VectorLit(exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.VectorLoad(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.VectorLength(exp, _) => sigSymsOf(exp)
    case Exp.Ascribe(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Exp.InstanceOf(exp, _, _) => sigSymsOf(exp)
    case Exp.CheckedCast(_, exp, _, _, _) => sigSymsOf(exp)
    case Exp.UncheckedCast(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Exp.Unsafe(exp, _, _, _, _) => sigSymsOf(exp)
    case Exp.Without(exp, _, _, _, _) => sigSymsOf(exp)
    case Exp.TryCatch(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Exp.Throw(exp, _, _, _) => sigSymsOf(exp)
    case Exp.Handler(_, rules, _, _, _, _, _) => rules.flatMap(rule => sigSymsOf(rule.exp)).toSet
    case Exp.RunWith(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.InvokeConstructor(_, args, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Exp.InvokeMethod(_, exp, args, _, _, _) => sigSymsOf(exp) ++ args.flatMap(sigSymsOf)
    case Exp.InvokeStaticMethod(_, args, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Exp.GetField(_, exp, _, _, _) => sigSymsOf(exp)
    case Exp.PutField(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.GetStaticField(_, _, _, _) => Set.empty
    case Exp.PutStaticField(_, exp, _, _, _) => sigSymsOf(exp)
    case Exp.NewObject(_, _, _, _, methods, _) => methods.flatMap(method => sigSymsOf(method.exp)).toSet
    case Exp.NewChannel(exp, _, _, _) => sigSymsOf(exp)
    case Exp.GetChannel(exp, _, _, _) => sigSymsOf(exp)
    case Exp.PutChannel(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.SelectChannel(rules, default, _, _, _) => rules.flatMap(rule => sigSymsOf(rule.chan) ++ sigSymsOf(rule.exp)).toSet ++ default.toSet.flatMap(sigSymsOf)
    case Exp.Spawn(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.ParYield(frags, exp, _, _, _) => sigSymsOf(exp) ++ frags.flatMap(f => sigSymsOf(f.exp))
    case Exp.Lazy(exp, _, _) => sigSymsOf(exp)
    case Exp.Force(exp, _, _, _) => sigSymsOf(exp)
    case Exp.FixpointConstraintSet(_, _, _) => Set.empty
    case Exp.FixpointLambda(_, exp, _, _, _) => sigSymsOf(exp)
    case Exp.FixpointMerge(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Exp.FixpointQueryWithProvenance(exps, Head.Atom(_, _, terms, _, _), _, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ terms.flatMap(sigSymsOf).toSet
    case Exp.FixpointQueryWithSelect(exps, queryExp, selects, _, where, _, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ sigSymsOf(queryExp) ++ selects.flatMap(sigSymsOf).toSet ++ where.flatMap(sigSymsOf).toSet
    case Exp.FixpointSolveWithProject(exps, _, _, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.FixpointInjectInto(exps, _, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Exp.Error(_, _, _) => Set.empty
  }

  /**
    * Creates an iterable over all the instance defs in `root`.
    */
  def instanceDefsOf(root: Root): Iterable[Def] = {
    for {
      inst <- root.instances.values
      defn <- inst.defs
    } yield defn
  }

  /**
    * Returns the free variables in the given expression `exp0`.
    */
  def freeVars(exp0: Exp): Map[Symbol.VarSym, Type] = exp0 match {
    case Exp.Cst(_, _, _) => Map.empty

    case Exp.Var(sym, tpe, _) => Map(sym -> tpe)

    case Exp.Hole(_, _, _, _, _) => Map.empty

    case Exp.HoleWithExp(exp, _, _, _, _) =>
      freeVars(exp)

    case Exp.OpenAs(_, exp, _, _) =>
      freeVars(exp)

    case Exp.Use(_, _, exp, _) =>
      freeVars(exp)

    case Exp.Lambda(fparam, exp, _, _) =>
      freeVars(exp) - fparam.bnd.sym

    case Exp.ApplyClo(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.ApplyDef(_, exps, _, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Exp.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Exp.ApplyOp(_, exps, _, _, _) =>
      exps.flatMap(freeVars).toMap

    case Exp.ApplySig(_, exps, _, _, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Exp.Unary(_, exp, _, _, _) =>
      freeVars(exp)

    case Exp.Binary(_, exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.Let(bnd, exp1, exp2, _, _, _) =>
      (freeVars(exp1) ++ freeVars(exp2)) - bnd.sym

    case Exp.LocalDef(TypedAst.Binder(sym, _), fparams, exp1, exp2, _, _, _) =>
      val bound = sym :: fparams.map(_.bnd.sym)
      (freeVars(exp1) -- bound) ++ (freeVars(exp2) - sym)

    case Exp.Region(Binder(sym, _), _, exp, _, _, _) =>
      freeVars(exp) - sym

    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Exp.Stm(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.Discard(exp, _, _) =>
      freeVars(exp)

    case Exp.Match(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, MatchRule(pat, guard, body, _)) =>
          acc ++ ((guard.map(freeVars).getOrElse(Map.empty) ++ freeVars(body)) -- freeVars(pat).keys)
      }

    case Exp.TypeMatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, TypeMatchRule(bnd, _, body, _)) => acc ++ (freeVars(body) - bnd.sym)
      }

    case Exp.RestrictableChoose(_, exp, rules, _, _, _) =>
      val e = freeVars(exp)
      val rs = rules.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, RestrictableChooseRule(pat, body)) => acc ++ (freeVars(body) -- freeVars(pat).toList)
      }
      e ++ rs

    case Exp.ExtMatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, ExtMatchRule(pat, exp1, _)) =>
          acc ++ freeVars(exp1) -- freeVars(pat)
      }

    case Exp.Tag(_, exps, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Exp.RestrictableTag(_, exps, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Exp.ExtTag(_, exps, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Exp.Tuple(elms, _, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Exp.RecordSelect(exp, _, _, _, _) =>
      freeVars(exp)

    case Exp.RecordExtend(_, value, rest, _, _, _) =>
      freeVars(value) ++ freeVars(rest)

    case Exp.RecordRestrict(_, rest, _, _, _) =>
      freeVars(rest)

    case Exp.ArrayLit(elms, exp, _, _, _) =>
      elms.foldLeft(freeVars(exp)) {
        case (acc, e) => acc ++ freeVars(e)
      }

    case Exp.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Exp.ArrayLoad(base, index, _, _, _) =>
      freeVars(base) ++ freeVars(index)

    case Exp.ArrayLength(base, _, _) =>
      freeVars(base)

    case Exp.ArrayStore(base, index, elm, _, _) =>
      freeVars(base) ++ freeVars(index) ++ freeVars(elm)

    case Exp.StructNew(_, fields, region, _, _, _) =>
      freeVars(region) ++ fields.flatMap { case (_, v) => freeVars(v) }

    case Exp.StructGet(e, _, _, _, _) =>
      freeVars(e)

    case Exp.StructPut(e1, _, e2, _, _, _) =>
      freeVars(e1) ++ freeVars(e2)

    case Exp.VectorLit(elms, _, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, e) => acc ++ freeVars(e)
      }

    case Exp.VectorLoad(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.VectorLength(exp, _) =>
      freeVars(exp)

    case Exp.Ascribe(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Exp.Without(exp, _, _, _, _) =>
      freeVars(exp)

    case Exp.InstanceOf(exp, _, _) =>
      freeVars(exp)

    case Exp.CheckedCast(_, exp, _, _, _) =>
      freeVars(exp)

    case Exp.UncheckedCast(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Exp.Unsafe(exp, _, _, _, _) =>
      freeVars(exp)

    case Exp.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, CatchRule(bnd, _, body, _)) => acc ++ freeVars(body) - bnd.sym
      }

    case Exp.Throw(exp, _, _, _) => freeVars(exp)

    case Exp.Handler(_, rules, _, _, _, _, _) =>
      rules.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, HandlerRule(_, fparams, exp, _)) => acc ++ freeVars(exp) -- fparams.map(_.bnd.sym)
      }

    case Exp.RunWith(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.InvokeConstructor(_, args, _, _, _) =>
      args.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Exp.InvokeMethod(_, exp, args, _, _, _) =>
      args.foldLeft(freeVars(exp)) {
        case (acc, obj) => acc ++ freeVars(obj)
      }

    case Exp.InvokeStaticMethod(_, args, _, _, _) =>
      args.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Exp.GetField(_, exp, _, _, _) =>
      freeVars(exp)

    case Exp.PutField(_, exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.GetStaticField(_, _, _, _) =>
      Map.empty

    case Exp.PutStaticField(_, exp, _, _, _) =>
      freeVars(exp)

    case Exp.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _)) => acc ++ freeVars(exp) -- fparams.map(_.bnd.sym)
      }

    case Exp.NewChannel(exp, _, _, _) =>
      freeVars(exp)

    case Exp.GetChannel(exp, _, _, _) =>
      freeVars(exp)

    case Exp.PutChannel(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.SelectChannel(rules, default, _, _, _) =>
      val d = default.map(freeVars).getOrElse(Map.empty)
      rules.foldLeft(d) {
        case (acc, SelectChannelRule(Binder(sym, _), chan, exp, _)) => acc ++ ((freeVars(chan) ++ freeVars(exp)) - sym)
      }

    case Exp.Spawn(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.ParYield(frags, exp, _, _, _) =>
      val freeFragVars = frags.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, ParYieldFragment(p, e, _)) => acc ++ freeVars(p) ++ freeVars(e)
      }
      freeVars(exp) -- freeFragVars.keys

    case Exp.Lazy(exp, _, _) =>
      freeVars(exp)

    case Exp.Force(exp, _, _, _) =>
      freeVars(exp)

    case Exp.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, c) => acc ++ freeVars(c)
      }

    case Exp.FixpointLambda(_, exp, _, _, _) =>
      freeVars(exp)

    case Exp.FixpointMerge(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Exp.FixpointQueryWithProvenance(exps, select, _, _, _, _) =>
      freeVars(exps) ++ freeVars(select)

    case Exp.FixpointQueryWithSelect(exps, queryExp, selects, from, where, _, _, _, _) =>
      freeVars(exps) ++ freeVars(queryExp) ++ freeVars(selects) ++ from.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        (acc, b) => acc ++ freeVars(b)
      } ++ freeVars(where)

    case Exp.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        (acc, exp) => acc ++ freeVars(exp)
      }

    case Exp.FixpointInjectInto(exps, _, _, _, _) =>
      freeVars(exps)

    case Exp.Error(_, _, _) =>
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

    case Pattern.Error(_, _) => Map.empty
  }

  /**
    * Returns the free variables in the given restrictable pattern `pat0`.
    */
  private def freeVars(pat0: RestrictableChoosePattern): Set[Symbol.VarSym] = pat0 match {
    case RestrictableChoosePattern.Tag(_, pat, _, _) => pat.flatMap(freeVars).toSet
    case RestrictableChoosePattern.Error(_, _) => Set.empty
  }

  /**
    * Returns the free variables in the given restrictable var-or-wild pattern `v`.
    */
  private def freeVars(v: RestrictableChoosePattern.VarOrWild): Option[Symbol.VarSym] = v match {
    case RestrictableChoosePattern.Wild(_, _) => None
    case RestrictableChoosePattern.Var(Binder(sym, _), _, _) => Some(sym)
    case RestrictableChoosePattern.Error(_, _) => None
  }

  /**
    * Returns the free variables in the given extensible pattern `pat0`.
    */
  private def freeVars(pat0: ExtPattern): Set[Symbol.VarSym] = pat0 match {
    case ExtPattern.Default(_) => Set.empty
    case ExtPattern.Tag(_, pats, _) => pats.toSet.flatMap((v: ExtTagPattern) => freeVars(v))
    case ExtPattern.Error(_) => Set.empty
  }

  /**
    * Returns the free variables in the given ext tag pattern `v`.
    */
  private def freeVars(v: ExtTagPattern): Set[Symbol.VarSym] = v match {
    case ExtTagPattern.Wild(_, _) => Set.empty
    case ExtTagPattern.Var(Binder(sym, _), _, _) => Set(sym)
    case ExtTagPattern.Unit(_, _) => Set.empty
    case ExtTagPattern.Error(_, _) => Set.empty
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

  /**
    * Returns the free variables in the given list of expressions `exp0`.
    */
  private def freeVars(exps0: List[Exp]): Map[Symbol.VarSym, Type] =
    exps0.foldLeft(Map.empty[Symbol.VarSym, Type]) {
      case (acc, exp) => acc ++ freeVars(exp)
    }

}
