package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type}

object TypedAstOps {

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  def freeVarsOf(pat0: Pattern): Set[Symbol.VarSym] = binds(pat0).keySet

  /**
    * Returns the set of variable symbols bound by the given pattern `pat0`.
    */
  def binds(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
    case Pattern.Wild(tpe, loc) => Map.empty
    case Pattern.Var(sym, tpe, loc) => Map(sym -> tpe)
    case Pattern.Cst(_, _, _) => Map.empty
    case Pattern.Tag(sym, pat, tpe, loc) => binds(pat)
    case Pattern.Tuple(elms, tpe, loc) => elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
      case (macc, elm) => macc ++ binds(elm)
    }
  }

  /**
    * Creates a set of all the sigs used in the given `exp`.
    */
  def sigSymsOf(exp0: Expression): Set[Symbol.SigSym] = exp0 match {
    case Expression.Cst(_, _, _) => Set.empty
    case Expression.Wild(_, _) => Set.empty
    case Expression.Var(_, _, _) => Set.empty
    case Expression.Def(_, _, _) => Set.empty
    case Expression.Sig(sym, _, _) => Set(sym)
    case Expression.Hole(_, _, _) => Set.empty
    case Expression.HoleWithExp(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.OpenAs(_, exp, _, _) => sigSymsOf(exp)
    case Expression.Use(_, _, exp, _) => sigSymsOf(exp)
    case Expression.Lambda(_, exp, _, _) => sigSymsOf(exp)
    case Expression.Apply(exp, exps, _, _, _, _) => sigSymsOf(exp) ++ exps.flatMap(sigSymsOf)
    case Expression.Unary(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Binary(_, exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Let(_, _, exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.LetRec(_, _, exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Region(_, _) => Set.empty
    case Expression.Scope(_, _, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.ScopeExit(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Expression.Stm(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Discard(exp, _, _, _) => sigSymsOf(exp)
    case Expression.Match(exp, rules, _, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp) ++ rule.guard.toList.flatMap(sigSymsOf))
    case Expression.TypeMatch(exp, rules, _, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.RelationalChoose(exps, rules, _, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.RestrictableChoose(_, exp, rules, _, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.Tag(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.RestrictableTag(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Tuple(elms, _, _, _, _) => elms.flatMap(sigSymsOf).toSet
    case Expression.RecordEmpty(_, _) => Set.empty
    case Expression.RecordSelect(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.RecordExtend(_, value, rest, _, _, _, _) => sigSymsOf(value) ++ sigSymsOf(rest)
    case Expression.RecordRestrict(_, rest, _, _, _, _) => sigSymsOf(rest)
    case Expression.ArrayLit(exps, exp, _, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ sigSymsOf(exp)
    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Expression.ArrayLoad(base, index, _, _, _, _) => sigSymsOf(base) ++ sigSymsOf(index)
    case Expression.ArrayLength(base, _, _, _) => sigSymsOf(base)
    case Expression.ArrayStore(base, index, elm, _, _, _) => sigSymsOf(base) ++ sigSymsOf(index) ++ sigSymsOf(elm)
    case Expression.VectorLit(exps, _, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expression.VectorLoad(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.VectorLength(exp, _) => sigSymsOf(exp)
    case Expression.Ref(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Deref(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Assign(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Ascribe(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.CheckedCast(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.UncheckedCast(exp, _, _, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.UncheckedMaskingCast(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Without(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.TryCatch(exp, rules, _, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.TryWith(exp, _, rules, _, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.Do(_, exps, _, _, _) => exps.flatMap(sigSymsOf).toSet
    case Expression.Resume(exp, _, _) => sigSymsOf(exp)
    case Expression.InvokeConstructor(_, args, _, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Expression.InvokeMethod(_, exp, args, _, _, _, _) => sigSymsOf(exp) ++ args.flatMap(sigSymsOf)
    case Expression.InvokeStaticMethod(_, args, _, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Expression.GetField(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.PutField(_, exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.GetStaticField(_, _, _, _, _) => Set.empty
    case Expression.PutStaticField(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.NewObject(_, _, _, _, _, methods, _) => methods.flatMap(method => sigSymsOf(method.exp)).toSet
    case Expression.NewChannel(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.GetChannel(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.PutChannel(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.SelectChannel(rules, default, _, _, _, _) => rules.flatMap(rule => sigSymsOf(rule.chan) ++ sigSymsOf(rule.exp)).toSet ++ default.toSet.flatMap(sigSymsOf)
    case Expression.Spawn(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Par(exp, _) => sigSymsOf(exp)
    case Expression.ParYield(frags, exp, _, _, _, _) => sigSymsOf(exp) ++ frags.flatMap(f => sigSymsOf(f.exp))
    case Expression.Lazy(exp, _, _) => sigSymsOf(exp)
    case Expression.Force(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointConstraintSet(_, _, _, _) => Set.empty
    case Expression.FixpointLambda(_, exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.FixpointSolve(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointFilter(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointInject(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointProject(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Instanceof(exp, _, _) => sigSymsOf(exp)
    case Expression.Error(_, _, _, _) => Set.empty
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
  def freeVars(exp0: Expression): Map[Symbol.VarSym, Type] = exp0 match {
    case Expression.Cst(_, _, _) => Map.empty

    case Expression.Wild(_, _) => Map.empty

    case Expression.Var(sym, tpe, _) => Map(sym -> tpe)

    case Expression.Def(_, _, _) => Map.empty

    case Expression.Sig(_, _, _) => Map.empty

    case Expression.Hole(_, _, _) => Map.empty

    case Expression.HoleWithExp(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.OpenAs(_, exp, _, _) =>
      freeVars(exp)

    case Expression.Use(_, _, exp, _) =>
      freeVars(exp)

    case Expression.Lambda(fparam, exp, _, _) =>
      freeVars(exp) - fparam.sym

    case Expression.Apply(exp, exps, _, _, _, _) =>
      exps.foldLeft(freeVars(exp)) {
        case (acc, exp) => freeVars(exp) ++ acc
      }

    case Expression.Unary(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.Let(sym, _, exp1, exp2, _, _, _, _) =>
      (freeVars(exp1) ++ freeVars(exp2)) - sym

    case Expression.LetRec(sym, _, exp1, exp2, _, _, _, _) =>
      (freeVars(exp1) ++ freeVars(exp2)) - sym

    case Expression.Region(_, _) =>
      Map.empty

    case Expression.Scope(sym, _, exp, _, _, _, _) =>
      freeVars(exp) - sym

    case Expression.ScopeExit(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.Discard(exp, _, _, _) =>
      freeVars(exp)

    case Expression.Match(exp, rules, _, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, MatchRule(pat, guard, exp)) =>
          acc ++ ( (guard.map(freeVars).getOrElse(Map.empty) ++ freeVars(exp)) -- freeVars(pat).keys )
      }

    case Expression.TypeMatch(exp, rules, _, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, MatchTypeRule(sym, _, exp)) => acc ++ (freeVars(exp) - sym)
      }

    case Expression.RelationalChoose(exps, rules, _, _, _, _) =>
      val es = exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }
      val rs = rules.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, RelationalChoiceRule(pats, exp)) => acc ++ (freeVars(exp) -- pats.flatMap(freeVars))
      }
      es ++ rs

    case Expression.RestrictableChoose(_, exp, rules, _, _, _, _) =>
      val e = freeVars(exp)
      val rs = rules.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, RestrictableChoiceRule(pat, exp)) => acc ++ (freeVars(exp) -- freeVars(pat).toList)
      }
      e ++ rs

    case Expression.Tag(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.RestrictableTag(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.Tuple(elms, _, _, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expression.RecordEmpty(_, _) => Map.empty

    case Expression.RecordSelect(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _, _) =>
      freeVars(value) ++ freeVars(rest)

    case Expression.RecordRestrict(_, rest, _, _, _, _) =>
      freeVars(rest)

    case Expression.ArrayLit(elms, exp, _, _, _, _) =>
      elms.foldLeft(freeVars(exp)) {
        case (acc, e) => acc ++ freeVars(e)
      }

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expression.ArrayLoad(base, index, _, _, _, _) =>
      freeVars(base) ++ freeVars(index)

    case Expression.ArrayLength(base, _, _, _) =>
      freeVars(base)

    case Expression.ArrayStore(base, index, elm, _, _, _) =>
      freeVars(base) ++ freeVars(index) ++ freeVars(elm)

    case Expression.VectorLit(elms, _, _, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, e) => acc ++ freeVars(e)
      }

    case Expression.VectorLoad(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.VectorLength(exp, _) =>
      freeVars(exp)

    case Expression.Ref(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.Deref(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.Assign(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.Ascribe(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.Without(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Expression.CheckedCast(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.UncheckedCast(exp, _, _, _, _, _, _, _) =>
      freeVars(exp)

    case Expression.UncheckedMaskingCast(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.TryCatch(exp, rules, _, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, CatchRule(sym, _, exp)) => acc ++ freeVars(exp) - sym
      }

    case Expression.TryWith(exp, _, rules, _, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, HandlerRule(_, fparams, exp)) => acc ++ freeVars(exp) -- fparams.map(_.sym)
      }

    case Expression.Do(_, exps, _, _, _) =>
      exps.flatMap(freeVars).toMap

    case Expression.Resume(exp, _, _) =>
      freeVars(exp)

    case Expression.InvokeConstructor(_, args, _, _, _, _) =>
      args.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
      args.foldLeft(freeVars(exp)) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
      args.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }

    case Expression.GetField(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.GetStaticField(_, _, _, _, _) =>
      Map.empty

    case Expression.PutStaticField(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.NewObject(_, _, _, _, _, methods, _) =>
      methods.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _, _)) => acc ++ freeVars(exp) -- fparams.map(_.sym)
      }

    case Expression.NewChannel(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.GetChannel(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.SelectChannel(rules, default, _, _, _, _) =>
      val d = default.map(freeVars).getOrElse(Map.empty)
      rules.foldLeft(d) {
        case (acc, SelectChannelRule(sym, chan, exp)) => acc ++ ((freeVars(chan) ++ freeVars(exp)) - sym)
      }

    case Expression.Spawn(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.Par(exp, _) =>
      freeVars(exp)

    case Expression.ParYield(frags, exp, _, _, _, _) =>
      val freeFragVars = frags.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, ParYieldFragment(p, e, _)) => acc ++ freeVars(p) ++ freeVars(e)
      }
      freeVars(exp) -- freeFragVars.keys

    case Expression.Lazy(exp, _, _) =>
      freeVars(exp)

    case Expression.Force(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, c) => acc ++ freeVars(c)
      }

    case Expression.FixpointLambda(_, exp, _, _, _, _, _) =>
      freeVars(exp)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Expression.FixpointFilter(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.FixpointInject(exp, _, _, _, _, _) =>
      freeVars(exp)

    case Expression.FixpointProject(_, exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.Instanceof(exp, _, _) =>
      freeVars(exp)

    case Expression.Error(_, _, _, _) =>
      Map.empty

  }

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
    case Pattern.Wild(_, _) => Map.empty
    case Pattern.Var(sym, tpe, _) => Map(sym -> tpe)
    case Pattern.Cst(_, _, _) => Map.empty
    case Pattern.Tag(_, pat, _, _) => freeVars(pat)
    case Pattern.Tuple(elms, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, pat) => acc ++ freeVars(pat)
      }
  }

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: RelationalChoicePattern): Set[Symbol.VarSym] = pat0 match {
    case RelationalChoicePattern.Wild(_) => Set.empty
    case RelationalChoicePattern.Absent(_) => Set.empty
    case RelationalChoicePattern.Present(sym, _, _) => Set(sym)
  }

  /**
    * Returns the free variables in the given restrictable pattern `pat0`.
    */
  private def freeVars(pat0: RestrictableChoicePattern): Set[Symbol.VarSym] = pat0 match {
    case RestrictableChoicePattern.Tag(_, pat, _, _) => pat.flatMap(freeVars).toSet
  }

  private def freeVars(v: RestrictableChoicePattern.VarOrWild): Option[Symbol.VarSym] = v match {
    case RestrictableChoicePattern.Wild(_, _) => None
    case RestrictableChoicePattern.Var(sym, _, _) => Some(sym)
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


}
