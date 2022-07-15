package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.Ast.Annotation.{Benchmark, Test}
import ca.uwaterloo.flix.language.ast.Ast.HoleContext
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type}

object TypedAstOps {

  /**
    * Returns a map of the holes in the given ast `root`.
    */
  def holesOf(root: Root): Map[Symbol.HoleSym, HoleContext] = {
    /**
      * Finds the holes and hole contexts in the given expression `exp0`.
      */
    def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, Type]): Map[Symbol.HoleSym, HoleContext] = exp0 match {
      case Expression.Wild(tpe, loc) => Map.empty

      case Expression.Var(sym, tpe, loc) => Map.empty

      case Expression.Def(sym, tpe, loc) => Map.empty

      case Expression.Sig(sym, tpe, loc) => Map.empty

      case Expression.Hole(sym, tpe, loc) => Map(sym -> HoleContext(sym, tpe, env0))

      case Expression.Unit(loc) => Map.empty

      case Expression.Null(tpe, loc) => Map.empty

      case Expression.True(loc) => Map.empty

      case Expression.False(loc) => Map.empty

      case Expression.Char(lit, loc) => Map.empty

      case Expression.Float32(lit, loc) => Map.empty

      case Expression.Float64(lit, loc) => Map.empty

      case Expression.Int8(lit, loc) => Map.empty

      case Expression.Int16(lit, loc) => Map.empty

      case Expression.Int32(lit, loc) => Map.empty

      case Expression.Int64(lit, loc) => Map.empty

      case Expression.BigInt(lit, loc) => Map.empty

      case Expression.Str(lit, loc) => Map.empty

      case Expression.Default(tpe, loc) => Map.empty

      case Expression.Lambda(fparam, exp, tpe, loc) =>
        val env1 = Map(fparam.sym -> fparam.tpe)
        visitExp(exp, env0 ++ env1)

      case Expression.Apply(exp, exps, _, _, _, _) =>
        val init = visitExp(exp, env0)
        exps.foldLeft(init) {
          case (acc, exp) => acc ++ visitExp(exp, env0)
        }

      case Expression.Unary(_, exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Let(sym, _, exp1, exp2, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0 + (sym -> exp1.tpe))

      case Expression.LetRec(sym, _, exp1, exp2, _, _, _, _) =>
        val env1 = env0 + (sym -> exp1.tpe)
        visitExp(exp1, env1) ++ visitExp(exp2, env1)

      case Expression.Region(_, _) =>
        Map.empty

      case Expression.Scope(_, _, exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0) ++ visitExp(exp3, env0)

      case Expression.Stm(exp1, exp2, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Discard(exp, _, _, _) =>
        visitExp(exp, env0)

      case Expression.Match(matchExp, rules, _, _, _, _) =>
        val m = visitExp(matchExp, env0)
        rules.foldLeft(m) {
          case (macc, MatchRule(pat, guard, exp)) =>
            macc ++ visitExp(guard, env0) ++ visitExp(exp, binds(pat) ++ env0)
        }

      case Expression.Choose(exps, rules, _, _, _, _) =>
        val m1 = exps.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (acc, exp) => acc ++ visitExp(exp, env0)
        }
        val m2 = rules.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (acc, ChoiceRule(pat, exp)) =>
            val env1 = pat.zip(exps).foldLeft(Map.empty[Symbol.VarSym, Type]) {
              case (acc, (ChoicePattern.Wild(_), exp)) => acc
              case (acc, (ChoicePattern.Absent(_), exp)) => acc
              case (acc, (ChoicePattern.Present(sym, _, _), exp)) => acc + (sym -> exp.tpe)
            }
            acc ++ visitExp(exp, env0 ++ env1)
        }
        m1 ++ m2

      case Expression.Tag(_, _, exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.Tuple(elms, _, _, _, _) =>
        elms.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, elm) => macc ++ visitExp(elm, env0)
        }

      case Expression.RecordEmpty(tpe, loc) => Map.empty

      case Expression.RecordSelect(base, _, _, _, _, _) =>
        visitExp(base, env0)

      case Expression.RecordExtend(_, value, rest, _, _, _, _) =>
        visitExp(rest, env0) ++ visitExp(value, env0)

      case Expression.RecordRestrict(_, rest, _, _, _, _) =>
        visitExp(rest, env0)

      case Expression.ArrayLit(exps, exp, _, _, _, _) =>
        exps.foldLeft(visitExp(exp, env0)) {
          case (acc, e) => acc ++ visitExp(e, env0)
        }

      case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0) ++ visitExp(exp3, env0)

      case Expression.ArrayLoad(base, index, _, _, _, _) =>
        visitExp(base, env0) ++ visitExp(index, env0)

      case Expression.ArrayStore(base, index, elm, _, _) =>
        visitExp(base, env0) ++ visitExp(index, env0) ++ visitExp(elm, env0)

      case Expression.ArrayLength(base, _, _, _) =>
        visitExp(base, env0)

      case Expression.ArraySlice(base, beginIndex, endIndex, _, _, _) =>
        visitExp(base, env0) ++ visitExp(beginIndex, env0) ++ visitExp(endIndex, env0)

      case Expression.Ref(exp1, exp2, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Deref(exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.Assign(exp1, exp2, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Ascribe(exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.Cast(exp, _, _, _, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.Without(exp, _, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.TryCatch(exp, rules, _, _, _, loc) =>
        rules.foldLeft(visitExp(exp, env0)) {
          case (macc, CatchRule(sym, clazz, body)) => macc ++ visitExp(body, env0 + (sym -> Type.mkNative(null, loc)))
        }

      case Expression.TryWith(exp, _, rules, _, _, _, _) =>
        rules.foldLeft(visitExp(exp, env0)) {
          case (macc, HandlerRule(_, fparams, body)) =>
            val env1 = fparams.map(fparam => fparam.sym -> fparam.tpe)
            macc ++ visitExp(body, env0 ++ env1)
        }

      case Expression.Do(_, exps, _, _, _) =>
        exps.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, exp) => macc ++ visitExp(exp, env0)
        }

      case Expression.Resume(exp, _, _) =>
        visitExp(exp, env0)

      case Expression.InvokeConstructor(_, args, _, _, _, _) =>
        args.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
        args.foldLeft(visitExp(exp, env0)) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
        args.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.GetField(_, exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.GetStaticField(_, _, _, _, _) =>
        Map.empty

      case Expression.PutStaticField(_, exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.NewObject(_, _, _, _, methods, _) =>
        methods.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, JvmMethod(_, fparams, exp, _, _, _, _)) => 
            val env1 = fparams.map(fparam => fparam.sym -> fparam.tpe)
            macc ++ visitExp(exp, env0 ++ env1)
        }

      case Expression.NewChannel(exp, _, _, _, _) => visitExp(exp, env0)

      case Expression.GetChannel(exp, _, _, _, _) => visitExp(exp, env0)

      case Expression.PutChannel(exp1, exp2, _, _, _, _) => visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.SelectChannel(rules, default, _, _, _, _) =>
        val rs = rules.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, SelectChannelRule(sym, chan, exp)) => macc ++ visitExp(chan, env0) ++ visitExp(exp, env0)
        }

        val d = default.map(visitExp(_, env0)).getOrElse(Map.empty)

        rs ++ d

      case Expression.Spawn(exp, _, _, _, _) => visitExp(exp, env0)

      case Expression.Lazy(exp, tpe, loc) => visitExp(exp, env0)

      case Expression.Force(exp, _, _, _, _) => visitExp(exp, env0)

      case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
        cs.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, c) => macc ++ visitConstraint(c, env0)
        }

      case Expression.FixpointLambda(_, exp, _, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.FixpointSolve(exp, _, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.FixpointFilter(_, exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.FixpointInject(exp, _, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.FixpointProject(_, exp, _, _, _, _) =>
        visitExp(exp, env0)

      case Expression.Reify(_, _, _, _, _) =>
        Map.empty

      case Expression.ReifyType(_, _, _, _, _, _) =>
        Map.empty

      case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _, _) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0) ++ visitExp(exp3, env0)

    }

    /**
      * Finds the holes and hole contexts in the given constraint `c0`.
      */
    def visitConstraint(c0: Constraint, env0: Map[Symbol.VarSym, Type]): Map[Symbol.HoleSym, HoleContext] = c0 match {
      case Constraint(cparams, head, body, loc) => visitHead(head, env0) ++ body.flatMap(visitBody(_, env0))
    }

    /**
      * Finds the holes and hole contexts in the given head predicate `h0`.
      */
    def visitHead(h0: Predicate.Head, env0: Map[Symbol.VarSym, Type]): Map[Symbol.HoleSym, HoleContext] = h0 match {
      case Predicate.Head.Atom(pred, den, terms, tpe, loc) => Map.empty
    }

    /**
      * Finds the holes and hole contexts in the given body predicate `b0`.
      */
    def visitBody(b0: Predicate.Body, env0: Map[Symbol.VarSym, Type]): Map[Symbol.HoleSym, HoleContext] = b0 match {
      case Predicate.Body.Atom(_, _, _, _, _, _, _) => Map.empty
      case Predicate.Body.Guard(exp, _) => visitExp(exp, env0)
      case Predicate.Body.Loop(_, exp, _) => visitExp(exp, env0)
    }

    /**
      * Returns the set of variables bound by the given list of formal parameters `fparams`.
      */
    def getEnvFromParams(fparams: List[FormalParam]): Map[Symbol.VarSym, Type] =
      fparams.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (macc, FormalParam(sym, mod, tpe, src, loc)) => macc + (sym -> tpe)
      }

    // Visit every definition.
    root.defs.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
      case (macc, (sym, defn)) => macc ++ visitExp(defn.impl.exp, getEnvFromParams(defn.spec.fparams))
    }
  }

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
    case Pattern.Unit(loc) => Map.empty
    case Pattern.True(loc) => Map.empty
    case Pattern.False(loc) => Map.empty
    case Pattern.Char(lit, loc) => Map.empty
    case Pattern.Float32(lit, loc) => Map.empty
    case Pattern.Float64(lit, loc) => Map.empty
    case Pattern.Int8(lit, loc) => Map.empty
    case Pattern.Int16(lit, loc) => Map.empty
    case Pattern.Int32(lit, loc) => Map.empty
    case Pattern.Int64(lit, loc) => Map.empty
    case Pattern.BigInt(lit, loc) => Map.empty
    case Pattern.Str(lit, loc) => Map.empty
    case Pattern.Tag(sym, tag, pat, tpe, loc) => binds(pat)
    case Pattern.Tuple(elms, tpe, loc) => elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
      case (macc, elm) => macc ++ binds(elm)
    }
    case Pattern.Array(elms, tpe, loc) => elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
      case (macc, elm) => macc ++ binds(elm)
    }
    case Pattern.ArrayTailSpread(elms, sym, tpe, loc) =>
      val boundElms = elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (macc, elm) => macc ++ binds(elm)
      }
      Map(sym -> tpe) ++ boundElms

    case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) =>
      val boundElms = elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (macc, elm) => macc ++ binds(elm)
      }
      Map(sym -> tpe) ++ boundElms
  }

  /**
    * Creates a set of all the sigs used in the given `exp`.
    */
  def sigSymsOf(exp: Expression): Set[Symbol.SigSym] = exp match {
    case Expression.Unit(_) => Set.empty
    case Expression.Null(_, _) => Set.empty
    case Expression.True(_) => Set.empty
    case Expression.False(_) => Set.empty
    case Expression.Char(_, _) => Set.empty
    case Expression.Float32(_, _) => Set.empty
    case Expression.Float64(_, _) => Set.empty
    case Expression.Int8(_, _) => Set.empty
    case Expression.Int16(_, _) => Set.empty
    case Expression.Int32(_, _) => Set.empty
    case Expression.Int64(_, _) => Set.empty
    case Expression.BigInt(_, _) => Set.empty
    case Expression.Str(_, _) => Set.empty
    case Expression.Default(_, _) => Set.empty
    case Expression.Wild(_, _) => Set.empty
    case Expression.Var(_, _, _) => Set.empty
    case Expression.Def(_, _, _) => Set.empty
    case Expression.Sig(sym, _, _) => Set(sym)
    case Expression.Hole(_, _, _) => Set.empty
    case Expression.Lambda(_, exp, _, _) => sigSymsOf(exp)
    case Expression.Apply(exp, exps, _, _, _, _) => sigSymsOf(exp) ++ exps.flatMap(sigSymsOf)
    case Expression.Unary(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Binary(_, exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Let(_, _, exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.LetRec(_, _, exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Region(_, _) => Set.empty
    case Expression.Scope(_, _, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Expression.Stm(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Discard(exp, _, _, _) => sigSymsOf(exp)
    case Expression.Match(exp, rules, _, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp) ++ sigSymsOf(rule.guard))
    case Expression.Choose(exps, rules, _, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.Tag(_, _, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Tuple(elms, _, _, _, _) => elms.flatMap(sigSymsOf).toSet
    case Expression.RecordEmpty(_, _) => Set.empty
    case Expression.RecordSelect(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.RecordExtend(_, value, rest, _, _, _, _) => sigSymsOf(value) ++ sigSymsOf(rest)
    case Expression.RecordRestrict(_, rest, _, _, _, _) => sigSymsOf(rest)
    case Expression.ArrayLit(exps, exp, _, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ sigSymsOf(exp)
    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Expression.ArrayLoad(base, index, _, _, _, _) => sigSymsOf(base) ++ sigSymsOf(index)
    case Expression.ArrayLength(base, _, _, _) => sigSymsOf(base)
    case Expression.ArrayStore(base, index, elm, _, _) => sigSymsOf(base) ++ sigSymsOf(index) ++ sigSymsOf(elm)
    case Expression.ArraySlice(base, beginIndex, endIndex, _, _, _) => sigSymsOf(base) ++ sigSymsOf(beginIndex) ++ sigSymsOf(endIndex)
    case Expression.Ref(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Deref(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Assign(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Ascribe(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Cast(exp, _, _, _, _, _, _, _) => sigSymsOf(exp)
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
    case Expression.NewObject(_, _, _, _, methods, _) => methods.flatMap(method => sigSymsOf(method.exp)).toSet
    case Expression.NewChannel(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.GetChannel(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.PutChannel(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.SelectChannel(rules, default, _, _, _, _) => rules.flatMap(rule => sigSymsOf(rule.chan) ++ sigSymsOf(rule.exp)).toSet ++ default.toSet.flatMap(sigSymsOf)
    case Expression.Spawn(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Lazy(exp, _, _) => sigSymsOf(exp)
    case Expression.Force(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointConstraintSet(_, _, _, _) => Set.empty
    case Expression.FixpointLambda(_, exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.FixpointSolve(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointFilter(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointInject(exp, _, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointProject(_, exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.Reify(_, _, _, _, _) => Set.empty
    case Expression.ReifyType(_, _, _, _, _, _) => Set.empty
    case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
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
    * Returns `true` if the given annotations contains the [[Benchmark]] annotation.
    */
  def isBenchmark(xs: List[Annotation]): Boolean = xs.exists {
    case Annotation(name, _, _) => name.isInstanceOf[Benchmark]
  }

  /**
    * Returns `true` if the given annotations contains the [[Test]] annotation.
    */
  def isTest(xs: List[Annotation]): Boolean = xs.exists {
    case Annotation(name, _, _) => name.isInstanceOf[Test]
  }

  /**
    * Returns the free variables in the given expression `exp0`.
    */
  def freeVars(exp0: Expression): Map[Symbol.VarSym, Type] = exp0 match {
    case Expression.Unit(_) => Map.empty

    case Expression.Null(_, _) => Map.empty

    case Expression.True(_) => Map.empty

    case Expression.False(_) => Map.empty

    case Expression.Char(_, _) => Map.empty

    case Expression.Float32(_, _) => Map.empty

    case Expression.Float64(_, _) => Map.empty

    case Expression.Int8(_, _) => Map.empty

    case Expression.Int16(_, _) => Map.empty

    case Expression.Int32(_, _) => Map.empty

    case Expression.Int64(_, _) => Map.empty

    case Expression.BigInt(_, _) => Map.empty

    case Expression.Str(_, _) => Map.empty

    case Expression.Default(_, _) => Map.empty

    case Expression.Wild(_, _) => Map.empty

    case Expression.Var(sym, tpe, _) => Map(sym -> tpe)

    case Expression.Def(_, _, _) => Map.empty

    case Expression.Sig(_, _, _) => Map.empty

    case Expression.Hole(_, _, _) => Map.empty

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

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.Discard(exp, _, _, _) =>
      freeVars(exp)

    case Expression.Match(exp, rules, _, _, _, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (acc, MatchRule(pat, guard, exp)) => acc ++ (freeVars(guard) ++ freeVars(exp)) -- freeVars(pat).keys
      }

    case Expression.Choose(exps, rules, _, _, _, _) =>
      val es = exps.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, exp) => acc ++ freeVars(exp)
      }
      val rs = rules.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, ChoiceRule(pats, exp)) => acc ++ (freeVars(exp) -- pats.flatMap(freeVars))
      }
      es ++ rs

    case Expression.Tag(_, _, exp, _, _, _, _) =>
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

    case Expression.ArrayStore(base, index, elm, _, _) =>
      freeVars(base) ++ freeVars(index) ++ freeVars(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _, _) =>
      freeVars(base) ++ freeVars(beginIndex) ++ freeVars(endIndex)

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

    case Expression.Cast(exp, _, _, _, _, _, _, _) =>
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

    case Expression.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _, _)) => acc ++ freeVars(exp) -- fparams.map(_.sym)
      }

    case Expression.NewChannel(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.GetChannel(exp, _, _, _, _) =>
      freeVars(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.SelectChannel(rules, default, _, _, _, _) =>
      val d = default.map(freeVars).getOrElse(Map.empty)
      rules.foldLeft(d) {
        case (acc, SelectChannelRule(sym, chan, exp)) => acc ++ ((freeVars(chan) ++ freeVars(exp)) - sym)
      }

    case Expression.Spawn(exp, _, _, _, _) =>
      freeVars(exp)

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

    case Expression.Reify(_, _, _, _, _) =>
      Map.empty

    case Expression.ReifyType(_, _, _, _, _, _) =>
      Map.empty

    case Expression.ReifyEff(sym, exp1, exp2, exp3, _, _, _, _) =>
      (freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)) - sym

  }

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: Pattern): Map[Symbol.VarSym, Type] = pat0 match {
    case Pattern.Wild(_, _) => Map.empty
    case Pattern.Var(sym, tpe, _) => Map(sym -> tpe)
    case Pattern.Unit(_) => Map.empty
    case Pattern.True(_) => Map.empty
    case Pattern.False(_) => Map.empty
    case Pattern.Char(_, _) => Map.empty
    case Pattern.Float32(_, _) => Map.empty
    case Pattern.Float64(_, _) => Map.empty
    case Pattern.Int8(_, _) => Map.empty
    case Pattern.Int16(_, _) => Map.empty
    case Pattern.Int32(_, _) => Map.empty
    case Pattern.Int64(_, _) => Map.empty
    case Pattern.BigInt(_, _) => Map.empty
    case Pattern.Str(_, _) => Map.empty
    case Pattern.Tag(_, _, pat, _, _) => freeVars(pat)
    case Pattern.Tuple(elms, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, pat) => acc ++ freeVars(pat)
      }

    case Pattern.Array(elms, _, _) =>
      elms.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (acc, pat) => acc ++ freeVars(pat)
      }

    case Pattern.ArrayTailSpread(elms, sym, _, _) => ??? // TODO

    case Pattern.ArrayHeadSpread(sym, elms, _, _) => ??? // TODO
  }

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: ChoicePattern): Set[Symbol.VarSym] = pat0 match {
    case ChoicePattern.Wild(_) => Set.empty
    case ChoicePattern.Absent(_) => Set.empty
    case ChoicePattern.Present(sym, _, _) => Set(sym)
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
    case Body.Loop(_, exp, _) => freeVars(exp)
  }


}
