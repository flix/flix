package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.Ast.Annotation.{Benchmark, Law, Lint, Test}
import ca.uwaterloo.flix.language.ast.Ast.HoleContext
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

      case Expression.Hole(sym, tpe, eff, loc) => Map(sym -> HoleContext(sym, tpe, env0))

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

      case Expression.Apply(exp, exps, tpe, eff, loc) =>
        val init = visitExp(exp, env0)
        exps.foldLeft(init) {
          case (acc, exp) => acc ++ visitExp(exp, env0)
        }

      case Expression.Unary(sop, exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0 + (sym -> exp1.tpe))

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0) ++ visitExp(exp3, env0)

      case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Match(matchExp, rules, tpe, eff, loc) =>
        val m = visitExp(matchExp, env0)
        rules.foldLeft(m) {
          case (macc, MatchRule(pat, guard, exp)) =>
            macc ++ visitExp(guard, env0) ++ visitExp(exp, binds(pat) ++ env0)
        }

      case Expression.Choose(exps, rules, tpe, eff, loc) =>
        val m1 = exps.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (acc, exp) => acc ++ visitExp(exp, env0)
        }
        val m2 = rules.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (acc, ChoiceRule(pat, exp)) =>
            val env1 = (pat.zip(exps)).foldLeft(Map.empty[Symbol.VarSym, Type]) {
              case (acc, (ChoicePattern.Wild(_), exp)) => acc
              case (acc, (ChoicePattern.Absent(_), exp)) => acc
              case (acc, (ChoicePattern.Present(sym, _, _), exp)) => acc + (sym -> exp.tpe)
            }
            acc ++ visitExp(exp, env0 ++ env1)
        }
        m1 ++ m2

      case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Tuple(elms, tpe, eff, loc) =>
        elms.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, elm) => macc ++ visitExp(elm, env0)
        }

      case Expression.RecordEmpty(tpe, loc) => Map.empty

      case Expression.RecordSelect(base, _, tpe, eff, loc) =>
        visitExp(base, env0)

      case Expression.RecordExtend(_, value, rest, tpe, eff, loc) =>
        visitExp(rest, env0) ++ visitExp(value, env0)

      case Expression.RecordRestrict(_, rest, tpe, eff, loc) =>
        visitExp(rest, env0)

      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        elms.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, elm) => macc ++ visitExp(elm, env0)
        }

      case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        visitExp(elm, env0)

      case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        visitExp(base, env0) ++ visitExp(index, env0)

      case Expression.ArrayStore(base, index, elm, loc) =>
        visitExp(base, env0) ++ visitExp(index, env0) ++ visitExp(elm, env0)

      case Expression.ArrayLength(base, eff, loc) =>
        visitExp(base, env0)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        visitExp(base, env0) ++ visitExp(beginIndex, env0) ++ visitExp(endIndex, env0)

      case Expression.Ref(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Deref(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.Existential(fparam, exp, loc) =>
        visitExp(exp, env0 + (fparam.sym -> fparam.tpe))

      case Expression.Universal(fparam, exp, loc) =>
        visitExp(exp, env0 + (fparam.sym -> fparam.tpe))

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.Cast(exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        rules.foldLeft(visitExp(exp, env0)) {
          case (macc, CatchRule(sym, clazz, body)) => macc ++ visitExp(body, env0 + (sym -> Type.mkNative(null)))
        }

      case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
        args.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
        args.foldLeft(visitExp(exp, env0)) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
        args.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, arg) => macc ++ visitExp(arg, env0)
        }

      case Expression.GetField(field, exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.GetStaticField(field, tpe, eff, loc) =>
        Map.empty

      case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp, env0)

      case Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp, env0)

      case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        val rs = rules.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, SelectChannelRule(sym, chan, exp)) => macc ++ visitExp(chan, env0) ++ visitExp(exp, env0)
        }

        val d = default.map(visitExp(_, env0)).getOrElse(Map.empty)

        rs ++ d

      case Expression.Spawn(exp, tpe, eff, loc) => visitExp(exp, env0)

      case Expression.Lazy(exp, tpe, loc) => visitExp(exp, env0)

      case Expression.Force(exp, tpe, eff, loc) => visitExp(exp, env0)

      case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
        cs.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
          case (macc, c) => macc ++ visitConstraint(c, env0)
        }

      case Expression.FixpointCompose(exp1, exp2, stf, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.FixpointProject(_, exp, tpe, eff, loc) =>
        visitExp(exp, env0)

      case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
        visitExp(exp1, env0) ++ visitExp(exp2, env0)

      case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, eff, loc) =>
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
      case Predicate.Head.Union(exp, tpe, loc) => visitExp(exp, env0)
    }

    /**
      * Finds the holes and hole contexts in the given body predicate `b0`.
      */
    def visitBody(b0: Predicate.Body, env0: Map[Symbol.VarSym, Type]): Map[Symbol.HoleSym, HoleContext] = b0 match {
      case Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) => Map.empty
      case Predicate.Body.Guard(exp, loc) => visitExp(exp, env0)
    }

    /**
      * Returns the set of variables bound by the given list of formal parameters `fparams`.
      */
    def getEnvFromParams(fparams: List[FormalParam]): Map[Symbol.VarSym, Type] =
      fparams.foldLeft(Map.empty[Symbol.VarSym, Type]) {
        case (macc, FormalParam(sym, mod, tpe, loc)) => macc + (sym -> tpe)
      }

    // Visit every definition.
    root.defs.foldLeft(Map.empty[Symbol.HoleSym, HoleContext]) {
      case (macc, (sym, defn)) => macc ++ visitExp(defn.exp, getEnvFromParams(defn.fparams))
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
    case Expression.Hole(_, _, _, _) => Set.empty
    case Expression.Lambda(_, exp, _, _) => sigSymsOf(exp)
    case Expression.Apply(exp, exps, _, _, _) => sigSymsOf(exp) ++ exps.flatMap(sigSymsOf)
    case Expression.Unary(_, exp, _, _, _) => sigSymsOf(exp)
    case Expression.Binary(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Let(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
    case Expression.Stm(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Match(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp) ++ sigSymsOf(rule.guard))
    case Expression.Choose(exps, rules, _, _, _) => exps.flatMap(sigSymsOf).toSet ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.Tag(_, _, exp, _, _, _) => sigSymsOf(exp)
    case Expression.Tuple(elms, _, _, _) => elms.flatMap(sigSymsOf).toSet
    case Expression.RecordEmpty(_, _) => Set.empty
    case Expression.RecordSelect(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.RecordExtend(_, value, rest, _, _, _) => sigSymsOf(value) ++ sigSymsOf(rest)
    case Expression.RecordRestrict(_, rest, _, _, _) => sigSymsOf(rest)
    case Expression.ArrayLit(elms, _, _, _) => elms.flatMap(sigSymsOf).toSet
    case Expression.ArrayNew(elm, len, _, _, _) => sigSymsOf(elm) ++ sigSymsOf(len)
    case Expression.ArrayLoad(base, index, _, _, _) => sigSymsOf(base) ++ sigSymsOf(index)
    case Expression.ArrayLength(base, _, _) => sigSymsOf(base)
    case Expression.ArrayStore(base, index, elm, _) => sigSymsOf(base) ++ sigSymsOf(index) ++ sigSymsOf(elm)
    case Expression.ArraySlice(base, beginIndex, endIndex, _, _) => sigSymsOf(base) ++ sigSymsOf(beginIndex) ++ sigSymsOf(endIndex)
    case Expression.Ref(exp, _, _, _) => sigSymsOf(exp)
    case Expression.Deref(exp, _, _, _) => sigSymsOf(exp)
    case Expression.Assign(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.Existential(_, exp, _) => sigSymsOf(exp)
    case Expression.Universal(_, exp, _) => sigSymsOf(exp)
    case Expression.Ascribe(exp, _, _, _) => sigSymsOf(exp)
    case Expression.Cast(exp, _, _, _) => sigSymsOf(exp)
    case Expression.TryCatch(exp, rules, _, _, _) => sigSymsOf(exp) ++ rules.flatMap(rule => sigSymsOf(rule.exp))
    case Expression.InvokeConstructor(_, args, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Expression.InvokeMethod(_, exp, args, _, _, _) => sigSymsOf(exp) ++ args.flatMap(sigSymsOf)
    case Expression.InvokeStaticMethod(_, args, _, _, _) => args.flatMap(sigSymsOf).toSet
    case Expression.GetField(_, exp, _, _, _) => sigSymsOf(exp)
    case Expression.PutField(_, exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.GetStaticField(_, _, _, _) => Set.empty
    case Expression.PutStaticField(_, exp, _, _, _) => sigSymsOf(exp)
    case Expression.NewChannel(exp, _, _, _) => sigSymsOf(exp)
    case Expression.GetChannel(exp, _, _, _) => sigSymsOf(exp)
    case Expression.PutChannel(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.SelectChannel(rules, default, _, _, _) => rules.flatMap(rule => sigSymsOf(rule.chan) ++ sigSymsOf(rule.exp)).toSet ++ default.toSet.flatMap(sigSymsOf)
    case Expression.Spawn(exp, _, _, _) => sigSymsOf(exp)
    case Expression.Lazy(exp, _, _) => sigSymsOf(exp)
    case Expression.Force(exp, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointConstraintSet(_, _, _, _) => Set.empty
    case Expression.FixpointCompose(exp1, exp2, _, _, _, _) => sigSymsOf(exp1) ++ (sigSymsOf(exp2))
    case Expression.FixpointSolve(exp, _, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointProject(_, exp, _, _, _) => sigSymsOf(exp)
    case Expression.FixpointEntails(exp1, exp2, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2)
    case Expression.FixpointFold(_, exp1, exp2, exp3, _, _, _) => sigSymsOf(exp1) ++ sigSymsOf(exp2) ++ sigSymsOf(exp3)
  }

  // MATT also use this for Redundancy after that merges
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
    * Returns `true` if the given annotations contains the [[Law]] annotation.
    */
  def isLaw(xs: List[Annotation]): Boolean = xs.exists {
    case Annotation(name, _, _) => name.isInstanceOf[Law]
  }

  /**
    * Returns `true` if the given annotations contains the [[Lint]] annotation.
    */
  def isLint(xs: List[Annotation]): Boolean = xs.exists {
    case Annotation(name, _, _) => name.isInstanceOf[Lint]
  }

  /**
    * Returns `true` if the given annotations contains the [[Test]] annotation.
    */
  def isTest(xs: List[Annotation]): Boolean = xs.exists {
    case Annotation(name, _, _) => name.isInstanceOf[Test]
  }

}
