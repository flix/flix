package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.Symbol.DefnSym
import ca.uwaterloo.flix.language.ast.ops.SimplifiedAstOps
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol, Type}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
 * Transform curried top-level defs, i.e. functions of the form:
 * def foo(x) = y -> z -> ...
 * into uncurried defs:
 * def foo(x) = y -> z -> ...
 * def foo2(x,y) = z -> ...
 * def foo3(x,y,z) = ...
 * and rewrite calls to foo to calls to the uncurried versions, when possible.
 */
object Uncurrier2 extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  override def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("Uncurrier2") {
    val ctxt = flix.subphase("Uncurrying defs") {
      root.defs.foldLeft(UncurryContext(root.defs, Map.empty[DefnSym, DefnSym])) {
        case (acc, (_, defn)) => uncurryDefn(defn, acc)
      }
    }

    val uncurriedDefs = flix.subphase("Uncurrying calls") {
      ctxt.defs.map {
        case (sym0, defn0) =>
          val body = ctxt.uncurryCall(defn0.exp)
          val defn1 = defn0.copy(exp = body)
          sym0 -> defn1
      }
    }

    val result = root.copy(defs = uncurriedDefs)

    result.toSuccess
  }

  /**
   * A context is a representation of the program as a set of definitions
   * and a map that describes the transformations applied to the definitions to uncurry them.
   * Given a curried top-level def foo, we generate defs foo$1, foo$2, ..., foo$n
   * where foo$1 is the original def foo. foo$2 has arity 2, etc.
   * Invariant: If foo$i is a key in `names`, then it is mapped to foo$i+1 and both foo$i and foo$i+1 are in `defs`.
   */
  private case class UncurryContext(defs: Map[DefnSym, Def], names: Map[DefnSym, DefnSym]) {
    /**
     * Look for function calls of the form: foo(x)(y)
     * in `exp0`, and rewrite into a single function call: foo$2(x,y)
     * where foo maps to foo$2 under `names`.
     * Calls with more than two arguments are uncurried by recursing on foo$2.
     */
    def uncurryCall(exp0: Expression): Expression =
      exp0 match {
        case Expression.Unit => exp0
        case Expression.True => exp0
        case Expression.False => exp0
        case Expression.Char(lit) => exp0
        case Expression.Float32(lit) => exp0
        case Expression.Float64(lit) => exp0
        case Expression.Int8(lit) => exp0
        case Expression.Int16(lit) => exp0
        case Expression.Int32(lit) => exp0
        case Expression.Int64(lit) => exp0
        case Expression.BigInt(lit) => exp0
        case Expression.Str(lit) => exp0

        case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

        case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)

        case Expression.Lambda(args, body, tpe, loc) =>
          val b = uncurryCall(body)
          Expression.Lambda(args, b, tpe, loc)

        case Expression.Apply(Expression.Apply(Expression.Def(sym0, tpe0, loc0), args1, tpe1, loc1), args2, tpe2, loc2) =>
          names.get(sym0) match {
            case Some(sym0Uncurried) =>
              val def0 = defs(sym0Uncurried)
              val as1 = args1.map(uncurryCall)
              val as2 = args2.map(uncurryCall)
              Expression.Apply(Expression.Def(sym0Uncurried, def0.tpe, loc1), as1 ++ as2, tpe2, loc2)
            case None =>
              Expression.Apply(Expression.Apply(Expression.Def(sym0, tpe0, loc0), args1, tpe1, loc1), args2, tpe2, loc2)
          }

        case Expression.Apply(exp, args, tpe, loc) =>
          val e = uncurryCall(exp)
          val as = args.map(uncurryCall)
          val app = Expression.Apply(e, as, tpe, loc)
          app match {
            // Check to see if we can uncurry further
            case Expression.Apply(Expression.Apply(Expression.Def(sym0, tpe0, loc0), args1, tpe1, loc1), args2, tpe2, loc2) => uncurryCall(app)
            case _ => app
          }

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = uncurryCall(exp1)
          val consequent = uncurryCall(exp2)
          val alternative = uncurryCall(exp3)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = uncurryCall(exp)
          val bs = branches map {
            case (sym, br) =>
              val b = uncurryCall(br)
              (sym -> b)
          }
          Expression.Branch(e, bs, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) => Expression.JumpTo(sym, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.Let(sym, e1, e2, tpe, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.LetRec(sym, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) =>
          val e = uncurryCall(exp)
          Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) =>
          val b = uncurryCall(base)
          Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms.map(uncurryCall)
          Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) => Expression.RecordEmpty(tpe, loc)

        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = uncurryCall(value)
          val r = uncurryCall(rest)
          Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = uncurryCall(rest)
          Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms.map(uncurryCall)
          Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = uncurryCall(elm)
          val ln = uncurryCall(len)
          Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = uncurryCall(base)
          val i = uncurryCall(index)
          Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = uncurryCall(base)
          val i = uncurryCall(index)
          val e = uncurryCall(elm)
          Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) =>
          val b = uncurryCall(base)
          Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = uncurryCall(base)
          val i1 = uncurryCall(startIndex)
          val i2 = uncurryCall(endIndex)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.Assign(e1, e2, tpe, loc)

        case Expression.Existential(fparam, exp, loc) =>
          val e = uncurryCall(exp)
          Expression.Existential(fparam, e, loc)

        case Expression.Universal(fparam, exp, loc) =>
          val e = uncurryCall(exp)
          Expression.Universal(fparam, e, loc)

        case Expression.Cast(exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.Cast(e, tpe, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = uncurryCall(exp)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val b = uncurryCall(body)
              CatchRule(sym, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          val as = args.map(uncurryCall)
          Expression.InvokeConstructor(constructor, as, tpe, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          val e = uncurryCall(exp)
          val as = args.map(uncurryCall)
          Expression.InvokeMethod(method, e, as, tpe, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          val as = args.map(uncurryCall)
          Expression.InvokeStaticMethod(method, as, tpe, loc)

        case Expression.GetField(field, exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.GetField(field, e, tpe, loc)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.PutField(field, e1, e2, tpe, loc)

        case Expression.GetStaticField(field, tpe, loc) => Expression.GetStaticField(field, tpe, loc)

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.PutStaticField(field, e, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val c = uncurryCall(chan)
              val e = uncurryCall(exp)
              SelectChannelRule(sym, c, e)
          }
          val d = default.map(uncurryCall)
          Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) => Expression.ProcessPanic(msg, tpe, loc)

        case Expression.FixpointConstraintSet(cs0, tpe, loc) =>
          val cs1 = cs0.map(visitConstraint)
          Expression.FixpointConstraintSet(cs1, tpe, loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = uncurryCall(exp)
          Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = uncurryCall(exp1)
          val e2 = uncurryCall(exp2)
          val e3 = uncurryCall(exp3)
          Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)
        case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }

    private def visitConstraint(cs: Constraint): Constraint = {
      val Constraint(cparams, head, body, loc) = cs
      val h = visitHeadPredicate(head)
      val bs = body.map(visitBodyPredicate)
      Constraint(cparams, h, bs, loc)
    }

    private def visitHeadPredicate(head0: Predicate.Head): Predicate.Head = head0 match {
      case Predicate.Head.Atom(name, den, terms, tpe, loc) =>
        val t = terms.map(visitHeadTerm)
        Predicate.Head.Atom(name, den, t, tpe, loc)
      case Predicate.Head.Union(exp, tpe, loc) =>
        val e = uncurryCall(exp)
        Predicate.Head.Union(e, tpe, loc)
    }

    private def visitBodyPredicate(body0: Predicate.Body): Predicate.Body = body0 match {
      case Predicate.Body.Atom(name, den, polarity, terms, tpe, loc) =>
        val t = terms.map(visitBodyTerm)
        Predicate.Body.Atom(name, den, polarity, t, tpe, loc)
      case Predicate.Body.Guard(exp, loc) =>
        val e = uncurryCall(exp)
        Predicate.Body.Guard(e, loc)
    }

    private def visitHeadTerm(term0: Term.Head): Term.Head = term0 match {
      case Term.Head.QuantVar(sym, tpe, loc) => term0
      case Term.Head.CapturedVar(sym, tpe, loc) => term0
      case Term.Head.Lit(lit, tpe, loc) =>
        val l = uncurryCall(lit)
        Term.Head.Lit(l, tpe, loc)
      case Term.Head.App(exp, args, tpe, loc) =>
        val e = uncurryCall(exp)
        Term.Head.App(e, args, tpe, loc)
    }

    private def visitBodyTerm(term0: Term.Body): Term.Body = term0 match {
      case Term.Body.Wild(tpe, loc) => term0
      case Term.Body.QuantVar(sym, tpe, loc) => term0
      case Term.Body.CapturedVar(sym, tpe, loc) => term0
      case Term.Body.Lit(exp, tpe, loc) =>
        val e = uncurryCall(exp)
        Term.Body.Lit(e, tpe, loc)
    }
  }

  /**
   * Transform `defn0` into a series of definitions.
   * Each definition in the series uncurries the previous definition one step further.
   * The process stops when the body of the definition is no longer a lambda.
   * @param ctxt0 A set of definitions and a mapping from a definition to its successor in the above series.
   * @return `ctxt0` updated with the result of uncurrying `defn0`.
   */
  private def uncurryDefn(defn0: Def, ctxt0: UncurryContext)(implicit flix: Flix): UncurryContext =
    // Flix assumes benchmarks and tests have a certain signature, so we leave those alone.
    if (defn0.ann.isBenchmark || defn0.ann.isTest) {
      ctxt0
    } else {
      defn0.exp match {
        case Expression.Lambda(fparams, body, tpe, loc) =>
          val sym1 = Symbol.freshDefnSym(defn0.sym)
          val params0 = defn0.fparams ++ fparams
          val params1 = params0.map(fparam => fparam.copy(sym = Symbol.freshVarSym(fparam.sym)))
          val vars0 = params0.zip(params1).map {
            case (oldParam, newParam) => oldParam.sym -> newParam.sym
          }
          val exp1 = SimplifiedAstOps.renameBoundVars(body, vars0.toMap)
          val tpe1 = uncurryArrow(defn0.tpe)
          val result = defn0.copy(sym = sym1, fparams = params1, exp = exp1, tpe = tpe1)
          val defs1 = ctxt0.defs + (sym1 -> result)
          val names1 = ctxt0.names + (defn0.sym -> sym1)
          val ctxt1 = UncurryContext(defs1, names1)
          uncurryDefn(result, ctxt1)

        case _ => ctxt0
      }
    }

  /** Uncurry an arrow type A -> (B -> C) into an arrow (A, B) -> C
   * where A may be multiple types and A, B, C are all proper types.
   * Example: (Int32 -> Option[Int32]) -> Int32 -> Option[Int32]
   * uncurries into: (Int32 -> Option[Int32], Int32) -> Option[Int32]
   */
  private def uncurryArrow(tpe0: Type): Type = tpe0 match {
    case Type.Apply(lhs0, Type.Apply(Type.Apply(Type.Arrow(2, eff1), b), c)) => //TODO: arity could be more than 2
      def visitLhs(lhs0: Type): Type = lhs0 match {
        case Type.Arrow(arity0, eff0) =>
          val eff2 = eff1 match {
            case Type.Impure => Type.Impure
            case Type.Pure => eff0
            case Type.Unit => Type.Unit //TODO: What does this mean?
            case Type.Var(id, kind, rigidity) => Type.Var(id, kind, rigidity) //TODO: What does this mean?
            case _ => throw InternalCompilerException(s"Unexpected effect: '${eff1.getClass}'.")
          }
          Type.Arrow(arity0 + 1, eff2)

        case Type.Apply(lhs0, rhs0) =>
          val lhs1 = visitLhs(lhs0)
          Type.Apply(lhs1, rhs0)

        case _ => throw InternalCompilerException(s"Unexpected type: '${lhs0.getClass}'.")
      }

      val lhs1 = visitLhs(lhs0)
      val lhs2 = Type.Apply(lhs1, b)
      Type.Apply(lhs2, c)

    case _ => throw InternalCompilerException(s"Unexpected type: '${tpe0.getClass}'.")
  }
}
