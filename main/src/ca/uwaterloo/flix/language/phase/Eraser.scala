package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ErasedAst.JType
import ca.uwaterloo.flix.language.ast.ErasedAst.JType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, FinalAst}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Eraser extends Phase[FinalAst.Root, FinalAst.Root]  {

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[FinalAst.Root, CompilationError] = flix.phase("Eraser") {
    /**
     * Translates the given `constraint0` to the ErasedAst.
     */
    def visitConstraint(constraint0: FinalAst.Constraint): ErasedAst.Constraint = {
      val cparams = constraint0.cparams.map {
        case FinalAst.ConstraintParam.HeadParam(sym, tpe, loc) => ErasedAst.ConstraintParam.HeadParam(sym, tpe, loc)
        case FinalAst.ConstraintParam.RuleParam(sym, tpe, loc) => ErasedAst.ConstraintParam.RuleParam(sym, tpe, loc)
      }
      val head = visitHeadPred(constraint0.head)
      val body = constraint0.body.map(visitBodyPred)

      ErasedAst.Constraint(cparams, head, body, constraint0.loc)
    }

    /**
     * Translates the given definition `def0` to the ErasedAst.
     */
    def visitDef(def0: FinalAst.Def): ErasedAst.Def = {
      val fs = def0.formals.map(visitFormalParam)
      // TODO: what is JType here?
      val exp = visitExp[JType](def0.exp)
      ErasedAst.Def(def0.ann, def0.mod, def0.sym, fs, exp, def0.tpe, def0.loc)
    }

    def castExp[T <: JType](exp: ErasedAst.Expression[JType]): ErasedAst.Expression[T] =
      exp.asInstanceOf[ErasedAst.Expression[T]]

    /**
     * Translates the given expression `exp0` to the ErasedAst.
     * TODO: Formal like Simplifier.scala
     */
    def visitExp[T <: JType](exp0: FinalAst.Expression): ErasedAst.Expression[T] = exp0 match {
      case FinalAst.Expression.Unit(loc) => castExp(ErasedAst.Expression.Unit(loc))
      case FinalAst.Expression.Null(tpe, loc) => castExp(ErasedAst.Expression.Null(tpe, loc))
      case FinalAst.Expression.True(loc) => castExp(ErasedAst.Expression.True(loc))
      case FinalAst.Expression.False(loc) => castExp(ErasedAst.Expression.False(loc))
      case FinalAst.Expression.Char(lit, loc) => castExp(ErasedAst.Expression.Char(lit, loc))
      case FinalAst.Expression.Float32(lit, loc) => castExp(ErasedAst.Expression.Float32(lit, loc))
      case FinalAst.Expression.Float64(lit, loc) => castExp(ErasedAst.Expression.Float64(lit, loc))
      case FinalAst.Expression.Int8(lit, loc) => castExp(ErasedAst.Expression.Int8(lit, loc))
      case FinalAst.Expression.Int16(lit, loc) => castExp(ErasedAst.Expression.Int16(lit, loc))
      case FinalAst.Expression.Int32(lit, loc) => castExp(ErasedAst.Expression.Int32(lit, loc))
      case FinalAst.Expression.Int64(lit, loc) => castExp(ErasedAst.Expression.Int64(lit, loc))
      case FinalAst.Expression.BigInt(lit, loc) => castExp(ErasedAst.Expression.BigInt(lit, loc))
      case FinalAst.Expression.Str(lit, loc) => castExp(ErasedAst.Expression.Str(lit, loc))
      case FinalAst.Expression.Var(sym, tpe, loc) => ErasedAst.Expression.Var(sym, tpe, loc)
      case FinalAst.Expression.Closure(sym, freeVars, _, tpe, loc) =>
        val newFreeVars = freeVars.map { case FinalAst.FreeVar(sym, tpe) => ErasedAst.FreeVar(sym, tpe) }
        castExp(ErasedAst.Expression.Closure(sym, newFreeVars, tpe, loc))
      case FinalAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        castExp(ErasedAst.Expression.ApplyClo(visitExp(exp), args.map(visitExp), tpe, loc))
      case FinalAst.Expression.ApplyDef(sym, args, tpe, loc) =>
        castExp(ErasedAst.Expression.ApplyDef(sym, args.map(visitExp), tpe, loc))
      case FinalAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
        castExp(ErasedAst.Expression.ApplyCloTail(visitExp(exp), args.map(visitExp), tpe, loc))
      case FinalAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
        castExp(ErasedAst.Expression.ApplyDefTail(sym, args.map(visitExp), tpe, loc))
      case FinalAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        val newFormals = formals.map { case FinalAst.FormalParam(sym, tpe) => ErasedAst.FormalParam(sym, tpe) }
        castExp(ErasedAst.Expression.ApplySelfTail(sym, newFormals, actuals.map(visitExp), tpe, loc))
      case FinalAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        castExp(ErasedAst.Expression.Unary(sop, op, visitExp(exp), tpe, loc))
      case FinalAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        castExp(ErasedAst.Expression.Binary(sop, op, visitExp(exp1), visitExp(exp2), tpe, loc))
      case FinalAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        ErasedAst.Expression.IfThenElse(visitExp(exp1), visitExp[T](exp2), visitExp[T](exp3), tpe, loc)
      case FinalAst.Expression.Branch(exp, branches, tpe, loc) =>
        val newBranches = branches.map { case (label, branchExp) => (label, visitExp(branchExp)) }
        ErasedAst.Expression.Branch(visitExp(exp), newBranches, tpe, loc)
      case FinalAst.Expression.JumpTo(sym, tpe, loc) =>
        ErasedAst.Expression.JumpTo(sym, tpe, loc)
      case FinalAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        ErasedAst.Expression.Let(sym, visitExp(exp1), visitExp(exp2), tpe, loc)
      case FinalAst.Expression.Is(sym, tag, exp, loc) =>
        castExp(ErasedAst.Expression.Is(sym, tag, visitExp(exp), loc))
      case FinalAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
        castExp(ErasedAst.Expression.Tag(sym, tag, visitExp(exp), tpe, loc))
      case FinalAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        ErasedAst.Expression.Untag(sym, tag, visitExp(exp), tpe, loc)
      case FinalAst.Expression.Index(base, offset, tpe, loc) =>
        val e = ErasedAst.Expression.Index(visitExp[JObject](base), offset, tpe, loc)
        ErasedAst.Expression.Cast(e, tpe, loc)
      case FinalAst.Expression.Tuple(elms, tpe, loc) =>
        castExp(ErasedAst.Expression.Tuple(elms.map(visitExp), tpe, loc))
      case FinalAst.Expression.RecordEmpty(tpe, loc) =>
        castExp(ErasedAst.Expression.RecordEmpty(tpe, loc))
      case FinalAst.Expression.RecordSelect(exp, field, tpe, loc) =>
        ErasedAst.Expression.RecordSelect(visitExp(exp), field, tpe, loc)
      case FinalAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
        castExp(ErasedAst.Expression.RecordExtend(field, visitExp(value), visitExp(rest), tpe, loc))
      case FinalAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
        castExp(ErasedAst.Expression.RecordRestrict(field, visitExp(rest), tpe, loc))
      case FinalAst.Expression.ArrayLit(elms, tpe, loc) =>
        castExp(ErasedAst.Expression.ArrayLit(elms.map(visitExp), tpe, loc))
      case FinalAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        castExp(ErasedAst.Expression.ArrayNew(visitExp(elm), visitExp(len), tpe, loc))
      case FinalAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        ErasedAst.Expression.ArrayLoad(visitExp(base), visitExp(index), tpe, loc)
      case FinalAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        castExp(ErasedAst.Expression.ArrayStore(visitExp(base), visitExp(index), visitExp(elm), tpe, loc))
      case FinalAst.Expression.ArrayLength(base, tpe, loc) =>
        castExp(ErasedAst.Expression.ArrayLength(visitExp(base), tpe, loc))
      case FinalAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        castExp(ErasedAst.Expression.ArraySlice(visitExp(base), visitExp(beginIndex), visitExp(endIndex), tpe, loc))
      case FinalAst.Expression.Ref(exp, tpe, loc) =>
        castExp(ErasedAst.Expression.Ref(visitExp(exp), tpe, loc))
      case FinalAst.Expression.Deref(exp, tpe, loc) =>
        ErasedAst.Expression.Deref(visitExp(exp), tpe, loc)
      case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        castExp(ErasedAst.Expression.Assign(visitExp(exp1), visitExp(exp2), tpe, loc))
      case FinalAst.Expression.Existential(fparam, exp, loc) =>
        val FinalAst.FormalParam(sym, tpe) = fparam
        castExp(ErasedAst.Expression.Existential(ErasedAst.FormalParam(sym, tpe), visitExp(exp), loc))
      case FinalAst.Expression.Universal(fparam, exp, loc) =>
        val FinalAst.FormalParam(sym, tpe) = fparam
        castExp(ErasedAst.Expression.Universal(ErasedAst.FormalParam(sym, tpe), visitExp(exp), loc))
      case FinalAst.Expression.Cast(exp, tpe, loc) =>
        ErasedAst.Expression.Cast(visitExp(exp), tpe, loc)
      case FinalAst.Expression.TryCatch(exp, rules, tpe, loc) =>
        val newRules = rules.map{ case FinalAst.CatchRule(sym, clazz, exp) =>
          ErasedAst.CatchRule[T](sym, clazz, visitExp(exp))}
        ErasedAst.Expression.TryCatch(visitExp(exp), newRules, tpe, loc)
      case FinalAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        castExp(ErasedAst.Expression.InvokeConstructor(constructor, args.map(visitExp), tpe, loc))
      case FinalAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        ErasedAst.Expression.InvokeMethod(method, visitExp(exp), args.map(visitExp), tpe, loc)
      case FinalAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        ErasedAst.Expression.InvokeStaticMethod(method, args.map(visitExp), tpe, loc)
      case FinalAst.Expression.GetField(field, exp, tpe, loc) =>
        ErasedAst.Expression.GetField(field, visitExp(exp), tpe, loc)
      case FinalAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
        castExp(ErasedAst.Expression.PutField(field, visitExp(exp1), visitExp(exp2), tpe, loc))
      case FinalAst.Expression.GetStaticField(field, tpe, loc) =>
        ErasedAst.Expression.GetStaticField(field, tpe, loc)
      case FinalAst.Expression.PutStaticField(field, exp, tpe, loc) =>
        castExp(ErasedAst.Expression.PutStaticField(field, visitExp(exp), tpe, loc))
      case FinalAst.Expression.NewChannel(exp, tpe, loc) =>
        castExp(ErasedAst.Expression.NewChannel(visitExp(exp), tpe, loc))
      case FinalAst.Expression.GetChannel(exp, tpe, loc) =>
        ErasedAst.Expression.GetChannel(visitExp(exp), tpe, loc)
      case FinalAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        castExp(ErasedAst.Expression.PutChannel(visitExp(exp1), visitExp(exp2), tpe, loc))
      case FinalAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val newRules = rules.map { case FinalAst.SelectChannelRule(sym, chan, exp) =>
          ErasedAst.SelectChannelRule[T](sym, visitExp(chan), visitExp(exp)) }
        ErasedAst.Expression.SelectChannel(newRules, default.map(visitExp[T]), tpe, loc)
      case FinalAst.Expression.Spawn(exp, tpe, loc) =>
        castExp(ErasedAst.Expression.Spawn(visitExp(exp), tpe, loc))
      case FinalAst.Expression.Lazy(exp, tpe, loc) =>
        castExp(ErasedAst.Expression.Lazy(visitExp(exp), tpe, loc))
      case FinalAst.Expression.Force(exp, tpe, loc) =>
        ErasedAst.Expression.Force(visitExp(exp), tpe, loc)
      case FinalAst.Expression.FixpointConstraintSet(cs, tpe, loc) =>
        val newCs = cs.map(visitConstraint)
        castExp(ErasedAst.Expression.FixpointConstraintSet(newCs, tpe, loc))
      case FinalAst.Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        castExp(ErasedAst.Expression.FixpointCompose(visitExp(exp1), visitExp(exp2), tpe, loc))
      case FinalAst.Expression.FixpointSolve(exp, stf, tpe, loc) =>
        castExp(ErasedAst.Expression.FixpointSolve(visitExp(exp), stf, tpe, loc))
      case FinalAst.Expression.FixpointProject(pred, exp, tpe, loc) =>
        castExp(ErasedAst.Expression.FixpointProject(pred, visitExp(exp), tpe, loc))
      case FinalAst.Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        castExp(ErasedAst.Expression.FixpointEntails(visitExp(exp1), visitExp(exp2), tpe, loc))
      case FinalAst.Expression.FixpointFold(pred, init, f, constraints, tpe, loc) =>
        def visitVar[TT <: JType](v: FinalAst.Expression.Var): ErasedAst.Expression.Var[TT] =
          visitExp(v).asInstanceOf[ErasedAst.Expression.Var[TT]]

        ErasedAst.Expression.FixpointFold(pred, visitVar(init), visitVar(f), visitVar(constraints), tpe, loc)
      case FinalAst.Expression.HoleError(sym, tpe, loc) =>
        ErasedAst.Expression.HoleError(sym, tpe, loc)
      case FinalAst.Expression.MatchError(tpe, loc) =>
        ErasedAst.Expression.MatchError(tpe, loc)
    }

    /**
     * Translates the given `head` predicate to the ErasedAst.
     */
    def visitHeadPred(head: FinalAst.Predicate.Head): ErasedAst.Predicate.Head = head match {
      case FinalAst.Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
        ErasedAst.Predicate.Head.Atom(pred, den, terms.map(visitTermHead), tpe, loc)

      case FinalAst.Predicate.Head.Union(exp, terms, tpe, loc) =>
        ErasedAst.Predicate.Head.Union(visitExp(exp), terms.map(visitTermHead), tpe, loc)
    }

    /**
     * Translates the given `body` predicate to the ErasedAst.
     */
    def visitBodyPred(body: FinalAst.Predicate.Body): ErasedAst.Predicate.Body = body match {
      case FinalAst.Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) =>
        ErasedAst.Predicate.Body.Atom(pred, den, polarity, terms.map(visitTermBody), tpe, loc)

      case FinalAst.Predicate.Body.Guard(exp, terms, loc) =>
        ErasedAst.Predicate.Body.Guard(visitExp(exp), terms.map(visitTermBody), loc)
    }

    /**
     * Translates the given 'head' term to the ErasedAst.
     */
    def visitTermHead(head: FinalAst.Term.Head): ErasedAst.Term.Head = head match {
      case FinalAst.Term.Head.QuantVar(sym, tpe, loc) =>
        ErasedAst.Term.Head.QuantVar(sym, tpe, loc)
      case FinalAst.Term.Head.CapturedVar(sym, tpe, loc) =>
        ErasedAst.Term.Head.CapturedVar(sym, tpe, loc)
      case FinalAst.Term.Head.Lit(sym, tpe, loc) =>
        ErasedAst.Term.Head.Lit(sym, tpe, loc)
      case FinalAst.Term.Head.App(exp, args, tpe, loc) =>
        ErasedAst.Term.Head.App(visitExp(exp), args, tpe, loc)
    }

    /**
     * Translates the given `body` term to the ErasedAst.
     */
    def visitTermBody(body: FinalAst.Term.Body): ErasedAst.Term.Body = body match {
      case FinalAst.Term.Body.Wild(tpe, loc) =>
        ErasedAst.Term.Body.Wild(tpe, loc)
      case FinalAst.Term.Body.QuantVar(sym, tpe, loc) =>
        ErasedAst.Term.Body.QuantVar(sym, tpe, loc)
      case FinalAst.Term.Body.CapturedVar(sym, tpe, loc) =>
        ErasedAst.Term.Body.CapturedVar(sym, tpe, loc)
      case FinalAst.Term.Body.Lit(sym, tpe, loc) =>
        ErasedAst.Term.Body.Lit(sym, tpe, loc)
    }

    /**
     * Translates the given `lattice0` to the ErasedAst.
     */
    def visitLatticeOps(lattice0: FinalAst.LatticeOps): ErasedAst.LatticeOps = lattice0 match {
      case FinalAst.LatticeOps(tpe, bot, equ, leq, lub, glb) =>
        ErasedAst.LatticeOps(tpe, bot, equ, leq, lub, glb)
    }

    /**
     * Translates the given attribute `a` to the ErasedAst.
     */
    def visitAttribute(a: FinalAst.Attribute): ErasedAst.Attribute =
      ErasedAst.Attribute(a.name, a.tpe)

    /**
     * Translates the given formal param `p` to the ErasedAst.
     */
    def visitFormalParam(p: FinalAst.FormalParam): ErasedAst.FormalParam =
      ErasedAst.FormalParam(p.sym, p.tpe)

    /**
     * Translates the property `p` to the ErasedAst.
     */
    def visitProperty(p: FinalAst.Property): ErasedAst.Property =
      ErasedAst.Property(p.law, p.defn, visitExp(p.exp))

    //
    // Main computation.
    //
    val defns = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map {
      case (k, FinalAst.Enum(mod, sym, cases0, tpeDeprecated, loc)) =>
        val cases = cases0 map {
          case (tag, FinalAst.Case(enumSym, tagName, tagTpeDeprecated, tagLoc)) => tag -> ErasedAst.Case(enumSym, tagName, tagTpeDeprecated, tagLoc)
        }
        k -> ErasedAst.Enum(mod, sym, cases, loc)
    }
    val latticeOps = root.latticeOps.map { case (k, v) => k -> visitLatticeOps(v) }
    val properties = root.properties.map { p => visitProperty(p) }
    val specialOps = root.specialOps
    val reachable = root.reachable

    val actualTransformation = ErasedAst.Root(defns, enums, latticeOps, properties, specialOps, reachable, root.sources).toSuccess
    root.toSuccess
  }
}
