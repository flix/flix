package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.{ChangeSet, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.TerminationError
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The Terminator phase verifies that functions annotated with `@Terminates` are structurally
  * recursive — meaning every recursive call is made on a strict substructure of a formal parameter.
  *
  * It also checks:
  * - Strict positivity of data types used for structural recursion.
  * - Absence of features that could break the termination guarantee.
  */
object Terminator {

  /**
    * Checks termination properties of `root`.
    */
  def run(root: Root, oldRoot: Root, changeSet: ChangeSet)(implicit flix: Flix): (Root, List[TerminationError]) =
    flix.phaseNew("Terminator") {
      implicit val sctx: SharedContext = SharedContext.mk()
      implicit val _r: Root = root
      val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(
        ParOps.parMapValues(_)(visitDef))
      (root.copy(defs = defs), sctx.errors.asScala.toList)
    }

  /**
    * Represents the relationship of a variable to a formal parameter.
    *
    * @param rootParam the formal parameter this variable is related to.
    * @param isStrict  true if the variable is a strict substructure of the parameter.
    */
  private case class ParamRelation(rootParam: Symbol.VarSym, isStrict: Boolean)

  /**
    * A map from variable symbols to their structural relationship with formal parameters.
    */
  private type SubEnv = Map[Symbol.VarSym, ParamRelation]

  /** Checks a definition for termination properties if annotated with @Terminates. */
  private def visitDef(defn: Def)(implicit sctx: SharedContext, root: Root, flix: Flix): Def = {
    if (defn.spec.ann.isTerminates) {
      checkStrictPositivity(defn)
      val fparams = defn.spec.fparams
      val initialEnv: SubEnv = fparams.map(fp => fp.bnd.sym -> ParamRelation(fp.bnd.sym, false)).toMap
      visitExp(defn.sym, fparams, initialEnv, defn.exp)
    }
    defn
  }

  // =========================================================================
  // Unified expression visitor — structural recursion + forbidden features
  // =========================================================================

  /**
    * Recursively visits `exp`, performing two checks simultaneously:
    *
    *   - **Forbidden features**: Every expression is checked regardless of recursion.
    *   - **Structural recursion**: Self-recursive calls are verified to pass a strict
    *     substructure of a formal parameter, and the substructure environment `env` is
    *     threaded through pattern matches and let-bindings.
    *
    * @param defnSym the symbol of the enclosing @Terminates function.
    * @param fparams the formal parameters of the function.
    * @param env     the current substructure environment.
    * @param exp     the expression to check.
    */
  private def visitExp(defnSym: Symbol.DefnSym, fparams: List[FormalParam], env: SubEnv, exp: Expr)(implicit sctx: SharedContext): Unit = {

    def visit(e: Expr): Unit = visitExp(defnSym, fparams, env, e)

    exp match {
      // --- Self-recursive call (structural recursion check) ---
      case Expr.ApplyDef(symUse, exps, _, _, _, _, loc) if symUse.sym == defnSym =>
        val hasDecreasingArg = exps.zip(fparams).exists {
          case (Expr.Var(sym, _, _), fp) =>
            env.get(sym) match {
              case Some(ParamRelation(rootParam, true)) => rootParam == fp.bnd.sym
              case _ => false
            }
          case _ => false
        }
        if (!hasDecreasingArg) {
          sctx.errors.add(TerminationError.NonStructuralRecursion(defnSym, loc))
        }
        exps.foreach(visit)

      // --- Match: extend env when scrutinee is a tracked variable ---
      case Expr.Match(scrutinee, rules, _, _, _) =>
        visit(scrutinee)
        val scrutineeRelation: Option[ParamRelation] = scrutinee match {
          case Expr.Var(sym, _, _) => env.get(sym)
          case _ => None
        }
        rules.foreach {
          case MatchRule(pat, guard, body, _) =>
            val extEnv = scrutineeRelation match {
              case Some(rel) => extendEnvFromPattern(env, pat, rel.rootParam)
              case None => env
            }
            guard.foreach(visitExp(defnSym, fparams, extEnv, _))
            visitExp(defnSym, fparams, extEnv, body)
        }

      // --- Let: propagate alias when RHS is a tracked variable ---
      case Expr.Let(bnd, exp1, exp2, _, _, _) =>
        visit(exp1)
        val extEnv = exp1 match {
          case Expr.Var(sym, _, _) => env.get(sym).map(rel => env + (bnd.sym -> rel)).getOrElse(env)
          case _ => env
        }
        visitExp(defnSym, fparams, extEnv, exp2)

      // --- LocalDef: don't propagate sub-env into local def body ---
      case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
        visit(exp1); visit(exp2)

      // =====================================================================
      // Forbidden expressions — report and recurse into children
      // =====================================================================

      case Expr.ArrayNew(e1, e2, e3, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "mutable array operation", loc))
        visit(e1); visit(e2); visit(e3)

      case Expr.ArrayStore(e1, e2, e3, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "mutable array operation", loc))
        visit(e1); visit(e2); visit(e3)

      case Expr.StructPut(e1, _, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "mutable struct operation", loc))
        visit(e1); visit(e2)

      case Expr.InvokeConstructor(_, exps, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java interop", loc))
        exps.foreach(visit)

      case Expr.InvokeMethod(_, e, exps, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java interop", loc))
        visit(e); exps.foreach(visit)

      case Expr.InvokeStaticMethod(_, exps, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java interop", loc))
        exps.foreach(visit)

      case Expr.GetField(_, e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java field access", loc))
        visit(e)

      case Expr.PutField(_, e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java field access", loc))
        visit(e1); visit(e2)

      case Expr.GetStaticField(_, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java field access", loc))

      case Expr.PutStaticField(_, e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java field access", loc))
        visit(e)

      case Expr.NewObject(_, _, _, _, methods, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "Java object creation", loc))
        methods.foreach(m => visit(m.exp))

      case Expr.UncheckedCast(e, _, _, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "unchecked cast", loc))
        visit(e)

      case Expr.Unsafe(e, _, _, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "unsafe block", loc))
        visit(e)

      case Expr.NewChannel(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "channel operation", loc))
        visit(e)

      case Expr.GetChannel(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "channel operation", loc))
        visit(e)

      case Expr.PutChannel(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "channel operation", loc))
        visit(e1); visit(e2)

      case Expr.SelectChannel(rules, default, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "channel operation", loc))
        rules.foreach(r => { visit(r.chan); visit(r.exp) })
        default.foreach(visit)

      case Expr.Spawn(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "concurrency", loc))
        visit(e1); visit(e2)

      case Expr.ParYield(frags, e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "concurrency", loc))
        frags.foreach(f => visit(f.exp)); visit(e)

      case Expr.FixpointSolveWithProject(exps, _, _, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "fixpoint operation", loc))
        exps.foreach(visit)

      case Expr.FixpointMerge(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "fixpoint operation", loc))
        visit(e1); visit(e2)

      case Expr.Throw(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "throw expression", loc))
        visit(e)

      case Expr.Force(e, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "lazy force", loc))
        visit(e)

      case Expr.ApplyClo(e1, e2, _, _, loc) =>
        sctx.errors.add(TerminationError.ForbiddenExpression(defnSym, "closure application", loc))
        visit(e1); visit(e2)

      // =====================================================================
      // Allowed expressions — just recurse into children
      // =====================================================================

      case Expr.Cst(_, _, _) => ()
      case Expr.Var(_, _, _) => ()
      case Expr.Hole(_, _, _, _, _) => ()
      case Expr.HoleWithExp(e, _, _, _, _) => visit(e)
      case Expr.OpenAs(_, e, _, _) => visit(e)
      case Expr.Use(_, _, e, _) => visit(e)
      case Expr.Lambda(_, e, _, _) => visit(e)
      case Expr.ApplyDef(_, exps, _, _, _, _, _) => exps.foreach(visit)
      case Expr.ApplyLocalDef(_, exps, _, _, _, _) => exps.foreach(visit)
      case Expr.ApplyOp(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.ApplySig(_, exps, _, _, _, _, _, _) => exps.foreach(visit)
      case Expr.Unary(_, e, _, _, _) => visit(e)
      case Expr.Binary(_, e1, e2, _, _, _) => visit(e1); visit(e2)
      case Expr.Region(_, _, e, _, _, _) => visit(e)
      case Expr.IfThenElse(e1, e2, e3, _, _, _) => visit(e1); visit(e2); visit(e3)
      case Expr.Stm(e1, e2, _, _, _) => visit(e1); visit(e2)
      case Expr.Discard(e, _, _) => visit(e)
      case Expr.TypeMatch(e, rules, _, _, _) => visit(e); rules.foreach(r => visit(r.exp))
      case Expr.RestrictableChoose(_, e, rules, _, _, _) => visit(e); rules.foreach(r => visit(r.exp))
      case Expr.ExtMatch(e, rules, _, _, _) => visit(e); rules.foreach(r => visit(r.exp))
      case Expr.Tag(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.RestrictableTag(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.ExtTag(_, exps, _, _, _) => exps.foreach(visit)
      case Expr.Tuple(exps, _, _, _) => exps.foreach(visit)
      case Expr.RecordSelect(e, _, _, _, _) => visit(e)
      case Expr.RecordExtend(_, e1, e2, _, _, _) => visit(e1); visit(e2)
      case Expr.RecordRestrict(_, e, _, _, _) => visit(e)
      case Expr.ArrayLit(exps, e, _, _, _) => exps.foreach(visit); visit(e)
      case Expr.ArrayLoad(e1, e2, _, _, _) => visit(e1); visit(e2)
      case Expr.ArrayLength(e, _, _) => visit(e)
      case Expr.StructNew(_, fields, region, _, _, _) => fields.foreach(f => visit(f._2)); region.foreach(visit)
      case Expr.StructGet(e, _, _, _, _) => visit(e)
      case Expr.VectorLit(exps, _, _, _) => exps.foreach(visit)
      case Expr.VectorLoad(e1, e2, _, _, _) => visit(e1); visit(e2)
      case Expr.VectorLength(e, _) => visit(e)
      case Expr.Ascribe(e, _, _, _, _, _) => visit(e)
      case Expr.InstanceOf(e, _, _) => visit(e)
      case Expr.CheckedCast(_, e, _, _, _) => visit(e)
      case Expr.Without(e, _, _, _, _) => visit(e)
      case Expr.TryCatch(e, rules, _, _, _) => visit(e); rules.foreach(r => visit(r.exp))
      case Expr.Handler(_, rules, _, _, _, _, _) => rules.foreach(r => visit(r.exp))
      case Expr.RunWith(e1, e2, _, _, _) => visit(e1); visit(e2)
      case Expr.Lazy(e, _, _) => visit(e)
      case Expr.FixpointConstraintSet(_, _, _) => ()
      case Expr.FixpointLambda(_, e, _, _, _) => visit(e)
      case Expr.FixpointQueryWithProvenance(exps, _, _, _, _, _) => exps.foreach(visit)
      case Expr.FixpointQueryWithSelect(exps, q, sels, _, where, _, _, _, _) => exps.foreach(visit); visit(q); sels.foreach(visit); where.foreach(visit)
      case Expr.FixpointInjectInto(exps, _, _, _, _) => exps.foreach(visit)
      case Expr.Error(_, _, _) => ()
    }
  }

  // =========================================================================
  // Pattern environment helpers
  // =========================================================================

  /**
    * Extends the substructure environment based on a pattern match.
    *
    * Variables bound inside `Pattern.Tag` sub-patterns are strict substructures of the root parameter.
    * Variables bound by `Pattern.Var` at the top level are aliases (not strict substructures).
    */
  private def extendEnvFromPattern(env: SubEnv, pat: Pattern, rootParam: Symbol.VarSym): SubEnv = pat match {
    case Pattern.Tag(_, pats, _, _) =>
      pats.foldLeft(env)((acc, p) => collectStrictSubstructures(acc, p, rootParam))
    case Pattern.Tuple(pats, _, _) =>
      pats.toList.foldLeft(env)((acc, p) => extendEnvFromPattern(acc, p, rootParam))
    case Pattern.Var(bnd, _, _) =>
      env + (bnd.sym -> ParamRelation(rootParam, false))
    case _ => env
  }

  /**
    * Collects strict substructure bindings from a pattern.
    * All variables found inside a Tag pattern are strict substructures.
    */
  private def collectStrictSubstructures(env: SubEnv, pat: Pattern, rootParam: Symbol.VarSym): SubEnv = pat match {
    case Pattern.Var(bnd, _, _) =>
      env + (bnd.sym -> ParamRelation(rootParam, true))
    case Pattern.Tag(_, pats, _, _) =>
      pats.foldLeft(env)((acc, p) => collectStrictSubstructures(acc, p, rootParam))
    case Pattern.Tuple(pats, _, _) =>
      pats.toList.foldLeft(env)((acc, p) => collectStrictSubstructures(acc, p, rootParam))
    case Pattern.Wild(_, _) => env
    case Pattern.Cst(_, _, _) => env
    case Pattern.Record(pats, restPat, _, _) =>
      val env1 = pats.foldLeft(env)((acc, rp) => collectStrictSubstructures(acc, rp.pat, rootParam))
      collectStrictSubstructures(env1, restPat, rootParam)
    case Pattern.Error(_, _) => env
  }

  // =========================================================================
  // Strict Positivity Check
  // =========================================================================

  /**
    * Checks that the types of formal parameters are strictly positive.
    */
  private def checkStrictPositivity(defn: Def)(implicit sctx: SharedContext, root: Root): Unit = {
    for (fparam <- defn.spec.fparams) {
      val tpe = fparam.tpe
      tpe.typeConstructor match {
        case Some(TypeConstructor.Enum(enumSym, _)) =>
          root.enums.get(enumSym).foreach { enm =>
            if (!isStrictlyPositive(enumSym, enm)) {
              sctx.errors.add(TerminationError.NonStrictlyPositiveType(defn.sym, tpe, fparam.loc))
            }
          }
        case _ => // Not an enum type, nothing to check
      }
    }
  }

  /**
    * Returns true if the enum is strictly positive (i.e., its recursive type constructor
    * does not appear in a negative position in any case field).
    */
  private def isStrictlyPositive(enumSym: Symbol.EnumSym, enm: TypedAst.Enum): Boolean = {
    enm.cases.values.forall { caze =>
      caze.tpes.forall(checkPositivity(enumSym, _, positive = true))
    }
  }

  /**
    * Checks whether `enumSym` appears only in positive positions within `tpe`.
    */
  private def checkPositivity(enumSym: Symbol.EnumSym, tpe: Type, positive: Boolean): Boolean = {
    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) if sym == enumSym =>
        positive

      case Some(TypeConstructor.Arrow(_)) =>
        val argTypes = tpe.arrowArgTypes
        val resultType = tpe.arrowResultType
        argTypes.forall(checkPositivity(enumSym, _, !positive)) &&
          checkPositivity(enumSym, resultType, positive)

      case _ =>
        tpe.typeArguments.forall(checkPositivity(enumSym, _, positive))
    }
  }

  // =========================================================================
  // SharedContext
  // =========================================================================

  private object SharedContext {
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  private case class SharedContext(errors: ConcurrentLinkedQueue[TerminationError])

}
