package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.annotation.tailrec
import scala.collection.mutable


object EffectBinder {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("EffectBinder") {
    // Make sure effects happen on an empty op-stack and maintain lparams for defs
    val newDefs = ParOps.parMapValues(root.defs)(letBindEffectsDef)
    root.copy(defs = newDefs)
  }

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(): LocalContext = LocalContext(mutable.ArrayDeque.empty)
  }

  /**
    * A local non-shared context. Does not need to be thread-safe.
    * @param lparams the bound variales in the def.
    */
  private case class LocalContext(lparams: mutable.ArrayDeque[ReducedAst.LocalParam])

  /** [[binder]] is expected to be Let/LetRec/Scope and the body is ignored */
  private case class Binder(binder: ReducedAst.Expr)

  private type Bound[A] = (A, Chain[Binder], Boolean)

  private object Bound {
    def traverseReverse[A](bl: Bound[List[A]], f: A => Bound[ReducedAst.Expr])(implicit lctx: LocalContext, flix: Flix): Bound[List[ReducedAst.Expr]] = {
      val (l, binders, mustBind) = bl
      var bs = binders
      var mb = mustBind
      val acc: mutable.ArrayBuffer[ReducedAst.Expr] = mutable.ArrayBuffer.empty
      for (elem <- l.reverseIterator) {
        val (e, bs1, mb1) = f(elem)
        bs = bs1 ++ bs
        mb = mb || mb1
        if (mb) {
          val (b, ee) = letBindThing(e)
          bs = b ++ bs
          acc.append(ee)
        } else acc.append(e)
      }
      (acc.reverse.toList, bs, mb)
    }
  }

  private def letBindEffectsDef(defn: ReducedAst.Def)(implicit flix: Flix): ReducedAst.Def = {
    implicit val lctx: LocalContext = LocalContext.mk()
    val stmt = letBindEffectsStmt(defn.stmt)
    val lparams = defn.lparams ++ lctx.lparams.toList
    defn.copy(stmt = stmt, lparams = lparams)
  }

  private def letBindEffectsStmt(stmt: ReducedAst.Stmt)(implicit lctx: LocalContext, flix: Flix): ReducedAst.Stmt = stmt match {
    case Stmt.Ret(expr, tpe, loc) => Stmt.Ret(letBindEffectsTopLevel(expr)._1, tpe, loc)
  }

  private def letBindEffectsTopLevel(exp: ReducedAst.Expr)(implicit lctx: LocalContext, flix: Flix): (ReducedAst.Expr, Boolean) = exp match {
    // fancy
    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val (e, mustBind) = letBindEffectsTopLevel(exp)
      val (branches1, mustBinds) = branches.map {
        case (sym, branchExp) =>
          val (be, mustBind) = letBindEffectsTopLevel(branchExp)
          ((sym, be), mustBind)
      }.unzip
      val branch = Expr.Branch(e, branches1.toMap, tpe, purity, loc)
      (branch, mustBind || mustBinds.exists(identity))
    case Expr.JumpTo(sym, tpe, purity, loc) =>
      (Expr.JumpTo(sym, tpe, purity, loc), false)
    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val (e1, binders, mustBind1) = letBindEffects(_ => false, exp1)
      val (e2, mustBind2) = letBindEffectsTopLevel(exp2)
      val lett = Expr.Let(sym, e1, e2, tpe, purity, loc)
      (bindBinders(binders, lett), mustBind1 || mustBind2)
    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val (e1, binders, mustBind1) = letBindEffects(_ => false, exp1)
      val (e2, mustBind2) = letBindEffectsTopLevel(exp2)
      val letRec = Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
      (bindBinders(binders, letRec), mustBind1 || mustBind2)
    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val (e, mustBind) = letBindEffectsTopLevel(exp)
      val scope = Expr.Scope(sym, e, tpe, purity, loc)
      (scope, mustBind)
    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val (e, mustBind) = letBindEffectsTopLevel(exp)
      val (rules1, mustBinds) = rules.map(cr => {
        val (cre, mustBind) = letBindEffectsTopLevel(cr.exp)
        (ReducedAst.CatchRule(cr.sym, cr.clazz, cre), mustBind)
      }).unzip
      val tryCatch = Expr.TryCatch(e, rules1, tpe, purity, loc)
      (tryCatch, mustBind || mustBinds.exists(identity))
    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val (rules1, mustBinds) = rules.map(hr => {
        val (e, mustBind) = letBindEffectsTopLevel(hr.exp)
        (hr.copy(exp = e), mustBind)
      }).unzip
      val (e, mustBind) = letBindEffectsTopLevel(exp)
      val tryWith = Expr.TryWith(e, effUse, rules1, tpe, purity, loc)
      (tryWith, mustBind || mustBinds.exists(identity))
    // simple
    case _: Expr.Cst | _: Expr.Var | _: Expr.ApplyAtomic | _: Expr.ApplyClo | _: Expr.ApplyDef |
         _: Expr.ApplySelfTail | _: Expr.IfThenElse | _: Expr.Do | _: Expr.Resume |
         _: Expr.NewObject => {
      val (e, binders, mustBind) = letBindEffects(_ => false, exp)
      (bindBinders(binders, e), mustBind)
    }
  }

  /** binds the first binder as the outermost one */
  private def bindBinders(binders: Chain[Binder], e: ReducedAst.Expr): ReducedAst.Expr = {
    binders.foldRight(e) {
      case (Binder(Expr.Let(sym, binderExp, _, _, _, loc)), accExp) =>
        val lett = e => ReducedAst.Expr.Let(sym, e, accExp, accExp.tpe, accExp.purity.combineWith(binderExp.purity), loc)
        val (binders, res) = hoist(lett, binderExp, Chain.empty)
        bindBinders(binders, res)
      case (Binder(Expr.LetRec(varSym, i, defSym, binderExp, _, _, _, loc)), accExp) =>
        val letRec = e => ReducedAst.Expr.LetRec(varSym, i, defSym, e, accExp, accExp.tpe, accExp.purity.combineWith(binderExp.purity), loc)
        val (binders, res) = hoist(letRec, binderExp, Chain.empty)
        bindBinders(binders, res)
      case (Binder(_), _) => throw InternalCompilerException("unexpected Binder", SourceLocation.Unknown)
    }
  }

  /**
    * let x = {let y = z; q}; t
    * same as
    * let y  z; let x = q; t
    */
  @tailrec
  private def hoist(binder: ReducedAst.Expr => ReducedAst.Expr, binding: ReducedAst.Expr, acc: Chain[Binder]): (Chain[Binder], ReducedAst.Expr) = binding match {
    case Expr.Let(_, _, exp2, _, _, _) =>
      hoist(binder, exp2, acc ++ Chain(Binder(binding)))
    case Expr.LetRec(_, _, _, _, exp2, _, _, _) =>
      hoist(binder, exp2, acc ++ Chain(Binder(binding)))
    case other => (acc, binder(other))
  }

  /**
    * Invariant: All effectful applications and Do operations will happen with
    * an empty operand stack.
    *
    * first binder is the outermost one
    */
  private def letBindEffects(mustBindThis: Boolean => Boolean, exp: ReducedAst.Expr)(implicit lctx: LocalContext, flix: Flix): Bound[ReducedAst.Expr] = {
    val (e, binders, mustBind) = exp match {
      case ReducedAst.Expr.Cst(_, _, _) => (exp, Chain.empty, false)
      case ReducedAst.Expr.Var(_, _, _) => (exp, Chain.empty, false)
      case ReducedAst.Expr.ApplyAtomic(AtomicOp.Spawn, exps, tpe, purity, loc) =>
        val (exps1, binders, _) = Bound.traverseReverse((exps, Chain.empty, false), letBindEffectsAndTryBind)
        val aa1 = ReducedAst.Expr.ApplyAtomic(AtomicOp.Spawn, exps1, tpe, purity, loc)
        (aa1, binders, true)
      case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val (exps1, binders, mustBind) = Bound.traverseReverse((exps, Chain.empty, false), letBindEffectsAndTryBind)
        val aa1 = ReducedAst.Expr.ApplyAtomic(op, exps1, tpe, purity, loc)
        (aa1, binders, mustBind)
      case ReducedAst.Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
        val (exps1, binders2, mustBind2) = Bound.traverseReverse((exps, Chain.empty, false), letBindEffectsAndTryBind)
        val (e1, binders1, mustBind1) = letBindEffects(b => b || mustBind2, exp)
        val ac = ReducedAst.Expr.ApplyClo(e1, exps1, ct, tpe, purity, loc)
        val mb = purity == Purity.Impure
        (ac, binders1 ++ binders2, mb && mustBind1 && mustBind2)
      case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        val (exps1, binders, mustBind) = Bound.traverseReverse((exps, Chain.empty, false), letBindEffectsAndTryBind)
        val ac = ReducedAst.Expr.ApplyDef(sym, exps1, ct, tpe, purity, loc)
        val mb = purity == Purity.Impure
        (ac, binders, mb && mustBind)
      case ReducedAst.Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
        val (actuals1, binders, mustBind) = Bound.traverseReverse((actuals, Chain.empty, false), letBindEffectsAndTryBind)
        val mb = purity == Purity.Impure
        (Expr.ApplySelfTail(sym, formals, actuals1, tpe, purity, loc), binders, mustBind || mb)
      case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val (e3, mustBind3) = letBindEffectsTopLevel(exp3)
        val (e2, mustBind2) = letBindEffectsTopLevel(exp2)
        val (e1, binders, mustBind1) = letBindEffects(_ || mustBind2 || mustBind3, exp1)
        val ite = ReducedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)
        (ite, binders, mustBind1 || mustBind2 || mustBind3)
      case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val (e, mustBind) = letBindEffectsTopLevel(exp)
        val (branches1, mustBinds) = branches.map {
          case (sym, branchExp) =>
            val (be, mustBind) = letBindEffectsTopLevel(branchExp)
            ((sym, be), mustBind)
        }.unzip
        val branch = Expr.Branch(e, branches1.toMap, tpe, purity, loc)
        (branch, Chain.empty, mustBind || mustBinds.exists(identity))
      case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        (ReducedAst.Expr.JumpTo(sym, tpe, purity, loc), Chain.empty, false)
      case ReducedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
        //
        // [[let b = do E(); do E()]]
        // --------------
        // let tmp = do E();
        // let b = tmp;
        // do E()
        //
        val (e2, binders2, mustBind2) = letBindEffectsAndTryBind(exp2)
        val (e1, binders1, mustBind1) = letBindEffects(b => b | mustBind2, exp1)
        val mustBind = mustBind2 || mustBind1
        val bc = if (mustBind) Chain(Binder(Expr.Let(sym, e1, null, tpe, purity, loc))) else Chain()
        val e = if (mustBind) e2 else Expr.Let(sym, e1, e2, tpe, purity, loc)
        (e, binders1 ++ bc ++ binders2, mustBind)
      case ReducedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
        val (e2, binders2, mustBind2) = letBindEffectsAndTryBind(exp2)
        val (e1, binders1, mustBind1) = letBindEffectsAndTryBind(exp1)
        val mustBind = mustBind2 || mustBind1
        val bc = if (mustBind) Chain(Binder(Expr.LetRec(varSym, index, defSym, e1, null, tpe, purity, loc))) else Chain()
        val e = if (mustBind) e2 else Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
        (e, binders1 ++ bc ++ binders2, mustBind)
      case ReducedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
        val (e, mustBind) = letBindEffectsTopLevel(exp)
        (Expr.Scope(sym, e, tpe, purity, loc), Chain.empty, mustBind)
      case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val (rules1, mustBinds) = rules.map(cr => {
          val (e, mustBind) = letBindEffectsTopLevel(cr.exp)
          (ReducedAst.CatchRule(cr.sym, cr.clazz, e), mustBind)
        }).unzip
        val (e, mustBind) = letBindEffectsTopLevel(exp)
        val tryCatch = ReducedAst.Expr.TryCatch(e, rules1, tpe, purity, loc)
        (tryCatch, Chain.empty, mustBind || mustBinds.exists(identity))
      case ReducedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
        val (rules1, mustBinds) = rules.map(hr => {
          val (e, mustBind) = letBindEffectsTopLevel(hr.exp)
          (ReducedAst.HandlerRule(hr.op, hr.fparams, e), mustBind)
        }).unzip
        val (e, mustBind) = letBindEffectsTopLevel(exp)
        val tryWith = ReducedAst.Expr.TryWith(e, effUse, rules1, tpe, purity, loc)
        (tryWith, Chain.empty, mustBind || mustBinds.exists(identity))
      case ReducedAst.Expr.Do(op, exps, tpe, purity, loc) =>
        val (exps1, _, _) = Bound.traverseReverse((exps, Chain.empty, false), letBindEffectsAndTryBind)
        val exp1 = ReducedAst.Expr.Do(op, exps1, tpe, purity, loc)
        (exp1, Chain.empty, true)
      case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc) =>
        val (exps1, binders, mustBind) = Bound.traverseReverse((exps, Chain.empty, false), letBindEffectsAndTryBind)
        val newObject = ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, exps1, loc)
        (newObject, binders, mustBind)
      case ReducedAst.Expr.Resume(_, _, loc) => throw InternalCompilerException("Explicit resume not supported", loc)
    }
    // invariant: |binders| > 0 => mustBind
    assert(if (binders.nonEmpty) mustBind else true, exp.getClass.getName)
    val (e1, binders1) = bindIfTrue(e, mustBindThis(mustBind))
    (e1, binders ++ binders1, mustBind)
  }

  private def letBindEffectsAndTryBind(exp: ReducedAst.Expr)(implicit lctx: LocalContext, flix: Flix): Bound[ReducedAst.Expr] = {
    letBindEffects(identity, exp)
  }

  private def bindIfTrue(e: Expr, mustBind: Boolean)(implicit lctx: LocalContext, flix: Flix): (Expr, Chain[Binder]) = {
    if (mustBind) {
      val (binders1, e1) = letBindThing(e)
      (e1, binders1)
    } else {
      (e, Chain.empty)
    }
  }

  private def letBindThing(exp: Expr)(implicit lctx: LocalContext, flix: Flix): (Chain[Binder], ReducedAst.Expr) = exp match {
    case _: Expr.Cst | _: Expr.Var | _: Expr.ApplyAtomic => (Chain.empty, exp)
    case _: Expr.ApplyClo | _: Expr.ApplyDef |
         _: Expr.ApplySelfTail | _: Expr.ApplySelfTail |
         _: Expr.IfThenElse | _: Expr.Branch | _: Expr.JumpTo |
         _: Expr.Scope | _: Expr.TryCatch | _: Expr.TryWith |
         _: Expr.Do | _: Expr.NewObject | _: Expr.LetRec |
         _: Expr.Let => {
      val fresh = Symbol.freshVarSym("anf", BoundBy.Let, exp.loc)(Level.Default, flix)
      lctx.lparams.addOne(ReducedAst.LocalParam(fresh, exp.tpe))
      (Chain(Binder(Expr.Let(fresh, exp, null, null, null, SourceLocation.Unknown))), ReducedAst.Expr.Var(fresh, exp.tpe, exp.loc))
    }
    case _: Expr.Resume => throw InternalCompilerException("unexpected ast node", SourceLocation.Unknown)
  }

  // <a> means any expression, a is a variable
  // [[...]] means the translation of ... (expr -> Monad[expr])
  // [{[...]}] means the translation of ... where letbinding does not bubble out (expr -> expr)

  //
  // [[<a> + do E()]]
  // --------------
  // let tmp1 = [[<a>]];
  // let tmp2 = do E();
  // tmp1 + tmp2
  //

  //
  // [[<a> + do E(do F())]]
  // --------------
  // let tmp1 = [[<a>]];
  // let tmp2 = do F();
  // let tmp3 = do E(tmp2);
  // tmp1 + tmp3
  //

  //
  // [[f(<a>, b, 2+3, do E(25), <c>]]
  // --------------
  // let tmp1 = f;
  // let tmp2 = [[<a>]];
  // let tmp3 = b;
  // let tmp4 = 2+3;
  // let tmp5 = 25;
  // let tmp6 = do E(tmp4);
  // tmp1(tmp2, tmp3, tmp4, tmp6, [[<c>]])
  //

  //
  // [[if (<a>) then <b> else do E()]]
  // --------------
  // if ([[<a>]] then [{[<b>]}] else do E()
  //

  //
  // [[<c> + (if (<a>) then <b> else do E())]]
  // --------------
  // let tmp1 = [[<c>]];
  // let tmp2 = if ([[<a>]] then [{[<b>]}] else do E();
  // tmp1 + tmp2
  //

  //
  // [[(a + b + c) + (<d> + do E())]]
  // --------------
  // let tmp1 = (a + b + c);
  // let tmp2 = [[<d>]];
  // let tmp3 = do E()
  // let tmp4 = tmp2 + tmp3;
  // tmp1 + tmp4
  //

  //
  // [[<a> + {let b = <c>; do E()}]]
  // --------------
  // let tmp1 = [[<a>]]
  // let b = [[<c>]]
  // let tmp = do E()
  // tmp1 + tmp2
  //

  //
  // [[f({let a = b; 2 + {let c = d; do E()}}, do E())]]
  // --------------
  // let a = b;
  // let c = d;
  // let tmp1 = do E();
  // let tmp2 = 2 + tmp1
  // let tmp3 = do E();
  // f(tmp2, tmp3)
  //

  //
  // [[f({let a = b; 2 + {let c = d; e}}, do E())]]
  // --------------
  // let tmp1 = {let a = b; 2 + {let c = d; e}};
  // let tmp2 = do E();
  // f(tmp1, tmp2)
  //

}
