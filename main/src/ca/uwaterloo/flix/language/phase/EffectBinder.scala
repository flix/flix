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
    // TODO short-circuiting ops are assumed non-short-circuiting

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
  private case class LocalContext(lparams: mutable.ArrayDeque[LocalParam])

  /** [[binder]] is expected to be Let/LetRec/Scope and the body is ignored */
  private case class Binder(binder: Expr)

  private type Bound[A] = (A, Chain[Binder])

  private object Bound {
    def traverse[A](bl: Bound[List[A]], f: A => Bound[Expr])(implicit lctx: LocalContext, flix: Flix): Bound[List[Expr]] = {
      val (l, binders) = bl
      var bs = binders
      val acc: mutable.ArrayBuffer[Expr] = mutable.ArrayBuffer.empty
      for (elem <- l) {
        val (e, bs1) = f(elem)
        val (b, ee) = letBindThing(e)
        bs = bs ++ b ++ bs1
        acc.append(ee)
      }
      (acc.toList, bs)
    }
  }

  private def letBindEffectsDef(defn: Def)(implicit flix: Flix): Def = {
    implicit val lctx: LocalContext = LocalContext.mk()
    val stmt = letBindEffectsStmt(defn.stmt)
    val lparams = defn.lparams ++ lctx.lparams.toList
    defn.copy(stmt = stmt, lparams = lparams)
  }

  private def letBindEffectsStmt(stmt: Stmt)(implicit lctx: LocalContext, flix: Flix): Stmt = stmt match {
    case Stmt.Ret(expr, tpe, loc) => Stmt.Ret(letBindEffectsTopLevel(expr), tpe, loc)
  }

  private def letBindEffectsTopLevel(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Expr = exp match {
    // simple
    case _: Expr.Cst | _: Expr.Var | _: Expr.ApplyAtomic | _: Expr.ApplyClo | _: Expr.ApplyDef |
         _: Expr.ApplySelfTail | _: Expr.IfThenElse | _: Expr.Do | _: Expr.Resume |
         _: Expr.NewObject => {
      val (e, binders) = letBindEffects(exp)
      bindBinders(binders, e)
    }
    // fancy
    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val branches1 = branches.map {
        case (sym, branchExp) => (sym, letBindEffectsTopLevel(branchExp))
      }
      Expr.Branch(e, branches1, tpe, purity, loc)
    case Expr.JumpTo(sym, tpe, purity, loc) =>
      Expr.JumpTo(sym, tpe, purity, loc)
    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val (e1, binders) = letBindEffects(exp1)
      val e2 = letBindEffectsTopLevel(exp2)
      val letExp = Expr.Let(sym, e1, e2, tpe, purity, loc)
      bindBinders(binders, letExp)
    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val (e1, binders) = letBindEffects(exp1)
      val e2 = letBindEffectsTopLevel(exp2)
      val letRecExp = Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
      bindBinders(binders, letRecExp)
    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      Expr.Scope(sym, e, tpe, purity, loc)
    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val rules1 = rules.map {
        case cr => CatchRule(cr.sym, cr.clazz, letBindEffectsTopLevel(cr.exp))
      }
      Expr.TryCatch(e, rules1, tpe, purity, loc)
    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val rules1 = rules.map {
        case hr => hr.copy(exp = letBindEffectsTopLevel(hr.exp))
      }
      Expr.TryWith(letBindEffectsTopLevel(exp), effUse, rules1, tpe, purity, loc)
  }

  /** leftmost binder is the outermost one */
  private def bindBinders(binders: Chain[Binder], e: Expr): Expr = {
    bindBindersAux(binders.toList.reverse, e)
  }

  /** leftmost binder is the innermost one */
  @tailrec
  private def bindBindersAux(binders: List[Binder], e: Expr): Expr = binders match {
    case Binder(Expr.Let(sym, binderExp, _, _, _, loc)) :: tail =>
      val letExp = Expr.Let(sym, binderExp, e, e.tpe, e.purity.combineWith(binderExp.purity), loc)
      bindBindersAux(tail, letExp)
    case Binder(Expr.LetRec(varSym, i, defSym, binderExp, _, _, _, loc)) :: tail =>
      val letRecExp = Expr.LetRec(varSym, i, defSym, binderExp, e, e.tpe, e.purity.combineWith(binderExp.purity), loc)
      bindBindersAux(tail, letRecExp)
    case _ :: _ => throw InternalCompilerException("unexpected binder", SourceLocation.Unknown)
    case Nil => e
  }

  /**
    * Invariant: All effectful applications and Do operations will happen with
    * an empty operand stack.
    *
    * first binder is the outermost one
    */
  private def letBindEffects(exp: Expr)(implicit lctx: LocalContext, flix: Flix): Bound[Expr] = {
    val (e, binders) = exp match {
      case Expr.Cst(_, _, _) => (exp, Chain.empty)
      case Expr.Var(_, _, _) => (exp, Chain.empty)
      case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val (exps1, binders) = Bound.traverse((exps, Chain.empty), letBindEffects)
        val applyAtomicExp = Expr.ApplyAtomic(op, exps1, tpe, purity, loc)
        (applyAtomicExp, binders)
      case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
        val (e1, binders1) = letBindEffects(exp)
        val (exps1, binders2) = Bound.traverse((exps, Chain.empty), letBindEffects)
        val applyCloExp = Expr.ApplyClo(e1, exps1, ct, tpe, purity, loc)
        (applyCloExp, binders1 ++ binders2)
      case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
        val (exps1, binders) = Bound.traverse((exps, Chain.empty), letBindEffects)
        val applyDefExp = Expr.ApplyDef(sym, exps1, ct, tpe, purity, loc)
        (applyDefExp, binders)
      case Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
        val (actuals1, binders) = Bound.traverse((actuals, Chain.empty), letBindEffects)
        val applySelfTailExp = Expr.ApplySelfTail(sym, formals, actuals1, tpe, purity, loc)
        (applySelfTailExp, binders)
      case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val (e1, binders) = letBindEffects(exp1)
        val e2 = letBindEffectsTopLevel(exp2)
        val e3 = letBindEffectsTopLevel(exp3)
        val ifThenElseExp = Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)
        (ifThenElseExp, binders)
      case Expr.Branch(exp, branches, tpe, purity, loc) =>
        val branches1 = branches.map {
          case (sym, branchExp) =>
            (sym, letBindEffectsTopLevel(branchExp))
        }
        val branchExp = Expr.Branch(letBindEffectsTopLevel(exp), branches1, tpe, purity, loc)
        (branchExp, Chain.empty)
      case Expr.JumpTo(sym, tpe, purity, loc) =>
        val jumpToExp = Expr.JumpTo(sym, tpe, purity, loc)
        (jumpToExp, Chain.empty)
      case Expr.Let(sym, exp1, exp2, _, _, loc) =>
        //
        // [[let b = do E(); do E()]]
        // --------------
        // let tmp = do E();
        // let b = tmp;
        // do E()
        //
        val (e1, binders1) = letBindEffects(exp1)
        val (e2, binders2) = letBindEffects(exp2)
        val letBinder = Binder(Expr.Let(sym, e1, null, null, null, loc))
        (e2, binders1 ++ Chain(letBinder) ++ binders2)
      case Expr.LetRec(varSym, index, defSym, exp1, exp2, _, _, loc) =>
        val (e1, binders1) = letBindEffects(exp1)
        val (e2, binders2) = letBindEffects(exp2)
        val letRecBinder = Binder(Expr.LetRec(varSym, index, defSym, e1, null, null, null, loc))
        (e2, binders1 ++ Chain(letRecBinder) ++ binders2)
      case Expr.Scope(sym, exp, tpe, purity, loc) =>
        val scopeExp = Expr.Scope(sym, letBindEffectsTopLevel(exp), tpe, purity, loc)
        (scopeExp, Chain.empty)
      case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val rules1 = rules.map {
          case cr => CatchRule(cr.sym, cr.clazz, letBindEffectsTopLevel(cr.exp))
        }
        val tryCatchExp = Expr.TryCatch(letBindEffectsTopLevel(exp), rules1, tpe, purity, loc)
        (tryCatchExp, Chain.empty)
      case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
        val rules1 = rules.map {
          case hr => HandlerRule(hr.op, hr.fparams, letBindEffectsTopLevel(hr.exp))
        }
        val tryWithExp = Expr.TryWith(letBindEffectsTopLevel(exp), effUse, rules1, tpe, purity, loc)
        (tryWithExp, Chain.empty)
      case Expr.Do(op, exps, tpe, purity, loc) =>
        // TODO fix this in other PR
        val (exps1, binders) = Bound.traverse((exps, Chain.empty), letBindEffects)
        val doExp = Expr.Do(op, exps1, tpe, purity, loc)
        (doExp, binders)
      case Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc) =>
        val (exps1, binders) = Bound.traverse((exps, Chain.empty), letBindEffects)
        val newObjectExp = Expr.NewObject(name, clazz, tpe, purity, methods, exps1, loc)
        (newObjectExp, binders)
      case Expr.Resume(_, _, loc) => throw InternalCompilerException("Explicit resume not supported", loc)
    }
    val (binders1, e1) = letBindThing(e)
    (e1, binders ++ binders1)
  }

  private def letBindThing(exp: Expr)(implicit lctx: LocalContext, flix: Flix): (Chain[Binder], Expr) = exp match {
    // ignore bindings of constants or variables
    // closures are required here to avoid breaking the `letrec f = clo(..., f)` shape
    case _: Expr.Cst | _: Expr.Var | Expr.ApplyAtomic(AtomicOp.Closure(_), _, _, _, _) => (Chain.empty, exp)
    // bind everything else
    case Expr.IfThenElse(exp1, exp2, exp3, _, purity, loc) => {
      val exp = Expr.IfThenElse(exp1, exp2, exp3, exp2.tpe, purity, loc)
      val fresh = Symbol.freshVarSym("anf", BoundBy.Let, exp.loc)(Level.Default, flix)
      lctx.lparams.addOne(LocalParam(fresh, exp.tpe))
      (Chain(Binder(Expr.Let(fresh, exp, null, null, null, SourceLocation.Unknown))), Expr.Var(fresh, exp.tpe, exp.loc))
    }
    case _: Expr.ApplyAtomic | _: Expr.ApplyClo | _: Expr.ApplyDef |
         _: Expr.ApplySelfTail | _: Expr.ApplySelfTail |
         _: Expr.IfThenElse | _: Expr.Branch | _: Expr.JumpTo |
         _: Expr.Scope | _: Expr.TryCatch | _: Expr.TryWith |
         _: Expr.Do | _: Expr.NewObject | _: Expr.LetRec |
         _: Expr.Let => {
      val fresh = Symbol.freshVarSym("anf", BoundBy.Let, exp.loc)(Level.Default, flix)
      lctx.lparams.addOne(LocalParam(fresh, exp.tpe))
      (Chain(Binder(Expr.Let(fresh, exp, null, null, null, SourceLocation.Unknown))), Expr.Var(fresh, exp.tpe, exp.loc))
    }
    case _: Expr.Resume => throw InternalCompilerException("unexpected ast node", SourceLocation.Unknown)
  }

}
