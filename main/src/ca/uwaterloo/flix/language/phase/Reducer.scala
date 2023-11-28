/*
 * Copyright 2023 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.ReducedAst.{Expr, Stmt}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Reducer {

  def run(root: LiftedAst.Root)(implicit flix: Flix): ReducedAst.Root = flix.phase("Reducer") {
    implicit val ctx: SharedContext = SharedContext(new ConcurrentLinkedQueue)

    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    val newEnums = ParOps.parMapValues(root.enums)(visitEnum)
    val newEffs = ParOps.parMapValues(root.effects)(visitEff)

    val root1 = ReducedAst.Root(newDefs, newEnums, newEffs, Set.empty, ctx.anonClasses.asScala.toList, root.entryPoint, root.sources)
    val types = typesOf(root1)
    val root2 = root1.copy(types = types)
    root2.copy(defs = root2.defs.map { case (sym, defn) => (sym, defn.copy(stmt = letBindEffectsStmt(defn.stmt))) })
  }

  private def visitDef(d: LiftedAst.Def)(implicit ctx: SharedContext): ReducedAst.Def = d match {
    case LiftedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, purity, loc) =>
      implicit val lctx: LocalContext = LocalContext.mk()
      val cs = cparams.map(visitFormalParam)
      val fs = fparams.map(visitFormalParam)
      val e = visitExpr(exp)
      val ls = lctx.lparams.toList
      val stmt = ReducedAst.Stmt.Ret(e, e.tpe, e.loc)
      ReducedAst.Def(ann, mod, sym, cs, fs, ls, stmt, tpe, purity, loc)
  }

  private def visitEnum(d: LiftedAst.Enum): ReducedAst.Enum = d match {
    case LiftedAst.Enum(ann, mod, sym, cases0, tpe, loc) =>
      val cases = cases0.map {
        case (sym, caze) => sym -> visitCase(caze)
      }
      val t = tpe
      ReducedAst.Enum(ann, mod, sym, cases, tpe, loc)
  }

  private def visitEff(effect: LiftedAst.Effect): ReducedAst.Effect = effect match {
    case LiftedAst.Effect(ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitEffectOp)
      ReducedAst.Effect(ann, mod, sym, ops, loc)
  }

  private def visitEffectOp(op: LiftedAst.Op): ReducedAst.Op = op match {
    case LiftedAst.Op(sym, ann, mod, fparams0, tpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      ReducedAst.Op(sym, ann, mod, fparams, tpe, purity, loc)
  }

  private def visitExpr(exp0: LiftedAst.Expr)(implicit lctx: LocalContext, ctx: SharedContext): ReducedAst.Expr = exp0 match {
    case LiftedAst.Expr.Cst(cst, tpe, loc) =>
      ReducedAst.Expr.Cst(cst, tpe, loc)

    case LiftedAst.Expr.Var(sym, tpe, loc) =>
      ReducedAst.Expr.Var(sym, tpe, loc)

    case LiftedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyClo(e, es, ct, tpe, purity, loc)

    case LiftedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

    case LiftedAst.Expr.ApplySelfTail(sym, formals, exps, tpe, purity, loc) =>
      val fs = formals.map(visitFormalParam)
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplySelfTail(sym, fs, es, tpe, purity, loc)

    case LiftedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      ReducedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case LiftedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = branches map {
        case (label, body) => label -> visitExpr(body)
      }
      ReducedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case LiftedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      ReducedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case LiftedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      lctx.lparams.addOne(ReducedAst.LocalParam(sym, exp1.tpe))
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ReducedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      lctx.lparams.addOne(ReducedAst.LocalParam(varSym, exp1.tpe))
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ReducedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      lctx.lparams.addOne(ReducedAst.LocalParam(sym, MonoType.Region))
      val e = visitExpr(exp)
      ReducedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case LiftedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules map {
        case LiftedAst.CatchRule(sym, clazz, body) =>
          lctx.lparams.addOne(ReducedAst.LocalParam(sym, MonoType.Object))
          val b = visitExpr(body)
          ReducedAst.CatchRule(sym, clazz, b)
      }
      ReducedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case LiftedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules.map {
        case LiftedAst.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = visitExpr(exp)
          ReducedAst.HandlerRule(op, fps, e)
      }
      ReducedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case LiftedAst.Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.Do(op, es, tpe, purity, loc)

    case LiftedAst.Expr.Resume(exp, tpe, loc) =>
      val e = visitExpr(exp)
      ReducedAst.Expr.Resume(e, tpe, loc)

    case LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val es = methods.map(m => visitExpr(m.clo))
      val specs = methods.map {
        case LiftedAst.JvmMethod(ident, fparams, _, retTpe, purity, loc) =>
          val f = fparams.map(visitFormalParam)
          ReducedAst.JvmMethod(ident, f, retTpe, purity, loc)
      }
      ctx.anonClasses.add(ReducedAst.AnonClass(name, clazz, tpe, specs, loc))

      ReducedAst.Expr.NewObject(name, clazz, tpe, purity, specs, es, loc)
  }

  private def visitCase(caze: LiftedAst.Case): ReducedAst.Case = caze match {
    case LiftedAst.Case(sym, tpe, loc) =>
      ReducedAst.Case(sym, tpe, loc)
  }

  private def visitFormalParam(fparam: LiftedAst.FormalParam): ReducedAst.FormalParam = fparam match {
    case LiftedAst.FormalParam(sym, mod, tpe, loc) =>
      ReducedAst.FormalParam(sym, mod, tpe, loc)
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

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    */
  private case class SharedContext(anonClasses: ConcurrentLinkedQueue[ReducedAst.AnonClass])

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  private def typesOf(root: ReducedAst.Root)(implicit flix: Flix): Set[MonoType] = {
    /**
      * Returns the set of types which occur in the given definition `defn0`.
      */
    def visitDefn(defn: ReducedAst.Def): Set[MonoType] = {
      // Compute the types in the captured formal parameters.
      val cParamTypes = defn.cparams.foldLeft(Set.empty[MonoType]) {
        case (sacc, ReducedAst.FormalParam(_, _, tpe, _)) => sacc + tpe
      }

      // Compute the types in the expression.
      val expressionTypes = visitStmt(defn.stmt)

      // `defn.fparams` and `defn.tpe` are both included in `defn.arrowType`

      // Return the types in the defn.
      cParamTypes ++ expressionTypes + defn.arrowType
    }

    def visitExp(exp0: ReducedAst.Expr): Set[MonoType] = (exp0 match {
      case ReducedAst.Expr.Cst(_, tpe, _) => Set(tpe)

      case ReducedAst.Expr.Var(_, tpe, _) => Set(tpe)

      case ReducedAst.Expr.ApplyClo(exp, exps, _, tpe, _, _) => visitExp(exp) ++ visitExps(exps) ++ Set(tpe)

      case ReducedAst.Expr.ApplyDef(_, exps, _, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case ReducedAst.Expr.ApplySelfTail(_, _, exps, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case ReducedAst.Expr.Branch(exp, branches, _, _, _) =>
        val exps = branches.map {
          case (_, e) => e
        }
        visitExp(exp) ++ visitExps(exps)

      case ReducedAst.Expr.JumpTo(_, _, _, _) => Set.empty

      case ReducedAst.Expr.Let(_, exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case ReducedAst.Expr.LetRec(_, _, _, exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

      case ReducedAst.Expr.Scope(_, exp, _, _, _) => visitExp(exp)

      case ReducedAst.Expr.TryCatch(exp, rules, _, _, _) => visitExp(exp) ++ visitExps(rules.map(_.exp))

      case ReducedAst.Expr.TryWith(exp, _, rules, _, _, _) => visitExp(exp) ++ visitExps(rules.map(_.exp))

      case ReducedAst.Expr.Do(_, exps, tpe, _, _) => visitExps(exps) ++ Set(tpe)

      case ReducedAst.Expr.Resume(exp, tpe, _) => visitExp(exp) ++ Set(tpe)

      case ReducedAst.Expr.NewObject(_, _, _, _, _, exps, _) =>
        visitExps(exps)

      case ReducedAst.Expr.ApplyAtomic(_, exps, tpe, _, _) => visitExps(exps) + tpe

    }) ++ Set(exp0.tpe)

    def visitExps(exps: Iterable[ReducedAst.Expr]): Set[MonoType] = {
      exps.foldLeft(Set.empty[MonoType]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }
    }

    def visitStmt(s: ReducedAst.Stmt): Set[MonoType] = s match {
      case ReducedAst.Stmt.Ret(e, tpe, loc) => visitExp(e)
    }

    // Visit every definition.
    val defTypes = ParOps.parAgg(root.defs.values, Set.empty[MonoType])({
      case (sacc, defn) => sacc ++ visitDefn(defn)
    }, _ ++ _)

    val enumTypes = root.enums.foldLeft(Set.empty[MonoType]) {
      case (sacc, (_, e)) =>
        // the enum type itself
        val eType = e.tpe
        // the types inside the cases
        val caseTypes = e.cases.values.map(_.tpe)
        sacc + eType ++ caseTypes
    }

    val result = defTypes ++ enumTypes

    nestedTypesOf(Set.empty, Queue.from(result))
  }

  /**
    * Returns all the type components of the given `types`.
    *
    * For example, if the types is just the type `Array[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Array[(Bool, Char, Int)]`
    * (and the types in `acc`).
    */
  @tailrec
  private def nestedTypesOf(acc: Set[MonoType], types: Queue[MonoType]): Set[MonoType] = {
    import MonoType._
    types.dequeueOption match {
      case Some((tpe, taskList)) =>
        val taskList1 = tpe match {
          case Unit | Bool | Char | Float32 | Float64 | BigDecimal | Int8 | Int16 |
               Int32 | Int64 | BigInt | String | Regex | Region | Enum(_) |
               RecordEmpty | SchemaEmpty | Native(_) => taskList
          case Array(elm) => taskList.enqueue(elm)
          case Lazy(elm) => taskList.enqueue(elm)
          case Ref(elm) => taskList.enqueue(elm)
          case Tuple(elms) => taskList.enqueueAll(elms)
          case Arrow(targs, tresult) => taskList.enqueueAll(targs).enqueue(tresult)
          case RecordExtend(_, value, rest) => taskList.enqueue(value).enqueue(rest)
          case SchemaExtend(_, t, rest) => taskList.enqueue(t).enqueue(rest)
        }
        nestedTypesOf(acc + tpe, taskList1)
      case None => acc
    }
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

  /** [[binder]] is expected to be Let/LetRec/Scope and the body is ignored */
  private case class Binder(binder: ReducedAst.Expr)

  private type Bound[A] = (A, Chain[Binder], Boolean)

  private object Bound {
    def traverseReverse[A](bl: Bound[List[A]], f: A => Bound[ReducedAst.Expr])(implicit flix: Flix): Bound[List[ReducedAst.Expr]] = {
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

  private def letBindEffectsStmt(stmt: ReducedAst.Stmt)(implicit flix: Flix): ReducedAst.Stmt = stmt match {
    case Stmt.Ret(expr, tpe, loc) => Stmt.Ret(letBindEffectsTopLevel(expr)._1, tpe, loc)
  }

  private def letBindEffectsTopLevel(exp: ReducedAst.Expr)(implicit flix: Flix): (ReducedAst.Expr, Boolean) = exp match {
    // fancy
    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val (e, mustBind) = letBindEffectsTopLevel(exp)
      val (branches1, mustBinds) = branches.map{
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
  private def letBindEffects(mustBindThis: Boolean => Boolean, exp: ReducedAst.Expr)(implicit flix: Flix): Bound[ReducedAst.Expr] = {
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
        val (e1, binders1, mustBind1) = letBindEffects(b => b || mustBind2,exp)
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

  private def letBindEffectsAndTryBind(exp: ReducedAst.Expr)(implicit flix: Flix): Bound[ReducedAst.Expr] = {
    letBindEffects(identity, exp)
  }

  private def bindIfTrue(e: Expr, mustBind: Boolean)(implicit flix: Flix): (Expr, Chain[Binder]) = {
    if (mustBind) {
      val (binders1, e1) = letBindThing(e)
      (e1, binders1)
    } else {
      (e, Chain.empty)
    }
  }

  private def letBindThing(exp: Expr)(implicit flix: Flix): (Chain[Binder], ReducedAst.Expr) = exp match {
    case _: Expr.Cst | _: Expr.Var | _: Expr.ApplyAtomic => (Chain.empty, exp)
    case _: Expr.ApplyClo | _: Expr.ApplyDef |
         _: Expr.ApplySelfTail | _: Expr.ApplySelfTail |
         _: Expr.IfThenElse | _: Expr.Branch | _: Expr.JumpTo |
         _: Expr.Scope | _: Expr.TryCatch | _: Expr.TryWith |
         _: Expr.Do | _: Expr.NewObject | _: Expr.LetRec |
         _: Expr.Let => {
      val fresh = Symbol.freshVarSym("anf", BoundBy.Let, exp.loc)(Level.Default, flix)
      (Chain(Binder(Expr.Let(fresh, exp, null, null, null, SourceLocation.Unknown))), ReducedAst.Expr.Var(fresh, exp.tpe, exp.loc))
    }
    case _: Expr.Resume => throw InternalCompilerException("unexpected ast node", SourceLocation.Unknown)
  }
}
