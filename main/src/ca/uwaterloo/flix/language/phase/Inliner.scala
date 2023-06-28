/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur._
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Root
import ca.uwaterloo.flix.language.ast.Purity.{Impure, Pure}
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, LiftedAst, OccurrenceAst, Purity, SemanticOp, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The inliner replaces closures and functions by their code to improve performance.
  */
object Inliner {

  sealed trait Expression

  object Expression {
    case class LiftedExp(exp: LiftedAst.Expression) extends Expression

    case class OccurrenceExp(exp: OccurrenceAst.Expression) extends Expression
  }

  /**
    * Candidates for inlining are functions with fewer than `InlineThreshold` expressions
    */
  private val InlineThreshold = 8

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): Validation[LiftedAst.Root, CompilationMessage] = flix.subphase("Inliner") {
    // Visit every definition in the program.
    val defs = root.defs.map {
      case (sym, defn) => sym -> visitDef(defn)(flix, root)
    }

    // Visit every enum in the program.
    val enums = root.enums.map {
      case (sym, enum) => sym -> visitEnum(enum)
    }

    // TODO RESTR-VARS add restrictable enums

    // Reassemble the ast root.
    val result = LiftedAst.Root(defs, enums, root.entryPoint, root.sources)

    result.toSuccess
  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from OccurrenceAst to LiftedAst.
    */
  private def visitDef(def0: OccurrenceAst.Def)(implicit flix: Flix, root: Root): LiftedAst.Def = {
    val convertedExp = visitExp(def0.exp, Map.empty)(root, flix)
    val cparams = def0.cparams.map {
      case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
    }
    val fparams = def0.fparams.map {
      case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
    }
    LiftedAst.Def(def0.ann, def0.mod, def0.sym, cparams, fparams, convertedExp, def0.tpe, def0.purity, def0.loc)
  }

  /**
    * Converts enum from OccurrenceAst to LiftedAst
    */
  private def visitEnum(enum0: OccurrenceAst.Enum): LiftedAst.Enum = {
    val cases = enum0.cases.map {
      case (sym, caze) =>
        sym -> LiftedAst.Case(sym, caze.tpe, caze.loc)
    }
    LiftedAst.Enum(enum0.ann, enum0.mod, enum0.sym, cases, enum0.tpe, enum0.loc)
  }

  /**
    * Performs inlining operations on the expression `exp0` of type OccurrenceAst.Expression.
    * Returns an expression of type Expression
    */
  private def visitExp(exp0: OccurrenceAst.Expression, subst0: Map[Symbol.VarSym, Expression])(implicit root: Root, flix: Flix): LiftedAst.Expression = exp0 match {
    case OccurrenceAst.Expression.Constant(cst, tpe, loc) => LiftedAst.Expression.Cst(cst, tpe, loc)

    case OccurrenceAst.Expression.Var(sym, tpe, loc) =>
      subst0.get(sym) match {
        // Case 1:
        // The variable `sym` is not in the substitution map and will not be inlined.
        case None => LiftedAst.Expression.Var(sym, tpe, loc)
        // Case 2:
        // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
        case Some(e1) =>
          e1 match {
            // If `e1` is a `LiftedExp` then `e1` has already been visited
            case Expression.LiftedExp(exp) => exp
            // If `e1` is a `OccurrenceExp` then `e1` has not been visited. Visit `e1`
            case Expression.OccurrenceExp(exp) => visitExp(exp, subst0)
          }
      }

    case OccurrenceAst.Expression.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      op match {
        case AtomicOp.Is(sym) if isSingleCaseEnum(sym, es) => LiftedAst.Expression.Cst(Ast.Constant.Bool(true), Type.Bool, loc)

        case AtomicOp.Untag(_) =>
          val List(e) = es
          // Inline expressions of the form Untag(Tag(e)) => e
          e match {
            case LiftedAst.Expression.ApplyAtomic(AtomicOp.Tag(_), innerExps, _, _, _) => innerExps.head
            case _ => LiftedAst.Expression.ApplyAtomic(op, es, tpe, purity, loc)
          }

        case _ => LiftedAst.Expression.ApplyAtomic(op, es, tpe, purity, loc)
      }

    case OccurrenceAst.Expression.ApplyClo(exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      e match {
        case LiftedAst.Expression.ApplyAtomic(AtomicOp.Closure(sym), closureArgs, _, _, _) =>
          val def1 = root.defs.apply(sym)
          // If `def1` is a single non-self call or
          // it is trivial
          // then inline the body of `def1`
          if (canInlineDef(def1)) {
            val e1 = rewriteTailCalls(def1.exp)
            // Map for substituting formal parameters of a function with the closureArgs currently in scope
            bindFormals(e1, (def1.cparams ++ def1.fparams).map(_.sym), closureArgs ++ as, Map.empty)
          } else {
            LiftedAst.Expression.ApplyClo(e, as, tpe, purity, loc)
          }
        case _ =>
          val as = args.map(visitExp(_, subst0))
          LiftedAst.Expression.ApplyClo(e, as, tpe, purity, loc)
      }

    case OccurrenceAst.Expression.ApplyDef(sym, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, subst0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call or
      // it is trivial
      // then inline the body of `def1`
      if (canInlineDef(def1)) {
        val e1 = rewriteTailCalls(def1.exp)
        bindFormals(e1, (def1.cparams ++ def1.fparams).map(_.sym), as, Map.empty)
      } else {
        LiftedAst.Expression.ApplyDef(sym, as, tpe, purity, loc)
      }

    case OccurrenceAst.Expression.ApplyCloTail(exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      e match {
        case LiftedAst.Expression.ApplyAtomic(AtomicOp.Closure(sym), closureArgs, _, _, _) =>
          val def1 = root.defs.apply(sym)
          // If `def1` is a single non-self call or
          // it is trivial
          // then inline the body of `def1`
          if (canInlineDef(def1)) {
            // Map for substituting formal parameters of a function with the freevars currently in scope
            bindFormals(def1.exp, (def1.cparams ++ def1.fparams).map(_.sym), closureArgs ++ as, Map.empty)
          } else {
            LiftedAst.Expression.ApplyCloTail(e, as, tpe, purity, loc)
          }
        case _ =>
          val as = args.map(visitExp(_, subst0))
          LiftedAst.Expression.ApplyCloTail(e, as, tpe, purity, loc)
      }

    case OccurrenceAst.Expression.ApplyDefTail(sym, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, subst0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call or
      // it is trivial
      // then inline the body of `def1`
      if (canInlineDef(def1)) {
        bindFormals(def1.exp, (def1.cparams ++ def1.fparams).map(_.sym), as, Map.empty)
      } else {
        LiftedAst.Expression.ApplyDefTail(sym, as, tpe, purity, loc)
      }

    case OccurrenceAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(visitExp(_, subst0))
      val fs = formals.map(visitFormalParam)
      LiftedAst.Expression.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case OccurrenceAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      reduceIfThenElse(e1, e2, e3, tpe, purity, loc)

    case OccurrenceAst.Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, subst0)
      }
      LiftedAst.Expression.Branch(e, bs, tpe, purity, loc)

    case OccurrenceAst.Expression.JumpTo(sym, tpe, purity, loc) => LiftedAst.Expression.JumpTo(sym, tpe, purity, loc)

    case OccurrenceAst.Expression.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      /// Case 1:
      /// If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
      /// Both code size and runtime are reduced
      if (isDeadAndPure(occur, exp1.purity)) {
        visitExp(exp2, subst0)
      } else {
        /// Case 2:
        /// If `exp1` occurs once and it is pure, then it is safe to inline.
        /// There is a small decrease in code size and runtime.
        val wantToPreInline = isUsedOnceAndPure(occur, exp1.purity)
        if (wantToPreInline) {
          val subst1 = subst0 + (sym -> Expression.OccurrenceExp(exp1))
          visitExp(exp2, subst1)
        } else {
          val e1 = visitExp(exp1, subst0)
          /// Case 3:
          /// If `e1` is trivial and pure, then it is safe to inline.
          // Code size and runtime are not impacted, because only trivial expressions are inlined
          val wantToPostInline = isTrivialAndPure(e1, exp1.purity) && occur != DontInline
          if (wantToPostInline) {
            /// If `e1` is to be inlined:
            /// Add map `sym` to `e1` and return `e2` without constructing the let expression.
            val subst1 = subst0 + (sym -> Expression.LiftedExp(e1))
            visitExp(exp2, subst1)
          } else {
            /// Case 4:
            /// If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
            /// Code size and runtime are not impacted
            val e2 = visitExp(exp2, subst0)
            LiftedAst.Expression.Let(sym, e1, e2, tpe, purity, loc)
          }
        }
      }

    case OccurrenceAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expression.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Scope(sym, e, tpe, purity, loc)

    case OccurrenceAst.Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          LiftedAst.CatchRule(sym, clazz, e)
      }
      LiftedAst.Expression.TryCatch(e, rs, tpe, purity, loc)

    case OccurrenceAst.Expression.InvokeStaticMethod(method, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyAtomic(AtomicOp.InvokeStaticMethod(method), es, tpe, purity, loc)

    case OccurrenceAst.Expression.GetField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.GetField(field), List(e), tpe, purity, loc)

    case OccurrenceAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.PutField(field), List(e1, e2), tpe, purity, loc)

    case OccurrenceAst.Expression.GetStaticField(field, tpe, purity, loc) =>
      LiftedAst.Expression.ApplyAtomic(AtomicOp.GetStaticField(field), List.empty, tpe, purity, loc)

    case OccurrenceAst.Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.PutStaticField(field), List(e), tpe, purity, loc)

    case OccurrenceAst.Expression.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val f = fparams.map {
            case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
          }
          val c = visitExp(clo, subst0)
          LiftedAst.JvmMethod(ident, f, c, retTpe, purity, loc)
      }
      LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc)

    case OccurrenceAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.Spawn, List(e1, e2), tpe, Purity.Impure, loc)

    case OccurrenceAst.Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.Lazy, List(e), tpe, Purity.Pure, loc)

    case OccurrenceAst.Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.Force, List(e), tpe, Purity.Pure, loc)

    case OccurrenceAst.Expression.HoleError(sym, tpe, loc) => LiftedAst.Expression.HoleError(sym, tpe, loc)

    case OccurrenceAst.Expression.MatchError(tpe, loc) => LiftedAst.Expression.MatchError(tpe, loc)
  }

  /**
    * A def can be inlined if
    * Its body consist of a single non self call or
    * Its body has a codesize below the inline threshold and its not being inlined into itself or
    * It only occurs once in the entire program and
    * It does not contain a reference to itself
    */
  private def canInlineDef(def0: OccurrenceAst.Def): Boolean = {
    val mayInline = def0.context.occur != DontInline && !def0.context.isSelfRecursive
    val isBelowInlineThreshold = def0.context.size < InlineThreshold
    val occursOnce = def0.context.occur == Once
    val canInline = def0.context.isDirectCall || isBelowInlineThreshold || occursOnce
    mayInline && canInline
  }

  /**
    * Checks if `occur` is Dead and purity is `Pure`
    */
  private def isDeadAndPure(occur: OccurrenceAst.Occur, purity: Purity): Boolean = (occur, purity) match {
    case (Dead, Purity.Pure) => true
    case _ => false
  }

  /**
    * Checks if `occur` is Once and `purity` is Pure
    */
  private def isUsedOnceAndPure(occur: OccurrenceAst.Occur, purity: Purity): Boolean = (occur, purity) match {
    case (Once, Purity.Pure) => true
    case _ => false
  }

  /**
    * Checks if `exp0` is trivial and `purity` is pure
    */
  private def isTrivialAndPure(exp0: LiftedAst.Expression, purity: Purity): Boolean = purity match {
    case Purity.Pure => isTrivialExp(exp0)
    case _ => false
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    * Substitute variables in `exp0` via the filled substitution map `env0`
    */
  private def bindFormals(exp0: OccurrenceAst.Expression, symbols: List[Symbol.VarSym], args: List[LiftedAst.Expression], env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: Root, flix: Flix): LiftedAst.Expression = {
    (symbols, args) match {
      case (sym :: nextSymbols, e1 :: nextExpressions) =>
        val freshVar = Symbol.freshVarSym(sym)
        val env1 = env0 + (sym -> freshVar)
        val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env1)
        val purity = combine(e1.purity, nextLet.purity)
        LiftedAst.Expression.Let(freshVar, e1, nextLet, exp0.tpe, purity, exp0.loc)
      case _ => substituteExp(exp0, env0)
    }
  }

  /**
    * Combines purities `p1` and `p2`
    * A combined purity is only pure if both `p1` and `p2` are pure, otherwise it is always impure.
    */
  def combine(p1: Purity, p2: Purity): Purity = (p1, p2) match {
    case (Pure, Pure) => Pure
    case _ => Impure
  }

  /**
    * Converts all tail calls in `exp0` to non-tail calls.
    */
  private def rewriteTailCalls(exp0: OccurrenceAst.Expression): OccurrenceAst.Expression = exp0 match {
    case OccurrenceAst.Expression.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      val e2 = rewriteTailCalls(exp2)
      OccurrenceAst.Expression.Let(sym, exp1, e2, occur, tpe, purity, loc)

    case OccurrenceAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e2 = rewriteTailCalls(exp2)
      val e3 = rewriteTailCalls(exp3)
      OccurrenceAst.Expression.IfThenElse(exp1, e2, e3, tpe, purity, loc)

    case OccurrenceAst.Expression.Branch(e0, br0, tpe, purity, loc) =>
      val br = br0 map {
        case (sym, exp) => sym -> rewriteTailCalls(exp)
      }
      OccurrenceAst.Expression.Branch(e0, br, tpe, purity, loc)

    case OccurrenceAst.Expression.ApplyCloTail(exp, args, tpe, purity, loc) => OccurrenceAst.Expression.ApplyClo(exp, args, tpe, purity, loc)

    case OccurrenceAst.Expression.ApplyDefTail(sym, args, tpe, purity, loc) => OccurrenceAst.Expression.ApplyDef(sym, args, tpe, purity, loc)

    case OccurrenceAst.Expression.ApplySelfTail(sym, _, actuals, tpe, purity, loc) => OccurrenceAst.Expression.ApplyDef(sym, actuals, tpe, purity, loc)

    case _ => exp0
  }

  /**
    * returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if:
    * It is either a literal (float, string, int, bool, unit), or it is a variable.
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  private def isTrivialExp(exp0: LiftedAst.Expression): Boolean = exp0 match {
    case LiftedAst.Expression.Cst(_, _, _) => true
    case LiftedAst.Expression.Var(_, _, _) => true
    case _ => false
  }

  /**
    * Substitute variables in `exp0` for new fresh variables in `env0`
    */
  private def substituteExp(exp0: OccurrenceAst.Expression, env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: Root, flix: Flix): LiftedAst.Expression = exp0 match {
    case OccurrenceAst.Expression.Constant(cst, tpe, loc) => LiftedAst.Expression.Cst(cst, tpe, loc)

    case OccurrenceAst.Expression.Var(sym, tpe, loc) => LiftedAst.Expression.Var(env0.getOrElse(sym, sym), tpe, loc)

    case OccurrenceAst.Expression.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(substituteExp(_, env0))

      op match {
        case AtomicOp.Unary(sop) =>
          val List(exp) = exps
          val e = substituteExp(exp, env0)
          unaryFold(sop, e, tpe, purity, loc)

        case AtomicOp.Binary(sop) =>
          val List(exp1, exp2) = exps
          val e1 = substituteExp(exp1, env0)
          val e2 = substituteExp(exp2, env0)
          binaryFold(sop, e1, e2, tpe, purity, loc)

        case _ => LiftedAst.Expression.ApplyAtomic(op, es, tpe, purity, loc)

      }

    case OccurrenceAst.Expression.ApplyClo(exp, args, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val as = args.map(substituteExp(_, env0))
      LiftedAst.Expression.ApplyClo(e, as, tpe, purity, loc)

    case OccurrenceAst.Expression.ApplyDef(sym, args, tpe, purity, loc) =>
      val as = args.map(substituteExp(_, env0))
      LiftedAst.Expression.ApplyDef(sym, as, tpe, purity, loc)

    case OccurrenceAst.Expression.ApplyCloTail(exp, args, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val as = args.map(substituteExp(_, env0))
      LiftedAst.Expression.ApplyCloTail(e, as, tpe, purity, loc)

    case OccurrenceAst.Expression.ApplyDefTail(sym, args, tpe, purity, loc) =>
      val as = args.map(substituteExp(_, env0))
      LiftedAst.Expression.ApplyDefTail(sym, as, tpe, purity, loc)

    case OccurrenceAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(substituteExp(_, env0))
      val fs = formals.map(visitFormalParam)
      LiftedAst.Expression.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case OccurrenceAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      val e3 = substituteExp(exp3, env0)
      reduceIfThenElse(e1, e2, e3, tpe, purity, loc)

    case OccurrenceAst.Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val bs = branches.map {
        case (sym, br) => sym -> substituteExp(br, env0)
      }
      LiftedAst.Expression.Branch(e, bs, tpe, purity, loc)

    case OccurrenceAst.Expression.JumpTo(sym, tpe, purity, loc) => LiftedAst.Expression.JumpTo(sym, tpe, purity, loc)

    case OccurrenceAst.Expression.Let(sym, exp1, exp2, _, tpe, purity, loc) =>
      val freshVar = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshVar)
      val e1 = substituteExp(exp1, env1)
      val e2 = substituteExp(exp2, env1)
      LiftedAst.Expression.Let(freshVar, e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val freshVar = Symbol.freshVarSym(varSym)
      val env1 = env0 + (varSym -> freshVar)
      val e1 = substituteExp(exp1, env1)
      val e2 = substituteExp(exp2, env1)
      LiftedAst.Expression.LetRec(freshVar, index, defSym, e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expression.Scope(sym, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      LiftedAst.Expression.Scope(sym, e, tpe, purity, loc)

    case OccurrenceAst.Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val freshVar = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshVar)
          val e = substituteExp(exp, env1)
          LiftedAst.CatchRule(freshVar, clazz, e)
      }
      LiftedAst.Expression.TryCatch(e, rs, tpe, purity, loc)

    case OccurrenceAst.Expression.InvokeStaticMethod(method, exps, tpe, purity, loc) =>
      val es = exps.map(substituteExp(_, env0))
      LiftedAst.Expression.ApplyAtomic(AtomicOp.InvokeStaticMethod(method), es, tpe, purity, loc)

    case OccurrenceAst.Expression.GetField(field, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.GetField(field), List(e), tpe, purity, loc)

    case OccurrenceAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.PutField(field), List(e1, e2), tpe, purity, loc)

    case OccurrenceAst.Expression.GetStaticField(field, tpe, purity, loc) =>
      LiftedAst.Expression.ApplyAtomic(AtomicOp.GetStaticField(field), List.empty, tpe, purity, loc)

    case OccurrenceAst.Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.PutStaticField(field), List(e), tpe, purity, loc)

    case OccurrenceAst.Expression.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val f = fparams.map {
            case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
          }
          val c = substituteExp(clo, env0)
          LiftedAst.JvmMethod(ident, f, c, retTpe, purity, loc)
      }
      LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc)

    case OccurrenceAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.Spawn, List(e1, e2), tpe, Purity.Impure, loc)

    case OccurrenceAst.Expression.Lazy(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.Lazy, List(e), tpe, Purity.Pure, loc)

    case OccurrenceAst.Expression.Force(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      LiftedAst.Expression.ApplyAtomic(AtomicOp.Force, List(e), tpe, Purity.Pure, loc)

    case OccurrenceAst.Expression.HoleError(sym, tpe, loc) => LiftedAst.Expression.HoleError(sym, tpe, loc)

    case OccurrenceAst.Expression.MatchError(tpe, loc) => LiftedAst.Expression.MatchError(tpe, loc)
  }

  /**
    * Translates the given formal parameter `fparam` from OccurrenceAst.FormalParam into a lifted formal parameter.
    */
  private def visitFormalParam(fparam: OccurrenceAst.FormalParam): LiftedAst.FormalParam = fparam match {
    case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
  }

  /**
    * Performs boolean folding on a given unary expression with the logic:
    * Folds not-true => false and not-false => true.
    */
  private def unaryFold(sop: SemanticOp, e: LiftedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation): LiftedAst.Expression = {
    (sop, e) match {
      case (SemanticOp.BoolOp.Not, LiftedAst.Expression.Cst(Ast.Constant.Bool(b), _, _)) => LiftedAst.Expression.Cst(Ast.Constant.Bool(!b), tpe, loc)
      case _ => LiftedAst.Expression.ApplyAtomic(AtomicOp.Unary(sop), List(e), tpe, purity, loc)
    }
  }

  /**
    * Performs boolean folding on a given binary expression with the logic:
    * Only fold if the expression removed is pure, and
    * For Or-expressions,
    * fold into true if either the left or right expression is true.
    * if either the left or right expression is false, fold into the other expression.
    * For And-expressions,
    * fold into false, if either the left or right expression is false.
    * if either the left or right expression is true, fold into the other expression.
    */
  private def binaryFold(sop: SemanticOp, e1: LiftedAst.Expression, e2: LiftedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation): LiftedAst.Expression = {
    (sop, e1, e2) match {
      case (SemanticOp.BoolOp.And, LiftedAst.Expression.Cst(Ast.Constant.Bool(true), _, _), _) => e2
      case (SemanticOp.BoolOp.And, _, LiftedAst.Expression.Cst(Ast.Constant.Bool(true), _, _)) => e1
      case (SemanticOp.BoolOp.And, LiftedAst.Expression.Cst(Ast.Constant.Bool(false), _, _), _) => LiftedAst.Expression.Cst(Ast.Constant.Bool(false), Type.Bool, loc)
      case (SemanticOp.BoolOp.And, _, LiftedAst.Expression.Cst(Ast.Constant.Bool(false), _, _)) if e1.purity == Pure => LiftedAst.Expression.Cst(Ast.Constant.Bool(false), Type.Bool, loc)
      case (SemanticOp.BoolOp.Or, LiftedAst.Expression.Cst(Ast.Constant.Bool(false), _, _), _) => e2
      case (SemanticOp.BoolOp.Or, _, LiftedAst.Expression.Cst(Ast.Constant.Bool(false), _, _)) => e1
      case (SemanticOp.BoolOp.Or, LiftedAst.Expression.Cst(Ast.Constant.Bool(true), _, _), _) => LiftedAst.Expression.Cst(Ast.Constant.Bool(true), Type.Bool, loc)
      case (SemanticOp.BoolOp.Or, _, LiftedAst.Expression.Cst(Ast.Constant.Bool(true), _, _)) if e1.purity == Pure => LiftedAst.Expression.Cst(Ast.Constant.Bool(true), Type.Bool, loc)
      case _ => LiftedAst.Expression.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), tpe, purity, loc)
    }
  }

  /**
    * If `outerCond` is always true then eliminate else branch
    * If `outerThen` is always true then eliminate then branch
    * Transforms expressions of the form
    * if (c1) (if (c2) e else jump l1) else jump l1)
    * to
    * if (c1 and c2) e else jump l1)
    */
  private def reduceIfThenElse(outerCond: LiftedAst.Expression, outerThen: LiftedAst.Expression, outerElse: LiftedAst.Expression, tpe: Type, purity: Purity, loc: SourceLocation): LiftedAst.Expression = outerCond match {
    case LiftedAst.Expression.Cst(Ast.Constant.Bool(true), _, _) => outerThen
    case LiftedAst.Expression.Cst(Ast.Constant.Bool(false), _, _) => outerElse
    case _ =>
      outerThen match {
        case LiftedAst.Expression.IfThenElse(innerCond, innerThen, innerElse, _, _, _) =>
          (outerElse, innerElse) match {
            case (LiftedAst.Expression.JumpTo(sym1, _, _, _), LiftedAst.Expression.JumpTo(sym2, _, _, _)) if sym1 == sym2 =>
              val op = AtomicOp.Binary(SemanticOp.BoolOp.And)
              val es = List(outerCond, innerCond)
              val tpe = outerCond.tpe
              val pur = combine(outerCond.purity, innerCond.purity)
              val andExp = LiftedAst.Expression.ApplyAtomic(op, es, tpe, pur, loc)
              LiftedAst.Expression.IfThenElse(andExp, innerThen, outerElse, tpe, purity, loc)
            case _ => LiftedAst.Expression.IfThenElse(outerCond, outerThen, outerElse, tpe, purity, loc)
          }
        case _ => LiftedAst.Expression.IfThenElse(outerCond, outerThen, outerElse, tpe, purity, loc)
      }
  }

  /**
    * Helper function for dealing with [[AtomicOp]].
    * Returns `true` if `sym` is an enum with one case and `exps` is pure.
    */
  private def isSingleCaseEnum(sym: Symbol.CaseSym, exps: List[LiftedAst.Expression])(implicit root: Root, flix: Flix): Boolean =
    exps match {
      case e :: Nil =>
        val enum0 = root.enums(sym.enumSym)
        enum0.cases.size == 1 && e.purity == Pure
      case _ => false
    }
}
