/*
 * Copyright 2017 Magnus Madsen, Ifaz Kabir
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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, HandlerBinding}
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * The inlining phase performs careful inlining of select functions based on heuristics.
  */
object Inliner extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    // TODO: Inliner disabled for now.
    return root.toSuccess

    implicit val genSym: GenSym = flix.genSym

    val t = System.nanoTime()

    /**
      * Computes the function definitions eligible for inlining.
      */
    val candidates = root.defs.foldLeft(Set.empty[Symbol.DefnSym]) {
      case (macc, (sym, defn)) if isAtomic(defn.exp) => macc + sym
      case (macc, (sym, defn)) => macc
    }

    /**
      * Transforms expressions inside definitions into optimized definitions.
      */
    val inlinedDefinitions = root.defs.foldLeft(Map.empty[Symbol.DefnSym, SimplifiedAst.Def]) {
      case (macc, (sym, defn)) =>
        val newExp = inline(root.defs, candidates, defn.exp)
        macc + (sym -> defn.copy(exp = newExp))
    }

    val e = System.nanoTime() - t
    root.copy(defs = inlinedDefinitions, time = root.time.copy(inliner = e)).toSuccess
  }

  /**
    * Traverses through `exp` and performs inlining using
    * definitions and sizes from `scores`
    */
  private def inline(definitions: Map[Symbol.DefnSym, SimplifiedAst.Def], candidates: Set[Symbol.DefnSym], exp0: Expression)(implicit genSym: GenSym): Expression = {
    def visit = inline(definitions, candidates, _: Expression)

    exp0 match {
      case Expression.ApplyClo(exp1, args, tpe, loc) =>
        Expression.ApplyClo(visit(exp1), args.map(visit), tpe, loc)

      /* Inline application */
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        //inline arguments
        val args1 = args.map(visit)
        definitions(sym) match {
          case SimplifiedAst.Def(_, _, _, formals, exp1, _, _) =>
            if (candidates.contains(sym)) {
              // Inline the body of the function
              val sub = formals.map(f => f.sym -> Symbol.freshVarSym(f.sym)).toMap
              val bindings = formals.map(f => sub(f.sym)).zip(args1)
              letBindArgs(renameAndSubstitute(exp1, sub), bindings)
            }
            else {
              // Do not inline the body -- score is too high
              Expression.ApplyDef(sym, args1, tpe, loc)
            }
        }

      case Expression.ApplyEff(sym, args, tpe, loc) =>
        Expression.ApplyEff(sym, args, tpe, loc)

      case Expression.ApplyCloTail(exp1, args, tpe, loc) =>
        // Do not inline tail calls.
        Expression.ApplyCloTail(visit(exp1), args.map(visit), tpe, loc)

      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        // Do not inline tail calls.
        Expression.ApplyDefTail(sym, args, tpe, loc)

      case Expression.ApplyEffTail(sym, args, tpe, loc) =>
        // Do not inline tail calls.
        Expression.ApplyEffTail(sym, args, tpe, loc)

      /* Inline inside expression */
      case Expression.Closure(ref, freeVars, tpe, loc) =>
        Expression.Closure(ref, freeVars, tpe, loc)
      case Expression.Unit => exp0
      case Expression.True => exp0
      case Expression.False => exp0
      case Expression.Char(_) => exp0
      case Expression.Float32(_) => exp0
      case Expression.Float64(_) => exp0
      case Expression.Int8(_) => exp0
      case Expression.Int16(_) => exp0
      case Expression.Int32(_) => exp0
      case Expression.Int64(_) => exp0
      case Expression.BigInt(_) => exp0
      case Expression.Str(_) => exp0
      case Expression.Var(_, _, _) => exp0
      case Expression.Def(_, _, _) => exp0
      case Expression.Eff(_, _, _) => exp0
      case Expression.Lambda(args, body, tpe, loc) =>
        Expression.Lambda(args, visit(body), tpe, loc)
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        Expression.ApplySelfTail(sym, formals, actuals.map(visit), tpe, loc)
      case Expression.Unary(sop, op, exp1, tpe, loc) =>
        Expression.Unary(sop, op, visit(exp1), tpe, loc)
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        Expression.Binary(sop, op, visit(exp1), visit(exp2), tpe, loc)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        Expression.IfThenElse(visit(exp1), visit(exp2), visit(exp3), tpe, loc)
      case Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        Expression.Branch(e, bs, tpe, loc)
      case Expression.JumpTo(sym, tpe, loc) => Expression.JumpTo(sym, tpe, loc)
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        Expression.Let(sym, visit(exp1), visit(exp2), tpe, loc)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => None
        Expression.LetRec(sym, visit(exp1), visit(exp2), tpe, loc)
      case Expression.Is(sym, tag, exp1, loc) =>
        Expression.Is(sym, tag, visit(exp1), loc)
      case Expression.Tag(sym, tag, exp1, tpe, loc) =>
        Expression.Tag(sym, tag, visit(exp1), tpe, loc)
      case Expression.Untag(sym, tag, exp1, tpe, loc) =>
        Expression.Untag(sym, tag, visit(exp1), tpe, loc)
      case Expression.Index(base, offset, tpe, loc) =>
        Expression.Index(visit(base), offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) =>
        Expression.Tuple(elms.map(visit), tpe, loc)
      case Expression.ArrayLit(elms, tpe, loc) =>
        Expression.ArrayLit(elms.map(visit), tpe, loc)
      case Expression.ArrayNew(elm, len, tpe, loc) =>
        Expression.ArrayNew(visit(elm), visit(len), tpe, loc)
      case Expression.ArrayLoad(base, index, tpe, loc) =>
        Expression.ArrayLoad(visit(base), visit(index), tpe, loc)
      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        Expression.ArrayStore(visit(base), visit(elm), visit(index), tpe, loc)
      case Expression.ArrayLength(base, tpe, loc) =>
        Expression.ArrayLength(visit(base), tpe, loc)
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        Expression.ArraySlice(visit(base), visit(beginIndex), visit(endIndex), tpe, loc)
      case Expression.Ref(exp1, tpe, loc) =>
        Expression.Ref(visit(exp1), tpe, loc)
      case Expression.Deref(exp1, tpe, loc) =>
        Expression.Deref(visit(exp1), tpe, loc)
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        Expression.Assign(visit(exp1), visit(exp2), tpe, loc)
      case Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = visit(exp)
        val bs = bindings map {
          case HandlerBinding(sym, handler) => HandlerBinding(sym, visit(handler))
        }
        Expression.HandleWith(e, bs, tpe, loc)
      case Expression.Existential(fparam, exp1, loc) =>
        Expression.Existential(fparam, visit(exp1), loc)
      case Expression.Universal(fparam, exp1, loc) =>
        Expression.Universal(fparam, visit(exp1), loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        Expression.NativeConstructor(constructor, args.map(visit), tpe, loc)
      case Expression.NativeField(_, _, _) => exp0
      case Expression.NativeMethod(method, args, tpe, loc) =>
        Expression.NativeMethod(method, args.map(visit), tpe, loc)
      case Expression.UserError(_, _) => exp0
      case Expression.HoleError(_, _, _, _) => exp0
      case Expression.MatchError(_, _) => exp0
      case Expression.SwitchError(_, _) => exp0
      /* Error */
      case Expression.LambdaClosure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Apply(exp1, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    }
  }

  /**
    * Performs alpha-renaming of an expression
    */
  private def renameAndSubstitute(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit genSym: GenSym): Expression = exp0 match {
    case Expression.Unit => exp0
    case Expression.True => exp0
    case Expression.False => exp0
    case Expression.Char(_) => exp0
    case Expression.Float32(_) => exp0
    case Expression.Float64(_) => exp0
    case Expression.Int8(_) => exp0
    case Expression.Int16(_) => exp0
    case Expression.Int32(_) => exp0
    case Expression.Int64(_) => exp0
    case Expression.BigInt(_) => exp0
    case Expression.Str(_) => exp0
    case Expression.Var(sym, tpe, loc) => Expression.Var(env0(sym), tpe, loc)
    case Expression.Def(_, _, _) => exp0
    case Expression.Eff(_, _, _) => exp0
    case Expression.Closure(ref, freeVars, tpe, loc) =>
      Expression.Closure(ref, freeVars.map(fv => fv.copy(sym = env0(fv.sym))), tpe, loc)
    case Expression.ApplyClo(exp1, args, tpe, loc) =>
      Expression.ApplyClo(renameAndSubstitute(exp1, env0), args.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ApplyDef(sym, args, tpe, loc) =>
      Expression.ApplyDef(sym, args.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ApplyEff(sym, args, tpe, loc) =>
      Expression.ApplyEff(sym, args.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ApplyCloTail(exp1, args, tpe, loc) =>
      Expression.ApplyCloTail(renameAndSubstitute(exp1, env0), args.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      Expression.ApplyDefTail(sym, args.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ApplyEffTail(sym, args, tpe, loc) =>
      Expression.ApplyEffTail(sym, args.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      Expression.ApplySelfTail(sym, formals, actuals.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.Unary(sop, op, exp1, tpe, loc) =>
      Expression.Unary(sop, op, renameAndSubstitute(exp1, env0), tpe, loc)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      Expression.Binary(sop, op, renameAndSubstitute(exp1, env0), renameAndSubstitute(exp2, env0), tpe, loc)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      Expression.IfThenElse(renameAndSubstitute(exp1, env0), renameAndSubstitute(exp2, env0), renameAndSubstitute(exp3, env0), tpe, loc)
    case Expression.Branch(exp, branches, tpe, loc) =>
      val e = renameAndSubstitute(exp, env0)
      val bs = branches map {
        case (sym, br) => sym -> renameAndSubstitute(br, env0)
      }
      Expression.Branch(e, bs, tpe, loc)
    case Expression.JumpTo(sym, tpe, loc) => Expression.JumpTo(sym, tpe, loc)
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val newSym = Symbol.freshVarSym(sym)
      val sub1 = env0 + (sym -> newSym)
      Expression.Let(newSym, renameAndSubstitute(exp1, sub1), renameAndSubstitute(exp2, sub1), tpe, loc)
    case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      val newSym = Symbol.freshVarSym(sym)
      val sub1 = env0 + (sym -> newSym)
      Expression.LetRec(newSym, renameAndSubstitute(exp1, sub1), renameAndSubstitute(exp2, sub1), tpe, loc)
    case Expression.Is(sym, tag, exp1, loc) =>
      Expression.Is(sym, tag, renameAndSubstitute(exp1, env0), loc)
    case Expression.Tag(sym, tag, exp1, tpe, loc) =>
      Expression.Tag(sym, tag, renameAndSubstitute(exp1, env0), tpe, loc)
    case Expression.Untag(sym, tag, exp1, tpe, loc) =>
      Expression.Untag(sym, tag, renameAndSubstitute(exp1, env0), tpe, loc)
    case Expression.Index(base, offset, tpe, loc) =>
      Expression.Index(renameAndSubstitute(base, env0), offset, tpe, loc)
    case Expression.Tuple(elms, tpe, loc) =>
      Expression.Tuple(elms.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ArrayLit(elms, tpe, loc) =>
      Expression.ArrayLit(elms.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.ArrayNew(elm, len, tpe, loc) =>
      Expression.ArrayNew(renameAndSubstitute(elm, env0), renameAndSubstitute(len, env0), tpe, loc)
    case Expression.ArrayLoad(base, index, tpe, loc) =>
      Expression.ArrayLoad(renameAndSubstitute(base, env0), renameAndSubstitute(index, env0), tpe, loc)
    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      Expression.ArrayStore(renameAndSubstitute(base, env0), renameAndSubstitute(index, env0), renameAndSubstitute(elm, env0), tpe, loc)
    case Expression.ArrayLength(base, tpe, loc) =>
      Expression.ArrayLength(renameAndSubstitute(base, env0), tpe, loc)
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      Expression.ArraySlice(renameAndSubstitute(base, env0), renameAndSubstitute(beginIndex, env0), renameAndSubstitute(endIndex, env0), tpe, loc)
    case Expression.Ref(exp1, tpe, loc) =>
      Expression.Ref(renameAndSubstitute(exp1, env0), tpe, loc)
    case Expression.Deref(exp1, tpe, loc) =>
      Expression.Deref(renameAndSubstitute(exp1, env0), tpe, loc)
    case Expression.Assign(exp1, exp2, tpe, loc) =>
      Expression.Assign(renameAndSubstitute(exp1, env0), renameAndSubstitute(exp2, env0), tpe, loc)
    case Expression.HandleWith(exp, bindings, tpe, loc) =>
      val e = renameAndSubstitute(exp, env0)
      val bs = bindings map {
        case HandlerBinding(sym, handler) => HandlerBinding(sym, renameAndSubstitute(handler, env0))
      }
      Expression.HandleWith(e, bs, tpe, loc)
    case Expression.Existential(fparam, exp1, loc) =>
      val newFparam = fparam.copy(sym = Symbol.freshVarSym(fparam.sym))
      val sub1 = env0 + (fparam.sym -> newFparam.sym)
      Expression.Existential(newFparam, renameAndSubstitute(exp1, sub1), loc)
    case Expression.Universal(fparam, exp1, loc) =>
      val newFparam = fparam.copy(sym = Symbol.freshVarSym(fparam.sym))
      val sub1 = env0 + (fparam.sym -> newFparam.sym)
      Expression.Universal(newFparam, renameAndSubstitute(exp1, sub1), loc)
    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      Expression.NativeConstructor(constructor, args.map(renameAndSubstitute(_, env0)), tpe, loc)
    case Expression.NativeField(_, _, _) => exp0
    case Expression.NativeMethod(method, args, tpe, loc) =>
      Expression.NativeMethod(method, args.map(renameAndSubstitute(_, env0)), tpe, loc)

    case Expression.UserError(_, _) => exp0
    case Expression.HoleError(_, _, _, _) => exp0
    case Expression.MatchError(_, _) => exp0
    case Expression.SwitchError(_, _) => exp0

    case Expression.Lambda(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.LambdaClosure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.Apply(exp1, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
  }

  /**
    * Takes in a expression that has already been alpha-renamed and a list of args and let binds the arguments
    */
  private def letBindArgs(exp: Expression, args: List[(Symbol.VarSym, Expression)]): Expression = {
    args match {
      case (sym, exp1) :: xs =>
        val exp2 = letBindArgs(exp, xs)
        Expression.Let(sym, exp1, exp2, exp2.tpe, exp1.loc)
      case Nil =>
        exp
    }
  }

  /**
    * Returns `true` if the given expression `exp0` is atomic.
    */
  def isAtomic(exp0: Expression): Boolean = exp0 match {
    //
    // Literals are atomic.
    //
    case Expression.Unit => true
    case Expression.True => true
    case Expression.False => true
    case Expression.Char(lit) => true
    case Expression.Float32(lit) => true
    case Expression.Float64(lit) => true
    case Expression.Int8(lit) => true
    case Expression.Int16(lit) => true
    case Expression.Int32(lit) => true
    case Expression.Int64(lit) => true
    case Expression.BigInt(lit) => true
    case Expression.Str(lit) => true

    //
    // Vars and Defs are atomic.
    //
    case Expression.Var(sym, tpe, loc) => true
    case Expression.Def(sym, tpe, loc) => true
    case Expression.Eff(sym, tpe, loc) => true

    //
    // Closure are atomic.
    //
    case Expression.Closure(defn, freeVars, tpe, loc) => true

    //
    // Applications are atomic if their arguments are, but tail calls are not.
    //
    case Expression.ApplyClo(exp, args, tpe, loc) => isAtomic(exp) && (args forall isAtomic)
    case Expression.ApplyDef(sym, args, tpe, loc) => args forall isAtomic
    case Expression.ApplyEff(sym, args, tpe, loc) => args forall isAtomic
    case Expression.ApplyCloTail(exp, args, tpe, loc) => false
    case Expression.ApplyDefTail(sym, args, tpe, loc) => false
    case Expression.ApplyEffTail(sym, args, tpe, loc) => false
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => false

    //
    // Unary expressions are atomic if the operand is atomic.
    //
    case Expression.Unary(sop, op, exp, tpe, loc) => isAtomic(exp)

    //
    // Binary expressions are atomic if the operands are.
    //
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => isAtomic(exp1) && isAtomic(exp2)

    //
    // If-then-else expressions are never atomic.
    //
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => false

    //
    // Branches are never atomic.
    //
    case Expression.Branch(exp, branches, tpe, loc) => false

    //
    // Jumps are never atomic.
    //
    case Expression.JumpTo(sym, tpe, loc) => false

    //
    // Let expressions are atomic if the component expressions are.
    //
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      isAtomic(exp1) && isAtomic(exp2)

    //
    // Let-rec expressions are atomic if the component expressions are.
    //
    case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      isAtomic(exp1) && isAtomic(exp2)

    //
    // Is expressions are atomic if the operand is.
    //
    case Expression.Is(sym, tag, exp, loc) => isAtomic(exp)

    //
    // Tag expressions are atomic if the operand is.
    //
    case Expression.Tag(sym, tag, exp, tpe, loc) => isAtomic(exp)

    //
    // Untag expressions are atomic if the operand is.
    //
    case Expression.Untag(sym, tag, exp, tpe, loc) => isAtomic(exp)

    //
    // Index expressions are atomic if the operand is.
    //
    case Expression.Index(base, offset, tpe, loc) => isAtomic(base)

    //
    // Tuple expressions are atomic if the elements are.
    //
    case Expression.Tuple(elms, tpe, loc) => elms forall isAtomic

    //
    // ArrayLit expressions are atomic if the elements are.
    //
    case Expression.ArrayLit(elms, tpe, loc) => elms forall isAtomic

    //
    // ArrayNew expressions are atomic if the element is.
    //
    case Expression.ArrayNew(elm, len, tpe, loc) => isAtomic(elm) && isAtomic(len)

    //
    // ArrayLoad expressions are atomic if the base and index are.
    //
    case Expression.ArrayLoad(base, index, tpe, loc) => isAtomic(base) && isAtomic(index)

    //
    // ArrayStore expressions are atomic if the base, index and element are.
    //
    case Expression.ArrayStore(base, index, elm, tpe, loc) => isAtomic(base) && isAtomic(index) && isAtomic(elm)

    //
    // ArrayLength expressions are atomic if the base is.
    //
    case Expression.ArrayLength(base, tpe, loc) => isAtomic(base)

    //
    // ArrayLength expressions are atomic if the base, beginIndex and endIndex are.
    //
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => isAtomic(base) && isAtomic(beginIndex) && isAtomic(endIndex)

    //
    // Reference expressions are atomic.
    //
    case Expression.Ref(exp, tpe, loc) => isAtomic(exp)

    //
    // Dereference expressions are atomic.
    //
    case Expression.Deref(exp, tpe, loc) => isAtomic(exp)

    //
    // Assign expressions are atomic.
    //
    case Expression.Assign(exp1, exp2, tpe, loc) => isAtomic(exp1) && isAtomic(exp2)

    //
    // HandleWith expressions are never atomic.
    //
    case Expression.HandleWith(exp, bindings, tpe, loc) => false

    //
    // Existential expressions are never atomic.
    //
    case Expression.Existential(fparam, exp, loc) => false

    //
    // Universal expressions are never atomic.
    //
    case Expression.Universal(fparam, exp, loc) => false

    //
    // Native Constructors expressions are atomic if their arguments are.
    //
    case Expression.NativeConstructor(constructor, args, tpe, loc) => args forall isAtomic

    //
    // Native Fields are atomic.
    //
    case Expression.NativeField(field, tpe, loc) => true

    //
    // Native Methods are atomic if their arguments are.
    //
    case Expression.NativeMethod(method, args, tpe, loc) => args forall isAtomic

    //
    // Errors are atomic.
    //
    case Expression.UserError(tpe, loc) => true
    case Expression.HoleError(sym, tpe, eff, loc) => true
    case Expression.MatchError(tpe, loc) => true
    case Expression.SwitchError(tpe, loc) => true

    case Expression.Lambda(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.LambdaClosure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.Apply(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
  }

}
