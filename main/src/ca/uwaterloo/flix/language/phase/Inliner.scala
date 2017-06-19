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
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition.Constant
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, FreeVar, LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.annotation.tailrec

/**
  * The inlining phase performs careful inlining of select functions based on heuristics.
  *
  * - A function of zero arguments is always inlined.
  * - A function of one boolean argument is always inlined.
  * - A function of two boolean arguments is always inlined.
  * - A function with a small "heuristic score" is always inlined.
  */
object Inliner extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    implicit val genSym: GenSym = flix.genSym

    val t = System.nanoTime()

    val definitions = root.definitions

    /**
      * Computes the score of each function definition
      */
    val scores = definitions.foldLeft(Map.empty[Symbol.DefnSym, Int]) {
      case (macc, (sym, constant)) =>
        macc + (sym -> Score.score(constant))
    }

    val inlinedDefinitions = definitions.foldLeft(Map.empty[Symbol.DefnSym, Constant]) {
      case (macc, (sym,constant)) =>
        val newExp = inlineExpr(definitions, scores, constant.exp)
      macc + (sym -> constant.copy(exp = newExp))
    }

    val e = System.nanoTime() - t
    root.copy(definitions = inlinedDefinitions, time = root.time.copy(inliner = e)).toSuccess
  }

  /**
    * Performs the variable substitutions from `varSyms` into `exp`
    */
  def substitute(exp: Expression, varSyms: Map[Symbol.VarSym, Symbol.VarSym]): Expression =
  {
    def f(e: Expression): Option[Expression] =
    e match {
      /* Substitute this expression */
      case v: Expression.Var =>
        varSyms.get(v.sym) match {
          case None => None
          case Some(sym) => Some(v.copy(sym = sym))
        }
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
        Some(Expression.MkClosureRef(ref, freeVars.map(fv => varSyms.get(fv.sym) match {
          case None => fv
          case Some(sym) => FreeVar(sym, fv.tpe)
        }), tpe, loc))
      /* Substitute inside expression */
      case _: LoadExpression => None
      case _: StoreExpression => None
      case Expression.Unit => None
      case Expression.True => None
      case Expression.False => None
      case Expression.Char(_) => None
      case Expression.Float32(_) => None
      case Expression.Float64(_) => None
      case Expression.Int8(_) => None
      case Expression.Int16(_) => None
      case Expression.Int32(_) => None
      case Expression.Int64(_) => None
      case Expression.BigInt(_) => None
      case Expression.Str(_) => None
      case Expression.Ref(sym, _, _) => None
      case Expression.Lambda(_, _, _, _) => None
      case Expression.Hook(_, _, _) => None
      case Expression.MkClosure(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression $e after lambda lifting")
      case Expression.ApplyRef(_, _, _, _) => None
      case Expression.ApplyTail(_, _, _, _, _) => None
      case Expression.ApplyHook(_, _, _, _) => None
      case Expression.Apply(_, _, _, _) => None
      case Expression.Unary(_, _, _, _) => None
      case Expression.Binary(_, _, _, _, _) => None
      case Expression.IfThenElse(_, _, _, _, _) => None
      case Expression.Let(_, _, _, _, _) => None
      case Expression.LetRec(_, _, _, _, _) => None
      case Expression.Is(_, _, _, _) => None
      case Expression.Tag(_, _, _, _, _) => None
      case Expression.Untag(_, _, _, _, _) => None
      case Expression.Index(_, _, _, _) => None
      case Expression.Tuple(_, _, _) => None
      case Expression.Existential(_, _, _) => None
      case Expression.Universal(_, _, _) => None
      case Expression.NativeConstructor(_, _, _, _) => None
      case Expression.NativeField(_, _, _) => None
      case Expression.NativeMethod(_, _, _, _) => None
      case Expression.UserError(_, _) => None
      case Expression.MatchError(_, _) => None
      case Expression.SwitchError(_, _) => None
    }
    AstVisitor.visitExpressionWith(f, exp)
  }

  /**
    * Performs some high-level inlining using `scores`
    */
  def inlineVisitor(definitions: Map[Symbol.DefnSym, Constant], scores: Map[Symbol.DefnSym, Int], exp: Expression)(implicit genSym: GenSym): Option[Expression] =
  {
    /**
      * Returns args and the symbols they should be bound to in required order for substitution and let binding
      * @param args List[Expression] of arguments
      * @return (revArgs, argSymbols) where argSymbols is arguments with all arguments replaced with appropriate symbols
      *         and revArgs are non-var arguments in reverse order
      */
    def argsToSym(args: List[Expression]): (List[(Expression,Symbol.VarSym)], List[Symbol.VarSym]) =
    {
      /**
        * @param letAcc Arguments and symbols they need to be bound to in reverse order
        * @param symAcc Symbols to use for inlining in reverse order
        * @param args Arguments to replace with variables if not variables
        */
      @tailrec def reverseArgVars(letAcc: List[(Expression,Symbol.VarSym)], symAcc: List[Symbol.VarSym], args: List[Expression]):
      (List[(Expression,Symbol.VarSym)], List[Symbol.VarSym]) =
      {
        args match {
          case x :: xs =>
            x match {
              case v: Expression.Var =>
                reverseArgVars(letAcc, v.sym::symAcc, xs)
              case _ =>
                val s = Symbol.freshVarSym("inlnrL")
                reverseArgVars((x, s)::letAcc, s::symAcc, xs)
            }
          case Nil =>
            (letAcc, symAcc.reverse)
        }
      }

      reverseArgVars(Nil, Nil, args)
    }

    @tailrec def letBindAndInlineArgs(revArgs: List[(Expression,Symbol.VarSym)], expr: Expression): Expression =
    {
      revArgs match {
        case x :: xs =>
          val letTerm = inlineExpr(definitions,scores,x._1)
          val letSym = x._2
          letBindAndInlineArgs(xs, Expression.Let(letSym,letTerm,expr,expr.tpe,letTerm.loc))
        case Nil =>
          expr
      }
    }

    exp match {
      /* Inline application */
      case Expression.ApplyRef(sym, args, _, _) =>
        definitions(sym) match {
          case Constant(_, _, formals, exp1, _, _, _) =>
            if (scores(sym) <= Score.MaxScore){
              val freshExp1 = FreshenExpr.alphaRename(exp1)
              val (revArgs, argSymbols) = argsToSym(args)
              val formalMap = formals.map(f => f.sym).zip(argSymbols).toMap
              val substituted = substitute(freshExp1, formalMap)
              val letBoundExpr = letBindAndInlineArgs(revArgs, substituted)
              Some(letBoundExpr)
            }
            else {
              None
            }
        }
      /* Inline inside expression */
      case Expression.MkClosureRef(_, _, _, _) => None
      case _: LoadExpression => None
      case _: StoreExpression => None
      case Expression.Unit => None
      case Expression.True => None
      case Expression.False => None
      case Expression.Char(_) => None
      case Expression.Float32(_) => None
      case Expression.Float64(_) => None
      case Expression.Int8(_) => None
      case Expression.Int16(_) => None
      case Expression.Int32(_) => None
      case Expression.Int64(_) => None
      case Expression.BigInt(_) => None
      case Expression.Str(_) => None
      case Expression.Var(_, _, _) => None
      case Expression.Ref(_, _, _) => None
      case Expression.Lambda(_, _, _, _) => None
      case Expression.Hook(_, _, _) => None
      case Expression.ApplyTail(_, _, _, _, _) => None
      case Expression.ApplyHook(_, _, _, _) => None
      case Expression.Apply(_, _, _, _) => None
      case Expression.Unary(_, _, _, _) => None
      case Expression.Binary(_, _, _, _, _) => None
      case Expression.IfThenElse(_, _, _, _, _) => None
      case Expression.Let(_, _, _, _, _) => None
      case Expression.LetRec(_, _, _, _, _) => None
      case Expression.Is(_, _, _, _) => None
      case Expression.Tag(_, _, _, _, _) => None
      case Expression.Untag(_, _, _, _, _) => None
      case Expression.Index(_, _, _, _) => None
      case Expression.Tuple(_, _, _) => None
      case Expression.Existential(_, _, _) => None
      case Expression.Universal(_, _, _) => None
      case Expression.NativeConstructor(_, _, _, _) => None
      case Expression.NativeField(_, _, _) => None
      case Expression.NativeMethod(_, _, _, _) => None
      case Expression.UserError(_, _) => None
      case Expression.MatchError(_, _) => None
      case Expression.SwitchError(_, _) => None
      /* Error */
      case Expression.MkClosure(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression $exp after lambda lifting")
    }
  }

  /**
    * Traverses through `exp` and performs inlining using
    * definitions and sizes from `scores`
    */
  def inlineExpr(definitions: Map[Symbol.DefnSym, Constant], scores: Map[Symbol.DefnSym, Int], exp: Expression)(implicit genSym: GenSym): Expression =
  {
    def f = inlineVisitor(definitions,scores,_: Expression)
    AstVisitor.visitExpressionWith(f, exp)
  }

}

object FreshenExpr {

  /**
    * Freshen all let binds in an expression
    */
  def alphaRename(exp: Expression)(implicit genSym: GenSym): Expression =
  {
    val (m,newLets) = freshSymbols(exp)
    Inliner.substitute(newLets,m)
  }

  def freshSymbols(exp:Expression)(implicit genSym: GenSym): (Map[Symbol.VarSym, Symbol.VarSym], Expression) =
  {
    var m = Map.empty[Symbol.VarSym, Symbol.VarSym]

    def freshExprVisitor(exp3:Expression): Option[Expression] =
    {
      exp3 match {
        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val newSym = Symbol.freshVarSym("inlnrFresh")
          m += (sym -> newSym)
          val freshExp1 = AstVisitor.visitExpressionWith(freshExprVisitor, exp1)
          val freshExp2 = AstVisitor.visitExpressionWith(freshExprVisitor, exp2)
          Some(Expression.Let(newSym,freshExp1,freshExp2,tpe,loc))
        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val newSym = Symbol.freshVarSym("inlnrFresh")
          m += (sym -> newSym)
          val freshExp1 = AstVisitor.visitExpressionWith(freshExprVisitor, exp1)
          val freshExp2 = AstVisitor.visitExpressionWith(freshExprVisitor, exp2)
          Some(Expression.LetRec(newSym,freshExp1,freshExp2,tpe,loc))
        case _ => None
      }
    }

    val newLets = AstVisitor.visitExpressionWith(freshExprVisitor, exp)
    (m,newLets)
  }

}

object AstVisitor {

  /**
    * Expression Visitor function for SimplifiedAST
    *
    * @param f function that takes in a Expression and returns a Option[Expression]
    * @param exp Expression to visit
    * @return if f(exp) is Some(e) then e, else recursively visit expressions in exp
    */
  def visitExpressionWith(f: Expression => Option[Expression], exp: Expression): Expression =
  {
    def visit(exp1: Expression): Expression = visitExpressionWith(f,exp1)
    f(exp) match {
      case None =>
        exp match {
          case _: StoreExpression => exp
          case _: LoadExpression => exp
          case Expression.Unit => exp
          case Expression.True => exp
          case Expression.False => exp
          case Expression.Char(_) => exp
          case Expression.Float32(_) => exp
          case Expression.Float64(_) => exp
          case Expression.Int8(_) => exp
          case Expression.Int16(_) => exp
          case Expression.Int32(_) => exp
          case Expression.Int64(_) => exp
          case Expression.BigInt(_) => exp
          case Expression.Str(lit) => exp
          case Expression.Var(_, _, _) => exp
          case Expression.Ref(_, _, _) => exp
          case Expression.Hook(_,_,_) => exp
          case Expression.MkClosureRef(_, _, _, _) => exp
          case Expression.NativeField(_, _, _) => exp
          case Expression.UserError(_, _) => exp
          case Expression.MatchError(_, _) => exp
          case Expression.SwitchError(_, _) => exp
          case Expression.Lambda(args, body, tpe, loc) =>
            Expression.Lambda(args, visit(body), tpe, loc)
          case Expression.ApplyRef(sym, args, tpe, loc) =>
            Expression.ApplyRef(sym, args.map(visit), tpe, loc)
          case Expression.ApplyTail(sym, formals, actuals, tpe, loc) =>
            Expression.ApplyTail(sym, formals, actuals.map(visit), tpe, loc)
          case Expression.ApplyHook(hook, args, tpe, loc) =>
            Expression.ApplyHook(hook, args.map(visit), tpe, loc)
          case Expression.Apply(exp1, args, tpe, loc) =>
            Expression.Apply(visit(exp1), args.map(visit), tpe, loc)
          case Expression.Unary(op, exp1, tpe, loc) =>
            Expression.Unary(op, visit(exp1), tpe, loc)
          case Expression.Binary(op, exp1, exp2, tpe, loc) =>
            Expression.Binary(op, visit(exp1), visit(exp2), tpe, loc)
          case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
            Expression.IfThenElse(visit(exp1), visit(exp2), visit(exp3), tpe, loc)
          case Expression.Let(sym, exp1, exp2, tpe, loc) =>
            Expression.Let(sym, visit(exp1), visit(exp2), tpe, loc)
          case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
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
          case Expression.Existential(fparam, exp1, loc) =>
            Expression.Existential(fparam, visit(exp1), loc)
          case Expression.Universal(fparam, exp1, loc) =>
            Expression.Universal(fparam, visit(exp1), loc)
          case Expression.NativeConstructor(constructor, args, tpe, loc) =>
            Expression.NativeConstructor(constructor, args.map(visit), tpe, loc)
          case Expression.NativeMethod(method, args, tpe, loc) =>
            Expression.NativeMethod(method, args.map(visit), tpe, loc)
          case Expression.MkClosure(_, _, _, _) =>
            throw InternalCompilerException(s"Unexpected expression $exp after lambda lifting")
        }
      case Some(exp1) => exp1
    }
  }

}

object Score {
  /**
    * The maximum score of a function for it to be eligible for inlining.
    */
  val MaxScore = 25

  /**
    * Returns the score of the given function definition `defn`.
    *
    * The score of an expression is computed as:
    *
    * - Every expression is worth one point (plus the sum of its children), except:
    * - A literal is worth zero points.
    * - A UserError, MatchError, or SwitchError is worth zero points.
    * - An if-then-else statement is worth the value of the condition plus two times the sum of the consequent and alternative.
    */
  def score(definitions: Constant): Int = exprScore(definitions.exp)

  /**
    * Returns the score of the given expression `exp`.
    */
  def exprScore(exp: Expression): Int = {
    exp match {
      case _: LoadExpression => 1
      case _: StoreExpression => 1
      case Expression.Unit => 0
      case Expression.True => 0
      case Expression.False => 0
      case Expression.Char(_) => 0
      case Expression.Float32(_) => 0
      case Expression.Float64(_) => 0
      case Expression.Int8(_) => 0
      case Expression.Int16(_) => 0
      case Expression.Int32(_) => 0
      case Expression.Int64(_) => 0
      case Expression.BigInt(_) => 0
      case Expression.Str(_) => 0
      case Expression.Var(_, _, _) => 1
      case Expression.Ref(_, _, _) => 1
      case Expression.Lambda(args, body, _, _) => 1 + args.length + exprScore(body)
      case Expression.Hook(_, _, _) => 1
      case Expression.MkClosureRef(ref, freeVars, _, _) => 1 + freeVars.length + exprScore(ref)
      case Expression.ApplyRef(sym, args, _, _) => 1 + args.map(exprScore).sum
      case Expression.ApplyTail(sym, formals, actuals, _, _) =>
        // Not to be inlined
        MaxScore + 1 + formals.length + actuals.map(exprScore).sum
      case Expression.ApplyHook(hook, args, _, _) => 1 + args.map(exprScore).sum
      case Expression.Apply(exp1, args, _, _) => 1 + exprScore(exp1) + args.map(exprScore).sum
      case Expression.Unary(op, exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Binary(op, exp1, exp2, _, _) => 1 + exprScore(exp1) + exprScore(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) => exprScore(exp1) + (2 * (exprScore(exp2) + exprScore(exp3)))
      case Expression.Let(sym, exp1, exp2, _, _) => 1 + exprScore(exp1) + exprScore(exp2)
      case Expression.LetRec(sym, exp1, exp2, _, _) => 1 + exprScore(exp1) + exprScore(exp2)
      case Expression.Is(sym, tag, exp1, loc) => 1 + exprScore(exp1)
      case Expression.Tag(sym, tag, exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Untag(sym, tag, exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Index(base, offset, _, _) => 2
      case Expression.Tuple(elms, _, _) => 1 + elms.map(exprScore).sum
      case Expression.Existential(fparam, exp1, loc) => 2 + exprScore(exp1)
      case Expression.Universal(fparam, exp1, loc) => 2 + exprScore(exp1)
      case Expression.NativeConstructor(constructor, args, _, _) => 2 + args.map(exprScore).sum
      case Expression.NativeField(field, _, _) => 2
      case Expression.NativeMethod(method, args, _, _) => 2 + args.map(exprScore).sum
      case Expression.UserError(_, _) => 0
      case Expression.MatchError(_, _) => 0
      case Expression.SwitchError(_, _) => 0
      case Expression.MkClosure(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression $exp after lambda lifting")
    }
  }
}
