/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, FreeVar, LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, VarSym}
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.annotation.tailrec
import scala.collection.immutable.{::, Nil}

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

    val definitions = root.definitions

    val scores = definitions.foldLeft(Map.empty[DefnSym, Int]) {
      case (macc, (sym, constant)) =>
        macc + (sym -> Score.score(constant))
    }

    val inlinedDefinitions =
    definitions.foldLeft(Map.empty[DefnSym, Constant]) {
      case (macc, (sym,constant)) =>
        val newExp = inlineExpr(definitions, scores, constant.exp)
      macc + (sym -> constant.copy(exp = newExp))
    }

    root.copy(definitions = inlinedDefinitions).toSuccess

  }

  /**
    * Performs the variable substitutions from `varSyms` into `exp`
    */
  def substituteWith(varSyms: Map[VarSym, VarSym], exp: Expression): Expression =
  {
    def f(e: Expression): Option[Expression] =
    e match {
      /* Substitute this expression */
      case v: Var =>
        varSyms.get(v.sym) match {
          case None => None
          case Some(sym) => Some(v.copy(sym = sym))
        }
      case MkClosureRef(ref, freeVars, tpe, loc) =>
        Some(MkClosureRef(ref, freeVars.map(fv => varSyms.get(fv.sym) match {
          case None => fv
          case Some(sym) => FreeVar(sym, fv.tpe)
        }), tpe, loc))
      /* Substitute inside expression */
      case _: LoadExpression => None
      case _: StoreExpression => None
      case Unit => None
      case True => None
      case False => None
      case Char(_) => None
      case Float32(_) => None
      case Float64(_) => None
      case Int8(_) => None
      case Int16(_) => None
      case Int32(_) => None
      case Int64(_) => None
      case BigInt(_) => None
      case Str(_) => None
      case Ref(sym, _, _) => None
      case Lambda(_, _, _, _) => None
      case Hook(_, _, _) => None
      case MkClosure(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression $e after lambda lifting")
      case ApplyRef(_, _, _, _) => None
      case ApplyTail(_, _, _, _, _) => None
      case ApplyHook(_, _, _, _) => None
      case Apply(_, _, _, _) => None
      case Unary(_, _, _, _) => None
      case Binary(_, _, _, _, _) => None
      case IfThenElse(_, _, _, _, _) => None
      case Let(_, _, _, _, _) => None
      case LetRec(_, _, _, _, _) => None
      case Is(_, _, _, _) => None
      case Tag(_, _, _, _, _) => None
      case Untag(_, _, _, _, _) => None
      case Index(_, _, _, _) => None
      case Tuple(_, _, _) => None
      case Existential(_, _, _) => None
      case Universal(_, _, _) => None
      case NativeConstructor(_, _, _, _) => None
      case NativeField(_, _, _) => None
      case NativeMethod(_, _, _, _) => None
      case UserError(_, _) => None
      case MatchError(_, _) => None
      case SwitchError(_, _) => None
    }
    AstVisitor.visitExpressionWith(f, exp)
  }

  /**
    * Either performs some high-level inlining using `scores`
    * or returns None to ask traversal to inline inside `exp`
    */
  def inlineVisitor(definitions: Map[DefnSym, Constant], scores: Map[DefnSym, Int], exp: Expression)(implicit genSym: GenSym): Option[Expression] =
  {
    /**
      * Returns args and the symbols they should be bound to in required order for substitution and let binding
      * @param args List[Expression] of arguments
      * @return (revArgs, argSymbols) where argSymbols is arguments with all arguments replaced with appropriate symbols
      *         and revArgs are non-var arguments in reverse order
      */
    def argsToSym(args: List[Expression]): (List[(Expression,VarSym)], List[VarSym]) =
    {
      /**
        * @param letAcc Arguments and symbols they need to be bound to in reverse order
        * @param symAcc Symbols to use for inlining in reverse order
        * @param args Arguments to replace with variables if not variables
        */
      @tailrec def reverseArgVars(letAcc: List[(Expression,VarSym)], symAcc: List[VarSym], args: List[Expression]):
      (List[(Expression,VarSym)], List[VarSym]) =
      {
        args match {
          case ::(head, tl) =>
            head match {
              case v: Var =>
                reverseArgVars(letAcc, v.sym::symAcc, tl)
              case _ =>
                val s = Symbol.freshVarSym("inlnrL")
                reverseArgVars((head, s)::letAcc, s::symAcc, tl)
            }
          case Nil =>
            (letAcc, symAcc.reverse)
        }
      }

      reverseArgVars(Nil, Nil, args)
    }

    @tailrec def letBindAndInlineArgs(revArgs: List[(Expression,VarSym)], expr: Expression): Expression =
    {
      revArgs match {
        case ::(head, tl) =>
          val letTerm = inlineExpr(definitions,scores,head._1)
          val letSym = head._2
          letBindAndInlineArgs(tl, Let(letSym,letTerm,expr,expr.tpe,letTerm.loc))
        case Nil =>
          expr
      }
    }

    exp match {
      /* Inline application */
      case ApplyRef(sym, args, _, _) =>
        definitions(sym) match {
          case Constant(_, _, formals, exp1, _, _, _) =>
            if (scores(sym) <= Score.MaxScore){
              val freshExp1 = FreshenExpr.freshenExpr(exp1)
              val (revArgs, argSymbols) = argsToSym(args)
              val formalMap = formals.map(f => f.sym).zip(argSymbols).toMap
              val substituted = substituteWith(formalMap,freshExp1)
              val letBoundExpr = letBindAndInlineArgs(revArgs, substituted)
              Some(letBoundExpr)
            }
            else {
              None
            }
        }
      /* Inline inside expression */
      case MkClosureRef(_, _, _, _) => None
      case _: LoadExpression => None
      case _: StoreExpression => None
      case Unit => None
      case True => None
      case False => None
      case Char(_) => None
      case Float32(_) => None
      case Float64(_) => None
      case Int8(_) => None
      case Int16(_) => None
      case Int32(_) => None
      case Int64(_) => None
      case BigInt(_) => None
      case Str(_) => None
      case Var(_, _, _) => None
      case Ref(_, _, _) => None
      case Lambda(_, _, _, _) => None
      case Hook(_, _, _) => None
      case ApplyTail(_, _, _, _, _) => None
      case ApplyHook(_, _, _, _) => None
      case Apply(_, _, _, _) => None
      case Unary(_, _, _, _) => None
      case Binary(_, _, _, _, _) => None
      case IfThenElse(_, _, _, _, _) => None
      case Let(_, _, _, _, _) => None
      case LetRec(_, _, _, _, _) => None
      case Is(_, _, _, _) => None
      case Tag(_, _, _, _, _) => None
      case Untag(_, _, _, _, _) => None
      case Index(_, _, _, _) => None
      case Tuple(_, _, _) => None
      case Existential(_, _, _) => None
      case Universal(_, _, _) => None
      case NativeConstructor(_, _, _, _) => None
      case NativeField(_, _, _) => None
      case NativeMethod(_, _, _, _) => None
      case UserError(_, _) => None
      case MatchError(_, _) => None
      case SwitchError(_, _) => None
      /* Error */
      case MkClosure(_, _, _, _) =>
        throw InternalCompilerException(s"Unexpected expression $exp after lambda lifting")
    }
  }

  /**
    * Traverses through `exp` and performs inlining using
    * definitions and sizes from `scores`
    */
  def inlineExpr(definitions: Map[DefnSym, Constant], scores: Map[DefnSym, Int], exp: Expression)(implicit genSym: GenSym): Expression =
  {
    def f = inlineVisitor(definitions,scores,_: Expression)
    AstVisitor.visitExpressionWith(f, exp)
  }

}

object FreshenExpr {

  /**
    * Freshen all let binds in an expression
    */
  def freshenExpr(exp: Expression)(implicit genSym: GenSym): Expression =
  {
    val (m,newLets) = freshSymbols(exp)
    Inliner.substituteWith(m,newLets)
  }

  def freshSymbols(exp:Expression)(implicit genSym: GenSym): (Map[VarSym, VarSym], Expression) =
  {
    var m = Map.empty[VarSym, VarSym]

    def freshExprVisitor(exp3:Expression): Option[Expression] =
    {
      exp3 match {
        case Let(sym, exp1, exp2, tpe, loc) =>
          val newSym = Symbol.freshVarSym("inlnrFresh")
          m += (sym -> newSym)
          val freshExp1 = AstVisitor.visitExpressionWith(freshExprVisitor, exp1)
          val freshExp2 = AstVisitor.visitExpressionWith(freshExprVisitor, exp2)
          Some(Let(newSym,freshExp1,freshExp2,tpe,loc))
        case LetRec(sym, exp1, exp2, tpe, loc) =>
          val newSym = Symbol.freshVarSym("inlnrFresh")
          m += (sym -> newSym)
          val freshExp1 = AstVisitor.visitExpressionWith(freshExprVisitor, exp1)
          val freshExp2 = AstVisitor.visitExpressionWith(freshExprVisitor, exp2)
          Some(LetRec(newSym,freshExp1,freshExp2,tpe,loc))
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
          case Unit => exp
          case True => exp
          case False => exp
          case Char(_) => exp
          case Float32(_) => exp
          case Float64(_) => exp
          case Int8(_) => exp
          case Int16(_) => exp
          case Int32(_) => exp
          case Int64(_) => exp
          case BigInt(_) => exp
          case Str(lit) => exp
          case Var(_, _, _) => exp
          case Ref(_, _, _) => exp
          case Hook(_,_,_) => exp
          case MkClosureRef(_, _, _, _) => exp
          case NativeField(_, _, _) => exp
          case UserError(_, _) => exp
          case MatchError(_, _) => exp
          case SwitchError(_, _) => exp
          case Lambda(args, body, tpe, loc) =>
            Lambda(args, visit(body), tpe, loc)
          case ApplyRef(sym, args, tpe, loc) =>
            ApplyRef(sym, args.map(visit), tpe, loc)
          case ApplyTail(sym, formals, actuals, tpe, loc) =>
            ApplyTail(sym, formals, actuals.map(visit), tpe, loc)
          case ApplyHook(hook, args, tpe, loc) =>
            ApplyHook(hook, args.map(visit), tpe, loc)
          case Apply(exp1, args, tpe, loc) =>
            Apply(visit(exp1), args.map(visit), tpe, loc)
          case Unary(op, exp1, tpe, loc) =>
            Unary(op, visit(exp1), tpe, loc)
          case Binary(op, exp1, exp2, tpe, loc) =>
            Binary(op, visit(exp1), visit(exp2), tpe, loc)
          case IfThenElse(exp1, exp2, exp3, tpe, loc) =>
            IfThenElse(visit(exp1), visit(exp2), visit(exp3), tpe, loc)
          case Let(sym, exp1, exp2, tpe, loc) =>
            Let(sym, visit(exp1), visit(exp2), tpe, loc)
          case LetRec(sym, exp1, exp2, tpe, loc) =>
            LetRec(sym, visit(exp1), visit(exp2), tpe, loc)
          case Is(sym, tag, exp1, loc) =>
            Is(sym, tag, visit(exp1), loc)
          case Tag(sym, tag, exp1, tpe, loc) =>
            Tag(sym, tag, visit(exp1), tpe, loc)
          case Untag(sym, tag, exp1, tpe, loc) =>
            Untag(sym, tag, visit(exp1), tpe, loc)
          case Index(base, offset, tpe, loc) =>
            Index(visit(base), offset, tpe, loc)
          case Tuple(elms, tpe, loc) =>
            Tuple(elms.map(visit), tpe, loc)
          case Existential(fparam, exp1, loc) =>
            Existential(fparam, visit(exp1), loc)
          case Universal(fparam, exp1, loc) =>
            Universal(fparam, visit(exp1), loc)
          case NativeConstructor(constructor, args, tpe, loc) =>
            NativeConstructor(constructor, args.map(visit), tpe, loc)
          case NativeMethod(method, args, tpe, loc) =>
            NativeMethod(method, args.map(visit), tpe, loc)
          case MkClosure(_, _, _, _) =>
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
  def exprScore(exp: Expression): Int = exprListScore(0,List(exp))

  /**
    * Returns the total score of the list of expressions `exps`.
    */
  @tailrec def exprListScore(acc: Int, expressions: List[Expression]): Int =
  expressions match {
    case ::(head, tl) =>
      head match {
        case _: LoadExpression => exprListScore(acc + 1, tl)
        case _: StoreExpression => exprListScore(acc + 1, tl)
        case Unit => exprListScore(acc, tl)
        case True => exprListScore(acc, tl)
        case False => exprListScore(acc, tl)
        case Char(_) => exprListScore(acc, tl)
        case Float32(_) => exprListScore(acc, tl)
        case Float64(_) => exprListScore(acc, tl)
        case Int8(_) => exprListScore(acc, tl)
        case Int16(_) => exprListScore(acc, tl)
        case Int32(_) => exprListScore(acc, tl)
        case Int64(_) => exprListScore(acc, tl)
        case BigInt(_) => exprListScore(acc, tl)
        case Str(_) => exprListScore(acc, tl)
        case Var(sym, _, _) => exprListScore(acc + 1, tl)
        case Ref(sym, _, _) => exprListScore(acc + 1, tl)
        case Lambda(args, body, _, _) => exprListScore(acc + args.length, body::tl)
        case Hook(hook, _, _) => exprListScore(acc + 1, tl)
        case MkClosure(lambda, freeVars, _, _) => exprListScore(acc + 1 + freeVars.length, lambda::tl)
        case MkClosureRef(ref, freeVars, _, _) => exprListScore(acc + 1 + freeVars.length, ref::tl)
        case ApplyRef(_, args, _, _) => exprListScore(acc + 2, args:::tl)
        case ApplyTail(_, formals, actuals, _, _) =>
          // not be inlined
          exprListScore(MaxScore + acc + 2 + formals.length, actuals:::tl)
        case ApplyHook(_, args, _, _) => exprListScore(acc + 2, args:::tl)
        case Apply(exp, args, _, _) => exprListScore(acc + 1, args:::exp::tl)
        case Unary(_, exp, _, _) => exprListScore(acc + 1, exp::tl)
        case Binary(_, exp1, exp2, _, _) => exprListScore(acc + 1, exp1::exp2::tl)
        case IfThenElse(exp1, exp2, exp3, _, _) =>
          val score2 = exprScore(exp2)
          val score3 = exprScore(exp3)
          exprListScore(acc + 2 * (score2 + score3), exp1::tl)
        case Let(_, exp1, exp2, _, _) => exprListScore(acc + 2, exp1::exp2::tl)
        case LetRec(_, exp1, exp2, _, _) => exprListScore(acc + 2, exp1::exp2::tl)
        case Is(_, _, exp, _) => exprListScore(acc + 3, exp::tl)
        case Tag(_, _, exp, _, _) => exprListScore(acc + 3, exp::tl)
        case Untag(_, _, exp, _, _) => exprListScore(acc + 3, exp::tl)
        case Index(base, _, _, _) => exprListScore(acc + 2, base::tl)
        case Tuple(elms, _, _) => exprListScore(acc + 1, elms:::tl)
        case Existential(_, exp, _) => exprListScore(acc + 1, exp::tl)
        case Universal(_, exp, _) => exprListScore(acc + 1, exp::tl)
        case NativeConstructor(_, args, _, _) => exprListScore(acc + 1, args:::tl)
        case NativeField(_, _, _) => exprListScore(acc + 1, tl)
        case NativeMethod(_, args, _, _) => exprListScore(acc + 1, args:::tl)
        case UserError(_, _) => exprListScore(acc,tl)
        case MatchError(_, _) => exprListScore(acc,tl)
        case SwitchError(_, _) => exprListScore(acc,tl)
      }
    case Nil => acc
  }

}
