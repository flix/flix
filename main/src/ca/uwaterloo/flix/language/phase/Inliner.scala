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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol, Type}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition.Constant
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.util.Validation
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
    * The maximum score of a function for it to be eligible for inlining.
    */
  val MaxScore = 25

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    val defnSizes = root.definitions.mapValues(c => (c, score(c)) )

    val newDefns =
    root.definitions.mapValues(c => Constant(c.ann, c.sym, c.formals, inlineExpr(defnSizes, c.exp), c.isSynthetic, c.tpe, c.loc))

    SimplifiedAst.Root(newDefns, root.enums, root.lattices, root.tables, root.indexes, root.strata, root.properties, root.reachable, root.time).toSuccess

  }

  /**
    * A param is considered safe to use for inlining if it is a variable or a literal
    */

  def isSafeParam(param: Expression): Boolean =
  param match {
    case Unit => true
    case True => true
    case False => true
    case Char(_) => true
    case Float32(_) => true
    case Float64(_) => true
    case Int8(_) => true
    case Int16(_) => true
    case Int32(_) => true
    case Int64(_) => true
    case BigInt(_) => true
    case Str(_) => true
    case Var(_, _, _) => true
    case _ => false
  }

  /**
    * Performs the substitutions from `sub` into `exp`
    */
  def doSubst(sub: Map[(Symbol.VarSym, Type), Expression], exp: Expression): Expression =
  {
    def f(e: Expression): Option[Expression] =
    e match {
      case Var(sym, tpe, _) =>
        sub.get(sym, tpe)
      case _ => None
    }
    traverseExpr(f, exp)
  }

  /**
    * Either performs some high-level inlining using `defnSizes`
    * or returns None to ask traversal to inline deeper
    */
  def inlineOpt(defnSizes:  Map[Symbol.DefnSym, (Constant, Int)], exp: SimplifiedAst): Option[Expression] =
  {
    exp match {
      case ApplyRef(sym, args, tpe, loc) =>
        if (args.forall(isSafeParam)) {
          defnSizes.get(sym) match {
            case None => None
            case Some((Constant(_, _, formals, exp1, _, _, _), size)) =>
              if (size <= MaxScore){
                def formalMap = formals.map(f => (f.sym,f.tpe)).zip(args).toMap
                Some(doSubst(formalMap,exp1))
              }
              else {
                None
              }
          }
        } else {
          None
        }
      case Let(sym, exp1, exp2, tpe, loc) =>
        if (isSafeParam(exp1)){
          val varMap = (((sym,exp1.tpe), exp1)::Nil).toMap
          val cleanExp = doSubst(varMap,exp2)
          // this is currently n^2 for deeply nested bindings, but type inference is exp(n)
          Some(inlineExpr(defnSizes, cleanExp))
        } else {
          None
        }
      case _ => None
    }
  }

  /**
    * Traverses through `exp` and performs inlining using
    * definitions and sizes from `defnSizes`
    */
  def inlineExpr(defnSizes: Map[Symbol.DefnSym, (Constant, Int)], exp: Expression): Expression =
  {
    traverseExpr(inlineOpt(defnSizes,_), exp)
  }

  /**
    * Traversal function for SimplifiedAst
    */
  def traverseExpr(f: Expression => Option[Expression], exp: Expression): Expression =
  {
    def trav(exp1: Expression): Expression = traverseExpr(f,exp1)
    f(exp) match {
      case None =>
        exp match {
          case Lambda(args, body, tpe, loc) =>
            Lambda(args, trav(body), tpe, loc)
          case MkClosure(lambda, freeVars, tpe, loc) =>
            MkClosure(Lambda(lambda.args, trav(lambda.body), lambda.tpe, lambda.loc), freeVars, tpe, loc)
          case ApplyRef(sym, args, tpe, loc) =>
            ApplyRef(sym, args.map(trav), tpe, loc)
          case ApplyTail(sym, formals, actuals, tpe, loc) =>
            ApplyTail(sym, formals, actuals.map(trav), tpe, loc)
          case ApplyHook(hook, args, tpe, loc) =>
            ApplyHook(hook, args.map(trav), tpe, loc)
          case Apply(exp1, args, tpe, loc) =>
            Apply(exp1, args.map(trav), tpe, loc)
          case Unary(op, exp1, tpe, loc) =>
            Unary(op, trav(exp1), tpe, loc)
          case Binary(op, exp1, exp2, tpe, loc) =>
            Binary(op, trav(exp1), trav(exp2), tpe, loc)
          case IfThenElse(exp1, exp2, exp3, tpe, loc) =>
            IfThenElse(trav(exp1), trav(exp2), trav(exp3), tpe, loc)
          case Let(sym, exp1, exp2, tpe, loc) =>
            Let(sym, trav(exp1), trav(exp2), tpe, loc)
          case Is(exp1, tag, loc) =>
            Is(trav(exp1), tag, loc)
          case Tag(sym, tag, exp1, tpe, loc) =>
            Tag(sym, tag, trav(exp1), tpe, loc)
          case Untag(tag, exp1, tpe, loc) =>
            Untag(tag, trav(exp1), tpe, loc)
          case Index(base, offset, tpe, loc) =>
            Index(trav(base), offset, tpe, loc)
          case Tuple(elms, tpe, loc) =>
            Tuple(elms.map(trav), tpe, loc)
          case Existential(fparam, exp1, loc) =>
            Existential(fparam, trav(exp1), loc)
          case Universal(fparam, exp1, loc) =>
            Universal(fparam, trav(exp1), loc)
          case NativeConstructor(constructor, args, tpe, loc) =>
            NativeConstructor(constructor, args.map(trav), tpe, loc)
          case NativeMethod(method, args, tpe, loc) =>
            NativeMethod(method, args.map(trav), tpe, loc)
          case _ => exp
        }
      case Some(exp1) => exp1
    }
  }


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
  def score(defn: Constant): Int = exprScore(defn.exp)

/**
  * Returns the score of the given expression `exp`.
  */
  def exprScore(exp: Expression): Int = exprListScore(0,List(exp))

  /**
    * Returns the total score of the list of expressions `exps`.
    */
  @tailrec def exprListScore(acc: Int, exps: List[Expression]): Int =
  exps match {
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
        case ApplyTail(_, formals, actuals, _, _) => exprListScore(acc + 2 + formals.length, actuals:::tl)
        case ApplyHook(_, args, _, _) => exprListScore(acc + 2, args:::tl)
        case Apply(exp, args, _, _) => exprListScore(acc + 1, args:::exp::tl)
        case Unary(_, exp, _, _) => exprListScore(acc + 1, exp::tl)
        case Binary(_, exp1, exp2, _, _) => exprListScore(acc + 1, exp1::exp2::tl)
        case IfThenElse(exp1, exp2, exp3, _, _) =>
          val score2 = exprScore(exp2)
          val score3 = exprScore(exp3)
          exprListScore(acc + 2 * (score2 + score3), exp1::tl)
        case Let(_, exp1, exp2, _, _) => exprListScore(acc + 2, exp1::exp2::tl)
        case Is(exp, _, _) => exprListScore(acc + 2, exp::tl)
        case Tag(_, _, exp, _, _) => exprListScore(acc + 3, exp::tl)
        case Untag(_, exp, _, _) => exprListScore(acc + 2, exp::tl)
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
