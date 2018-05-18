/*
 * Copyright 2015-2016 Ming-Ho Yee
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
import ca.uwaterloo.flix.language.ast.{Ast, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

object LambdaLift extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, SimplifiedAst.Def]

  /**
    * Performs lambda lifting on all definitions in the AST.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val t = System.nanoTime()

    // A mutable map to hold lambdas that are lifted to the top level.
    val m: TopLevel = mutable.Map.empty

    val definitions = root.defs.map {
      case (name, decl) => name -> lift(decl, m)
    }
    val handlers = root.handlers.map {
      case (k, v) => k -> lift(v, m)
    }
    val properties = root.properties.map(p => lift(p, m))

    // Return the updated AST root.
    val e = System.nanoTime() - t
    root.copy(defs = definitions ++ m, handlers = handlers, properties = properties, time = root.time.copy(lambdaLift = e)).toSuccess
  }

  /**
    * Performs lambda lifting on the given declaration `decl`.
    *
    * The definition's expression is closure converted, and then lifted definitions are added to the mutable map `m`.
    * The updated definition is then returned.
    */
  private def lift(decl: SimplifiedAst.Def, m: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Def = {
    val convExp = ClosureConv.convert(decl.exp)
    val liftExp = lift(convExp, m, Some(decl.sym))
    decl.copy(exp = liftExp)
  }

  /**
    * Performs lambda lifting on the given handler `handler`.
    *
    * The handler's expression is closure converted, and then the lifted definitions are added to the mutable map `m`.
    * The updated definition is then returned.
    */
  private def lift(handler: SimplifiedAst.Handler, m: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Handler = {
    val convExp = ClosureConv.convert(handler.exp)
    val liftExp = lift(convExp, m, None)
    handler.copy(exp = liftExp)
  }

  /**
    * Performs lambda lifting on the given property `prop`.
    *
    * The property's expression is closure converted, and then the lifted definitions are added to the mutable map `m`.
    * The updated definition is then returned.
    */
  private def lift(prop: SimplifiedAst.Property, m: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Property = {
    val convExp = ClosureConv.convert(prop.exp)
    val liftExp = lift(convExp, m, None)
    prop.copy(exp = liftExp)
  }

  /**
    * Performs lambda lifting on the given expression `exp0`.
    *
    * Adds new top-level definitions to the mutable map `m`.
    */
  private def lift(exp0: Expression, m: TopLevel, symOpt: Option[Symbol.DefnSym])(implicit genSym: GenSym): Expression = {
    def visit(e: Expression): Expression = e match {
      case Expression.Unit => e
      case Expression.True => e
      case Expression.False => e
      case Expression.Char(lit) => e
      case Expression.Float32(lit) => e
      case Expression.Float64(lit) => e
      case Expression.Int8(lit) => e
      case Expression.Int16(lit) => e
      case Expression.Int32(lit) => e
      case Expression.Int64(lit) => e
      case Expression.BigInt(lit) => e
      case Expression.Str(lit) => e
      case Expression.Var(sym, tpe, loc) => e
      case Expression.Def(sym, tpe, loc) => e
      case Expression.Eff(sym, tpe, loc) => e

      case Expression.Lambda(fparams, body, tpe, loc) =>
        // Lift the lambda to a top-level definition, and replacing the Lambda expression with a Ref.

        // First, recursively visit the lambda body, lifting any inner lambdas.
        val liftedBody = visit(body)

        // Generate a fresh symbol for the definition.
        val freshSymbol = symOpt match {
          case None => Symbol.freshDefnSym("none") // TODO: This seems suspicious.
          case Some(oldSym) => Symbol.freshDefnSym(oldSym)
        }

        // Create a new top-level definition, using the fresh name and lifted body.
        val ann = Ast.Annotations.Empty
        val mod = Ast.Modifiers(Ast.Modifier.Synthetic :: Nil)
        val defn = SimplifiedAst.Def(ann, mod, freshSymbol, fparams, liftedBody, tpe, loc)

        // Update the map that holds newly-generated definitions
        m += (freshSymbol -> defn)

        // Finally, replace this current Lambda node with a Ref to the newly-generated name.
        SimplifiedAst.Expression.Def(freshSymbol, tpe, loc)

      case Expression.Closure(ref, freeVars, tpe, loc) => e

      case Expression.Apply(exp, args, tpe, loc) =>
        Expression.Apply(visit(exp), args.map(visit), tpe, loc)

      case SimplifiedAst.Expression.LambdaClosure(lambda, freeVars, tpe, loc) =>
        // Replace a Def expression with a Closure expression.
        visit(lambda) match {
          case defn: SimplifiedAst.Expression.Def =>
            SimplifiedAst.Expression.Closure(defn.sym, freeVars, tpe, loc)
          case _ => throw InternalCompilerException(s"Unexpected expression: '$lambda'.")
        }

      case Expression.ApplyClo(exp, args, tpe, loc) =>
        Expression.ApplyClo(visit(exp), args.map(visit), tpe, loc)
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        Expression.ApplyDef(sym, args.map(visit), tpe, loc)
      case Expression.ApplyEff(sym, args, tpe, loc) =>
        Expression.ApplyEff(sym, args.map(visit), tpe, loc)
      case Expression.Unary(sop, op, exp, tpe, loc) =>
        Expression.Unary(sop, op, visit(exp), tpe, loc)
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
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        Expression.LetRec(sym, visit(exp1), visit(exp2), tpe, loc)
      case Expression.Is(sym, tag, exp, loc) =>
        Expression.Is(sym, tag, visit(exp), loc)
      case Expression.Tag(enum, tag, exp, tpe, loc) =>
        Expression.Tag(enum, tag, visit(exp), tpe, loc)
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        Expression.Untag(sym, tag, visit(exp), tpe, loc)
      case Expression.Index(exp, offset, tpe, loc) =>
        Expression.Index(visit(exp), offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) =>
        Expression.Tuple(elms.map(visit), tpe, loc)
      case Expression.ArrayLit(elms, tpe, loc) =>
        Expression.ArrayLit(elms.map(visit), tpe, loc)
      case Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visit(elm)
        val ln = visit(len)
        Expression.ArrayNew(e, ln, tpe, loc)
      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        Expression.ArrayLoad(b, i, tpe, loc)
      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val e = visit(elm)
        Expression.ArrayStore(b, i, e, tpe, loc)
      case Expression.ArrayLength(base, tpe, loc) =>
        val b = visit(base)
        Expression.ArrayLength(b, tpe, loc)
      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visit(base)
        val i1 = visit(startIndex)
        val i2 = visit(endIndex)
        Expression.ArraySlice(b, i1, i2, tpe, loc)
      case Expression.Ref(exp, tpe, loc) =>
        Expression.Ref(visit(exp), tpe, loc)
      case Expression.Deref(exp, tpe, loc) =>
        Expression.Deref(visit(exp), tpe, loc)
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        Expression.Assign(visit(exp1), visit(exp2), tpe, loc)
      case Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = visit(exp)
        val bs = bindings map {
          case HandlerBinding(sym, handler) => HandlerBinding(sym, visit(handler))
        }
        Expression.HandleWith(e, bs, tpe, loc)
      case Expression.Existential(params, exp, loc) =>
        Expression.Existential(params, visit(exp), loc)
      case Expression.Universal(params, exp, loc) =>
        Expression.Universal(params, visit(exp), loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args.map(e => visit(e))
        Expression.NativeConstructor(constructor, es, tpe, loc)
      case Expression.NativeField(field, tpe, loc) => e
      case Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args.map(e => visit(e))
        Expression.NativeMethod(method, es, tpe, loc)

      case Expression.UserError(tpe, loc) => e
      case Expression.HoleError(sym, tpe, eff, loc) => e
      case Expression.MatchError(tpe, loc) => e
      case Expression.SwitchError(tpe, loc) => e

      case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.ApplyEffTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    }

    visit(exp0)
  }

}
