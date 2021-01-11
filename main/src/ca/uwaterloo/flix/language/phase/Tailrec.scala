/*
 * Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Ast.{Modifier, Modifiers}
import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast.Symbol.DefnSym
import ca.uwaterloo.flix.language.ast.ops.LiftedAstOps
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Optimization, Validation}

import scala.collection.mutable

/**
  * The Tailrec phase identifies function calls that are in tail recursive position.
  *
  * Specifically, it replaces `ApplyRef` AST nodes with `ApplyTail` AST nodes
  * when the `ApplyRef` node calls the same function and occurs in tail position.
  */
object Tailrec extends Phase[Root, Root] {

  /**
    * Identifies tail recursive calls in the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Tailrec") {
    //println("__________________Before TailRec____________________________________")
    //ln(PrettyPrinter.Lifted.fmtRoot(root).fmt(TerminalContext.AnsiTerminal))
    //
    // Check if tail call elimination is enabled.
    //
    if (!(flix.options.optimizations contains Optimization.TailCalls))
      return root.toSuccess

    val helperDefsMap: mutable.Map[DefnSym, Def] = mutable.Map.empty
    //
    // Rewrite tail calls and identify TRMC
    //
    val defns = root.defs.map {
      case (sym, defn) => sym -> tailrec(defn, helperDefsMap)
    }

    val helperdefns = helperDefsMap.map {
      case (sym, defn) => defn.sym -> makeTRMCHelper(defn, sym)
    }

    val newRoot = root.copy(defs = defns.concat(helperdefns))
    //println("__________________After TailRec____________________________________")
    //println(PrettyPrinter.Lifted.fmtRoot(newRoot).fmt(TerminalContext.AnsiTerminal))
    newRoot.toSuccess
  }

  def makeTRMCHelper(defn: Def, originalDefnSym: DefnSym)(implicit flix: Flix): Def = {
    val refreshedDef = LiftedAstOps.refreshVarNames(defn)

    // Todo: would this ever result in an error? Only when there is trmc in a function without parameters
    def endType = defn.tpe.arrowResultType

    val endParam = FormalParam(Symbol.freshVarSym("end"), Modifiers(List(Modifier.Synthetic)),
      endType,
      SourceLocation.Generated)
    val helperParams = refreshedDef.fparams.appended(endParam)

    // Generate the type for the new function
    val arrowArity = refreshedDef.tpe.typeConstructor match {
      case Some(TypeConstructor.Arrow(arity)) => arity
      case t => throw InternalCompilerException(s"Option of function type expected, but got: '$t'.")
    }
    val targs = refreshedDef.tpe.typeArguments
    val newType = Type.mkApply(Type.Cst(TypeConstructor.Arrow(arrowArity + 1), refreshedDef.loc), targs ::: Type.Unit :: Nil)

    /**
      * Very similar to that of tailrec.
      *
      * We need to find all tail Nil and tail x :: f
      * Also replaces every `ApplyRef`, which calls the same function and occurs in tail position, with `ApplyTail`.
      */
    def visit(exp0: Expression): Expression = exp0 match {
      // TODO: For all tails that don't go deeper: They should be set as the tail of end.
      /*
      * Let: The body expression is in tail position.
      * (The value expression is *not* in tail position).
      */
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e2 = visit(exp2)
        Expression.Let(sym, exp1, e2, Type.Unit, loc)

      /*
       * If-Then-Else: Consequent and alternative are both in tail position.
       * (The condition is *not* in tail position).
       */
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        Expression.IfThenElse(exp1, e2, e3, Type.Unit, loc)

      /*
       * Branch: Each branch is in tail position.
       */
      case Expression.Branch(e0, br0, tpe, loc) =>
        val br = br0 map {
          case (sym, exp) => sym -> visit(exp)
        }
        Expression.Branch(e0, br, Type.Unit, loc)

      /*
       * ApplyClo.
       */
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        Expression.ApplyCloTail(exp, args, tpe, loc)

      /*
       * ApplyDef.
       */
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        // Check whether this is a self recursive call.
        if (originalDefnSym != sym) {
          // Case 1: Tail recursive call.
          Expression.ApplyDefTail(sym, args, tpe, loc)
        } else {
          // Case 2: Self recursive call.
          // We need to make sure that the extra helper parameter is always passed down
          val endParamExp = Expression.Var(endParam.sym, endParam.tpe, loc)
          Expression.ApplySelfTail(sym, refreshedDef.fparams, args.appended(endParamExp), Type.Unit, loc)
        }

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) => SelectChannelRule(sym, chan, visit(exp))
        }

        val d = default.map(exp => visit(exp))

        Expression.SelectChannel(rs, d, tpe, loc)

      // Match Nil
      case Expression.Tag(tagSym, Name.Tag("Nil", nilLoc), Expression.Unit, tagTpe, tagLoc) =>
        Expression.Unit
      //Expression.Var(endParam.sym, tagTpe, tagLoc)

      // Match a Cons Tag with 2 elements, the second being a self-recursive call
      case Expression.Tag(sym, Name.Tag("Cons", _),
      Expression.Tuple(hd :: Expression.ApplyDef(funSym, args, tpeDef, _) :: Nil, tpeTup, locTup), tagTpe, tagLoc) =>
        // Bail if not the function of the original definition
        if (funSym != originalDefnSym) return exp0

        //println("Found a cons in helper")
        val flixNil = Expression.Tag(sym, Name.Tag("Nil", tagLoc), Expression.Unit, tagTpe, tagLoc)
        val consArg = Expression.Tag(sym, Name.Tag("Cons", tagLoc),
          Expression.Tuple(hd :: flixNil :: Nil, tpeTup, tagLoc)
          , tagTpe, tagLoc)

        val newTailSym = Symbol.freshVarSym("new_tail")
        val newTailExp = Expression.Var(newTailSym, tagTpe, tagLoc)

        val endParamExp = Expression.Var(endParam.sym, endParam.tpe, tagLoc)
        val endParamExpUntagged = Expression.Untag(sym, Name.Tag("Cons", tagLoc), endParamExp, tpeTup, tagLoc)
        val setNewTail = Expression.IndexMut(endParamExpUntagged, 1, newTailExp, Type.Unit, tagLoc)

        val applySelfExp = Expression.ApplySelfTail(refreshedDef.sym, helperParams, args ::: List(newTailExp), Type.Unit, tagLoc)

        val letExp = Expression.Let(Symbol.freshVarSym(), setNewTail, applySelfExp, Type.Unit, tagLoc)
        Expression.Let(newTailSym, consArg, letExp, Type.Unit, tagLoc)

      /*
       * Other expression: No calls in tail position.
       */
      case _ => exp0
    }

    /*val vt = new VirtualTerminal()
    PrettyPrinter.Lifted.fmtDef(helperDef, vt)
    println("__________________Helper____________________________________")
    println(vt.fmt(TerminalContext.AnsiTerminal))*/
    val newExp = visit(refreshedDef.exp)
    refreshedDef.copy(fparams = helperParams, exp = newExp, tpe = newType)
  }

  /**
    * Identifies tail recursive calls in the given definition `defn`.
    */
  private def tailrec(defn: Def, helperMap: mutable.Map[DefnSym, Def])(implicit flix: Flix): Def = {
    /**
      * Introduces tail recursive calls in the given expression `exp0`.
      *
      * Replaces every `ApplyRef`, which calls the same function and occurs in tail position, with `ApplyTail`.
      */
    def visit(exp0: Expression): Expression = exp0 match {
      /*
       * Let: The body expression is in tail position.
       * (The value expression is *not* in tail position).
       */
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e2 = visit(exp2)
        Expression.Let(sym, exp1, e2, tpe, loc)

      /*
       * If-Then-Else: Consequent and alternative are both in tail position.
       * (The condition is *not* in tail position).
       */
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        Expression.IfThenElse(exp1, e2, e3, tpe, loc)

      /*
       * Branch: Each branch is in tail position.
       */
      case Expression.Branch(e0, br0, tpe, loc) =>
        val br = br0 map {
          case (sym, exp) => sym -> visit(exp)
        }
        Expression.Branch(e0, br, tpe, loc)

      /*
       * ApplyClo.
       */
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        Expression.ApplyCloTail(exp, args, tpe, loc)

      /*
       * ApplyDef.
       */
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        // Check whether this is a self recursive call.
        if (defn.sym != sym) {
          // Case 1: Tail recursive call.
          Expression.ApplyDefTail(sym, args, tpe, loc)
        } else {
          // Case 2: Self recursive call.
          Expression.ApplySelfTail(sym, defn.fparams, args, tpe, loc)
        }

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) => SelectChannelRule(sym, chan, visit(exp))
        }

        val d = default.map(exp => visit(exp))

        Expression.SelectChannel(rs, d, tpe, loc)

      // Match a Cons Tag with 2 elements, the second being a self-recursive call
      case Expression.Tag(sym, Name.Tag("Cons", _),
      Expression.Tuple(hd :: Expression.ApplyDef(defn.sym, args, tpeDef, _) :: Nil, tpeTup, locTup), tpeTag, locTag) =>
        // First check that `exp` is a Tuple

        //println("Found a cons")
        // Now we check if the helper already exists in the map, otherwise we insert it
        val funDefnSym = defn.sym
        val helperSym = if (helperMap.contains(funDefnSym)) {
          // Just get the symbol of the existing helper
          helperMap(funDefnSym).sym
        } else {
          // Add the new helper to the map
          val newSym = Symbol.freshDefnSym(funDefnSym.namespace, funDefnSym.text + "helper")
          helperMap.addOne(defn.sym, defn.copy(sym = newSym))

          newSym
        }


        val flixNil = Expression.Tag(sym, Name.Tag("Nil", locTag), Expression.Unit, tpeTag, locTag)
        val retVarSym = Symbol.freshVarSym("ret")
        val retVarExp = Expression.Var(retVarSym, tpeTag, locTag)
        val consArg = Expression.Tag(sym, Name.Tag("Cons", locTag),
          Expression.Tuple(hd :: flixNil :: Nil, tpeTup, locTag)
          , tpeTag, locTag)


        val applyHelperExp = Expression.ApplyDef(helperSym, args ::: List(retVarExp), Type.Unit, locTag)
        val retLetExp = Expression.Let(Symbol.freshVarSym(), applyHelperExp, retVarExp, tpeTag, locTag)
        Expression.Let(retVarSym, consArg, retLetExp, tpeTag, locTag)
      /*
       * Other expression: No calls in tail position.
       */
      case _ => exp0
    }

    defn.copy(exp = visit(defn.exp))
  }

}
