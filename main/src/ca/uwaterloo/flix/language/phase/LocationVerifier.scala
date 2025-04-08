/*
 * Copyright 2025 Chenhao Gao
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
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Pattern, RestrictableChoosePattern}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.LocationError
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.IterableHasAsScala

/**
  * The LocationVerifier verifies the locations of the Flix program.
  *
  * We currently enforce three invariants:
  *   - The location of the child node must be contained in the location of its parent node.
  *   - The locations of the nodes must be in appearance order.
  *   - The parent node and its last child node must have the same ending.
  */
object LocationVerifier {
  def run(root: TypedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (TypedAst.Root, List[LocationError]) = flix.phaseNew("LocationVerifier") {
    implicit val sctx: SharedContext = SharedContext.mk()
    val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_)(visitDef))
    (root.copy(defs = defs), sctx.errors.asScala.toList)
  }

  private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext): TypedAst.Def = {
    visitExp(defn.exp)
    defn
  }


  private def visitExp(exp0: Expr)(implicit sctx: SharedContext): Unit = exp0 match {
    case Expr.Cst(_, tpe, _) =>
      visitType(tpe)

    case Expr.Var(_, tpe, _) =>
      visitType(tpe)

    case Expr.Hole(_, _, tpe, eff, _) =>
      visitType(tpe)
      visitType(eff)

    case Expr.HoleWithExp(exp, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.OpenAs(_, exp, tpe, _) =>
      visitExp(exp)
      visitType(tpe)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, tpe, _) =>
      visitExp(exp)
      visitType(tpe)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ApplyDef(_, exps, itpe, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(itpe)
      visitType(tpe)
      visitType(eff)

    case Expr.ApplyLocalDef(_, exps, arrowTpe, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(arrowTpe)
      visitType(tpe)
      visitType(eff)

    case Expr.ApplySig(_, exps, itpe, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(itpe)
      visitType(tpe)
      visitType(eff)

    case Expr.Unary(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Binary(_, exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Let(bnd, exp1, exp2, tpe, eff, _) =>
      visitBinder(bnd)
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, _) =>
      visitBinder(bnd)
      fparams.foreach(visitFormalParam)
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Region(tpe, _) =>
      visitType(tpe)

    case Expr.Scope(bnd, _, exp, tpe, eff, _) =>
      visitBinder(bnd)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      verifyAppearanceOrder(List(exp1.loc, exp2.loc, exp3.loc))
      verifyParentContainment(loc, List(exp1.loc, exp2.loc, exp3.loc))
      verifySameEnding(loc, exp3.loc)
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(tpe)
      visitType(eff)

    case Expr.Stm(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Discard(exp, eff, _) =>
      visitExp(exp)
      visitType(eff)

    case Expr.Match(exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitMatchRule)
      visitType(tpe)
      visitType(eff)

    case Expr.TypeMatch(exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitTypeMatchRule)
      visitType(tpe)
      visitType(eff)

    case Expr.RestrictableChoose(_, exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitRestrictableChooseRule)
      visitType(tpe)
      visitType(eff)

    case Expr.Tag(_, exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.RestrictableTag(_, exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.Tuple(exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordSelect(exp, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordExtend(_, exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordRestrict(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLit(exps, exp, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLength(exp, eff, _) =>
      visitExp(exp)
      visitType(eff)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(eff)

    case Expr.StructNew(_, fields, region, tpe, eff, _) =>
      fields.foreach{ field =>
        visitExp(field._2)
      }
      visitExp(region)
      visitType(tpe)
      visitType(eff)

    case Expr.StructGet(exp, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.StructPut(exp1, _, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLit(exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, _) =>
      visitExp(exp)
      declaredType.toList.foreach(visitType)
      declaredEff.toList.foreach(visitType)
      visitType(tpe)
      visitType(eff)

    case Expr.Unsafe(exp, runEff, tpe, eff, _) =>
      visitExp(exp)
      visitType(runEff)
      visitType(tpe)
      visitType(eff)

    case Expr.Without(exp, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.TryCatch(exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitCatchRule)
      visitType(tpe)
      visitType(eff)

    case Expr.Throw(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Handler(_, rules, bodyType, bodyEff, handledEff, tpe, _) =>
      rules.foreach(visitHandlerRule)
      visitType(bodyType)
      visitType(bodyEff)
      visitType(handledEff)
      visitType(tpe)

    case Expr.RunWith(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Do(_, exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.InvokeConstructor(_, exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.InvokeMethod(_, exp, exps, tpe, eff, _) =>
      visitExp(exp)
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.InvokeStaticMethod(_, exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.GetField(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.PutField(_, exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.GetStaticField(_, tpe, eff, _) =>
      visitType(tpe)
      visitType(eff)

    case Expr.PutStaticField(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.NewObject(_, _, tpe, eff, methods, _) =>
      visitType(tpe)
      visitType(eff)
      methods.foreach(visitJvmMethod)

    case Expr.NewChannel(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.PutChannel(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.SelectChannel(rules, default, tpe, eff, _) =>
      rules.foreach(visitSelectChannelRule)
      default.toList.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.Spawn(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ParYield(frags, exp, tpe, eff, _) =>
      frags.foreach(visitParYieldFragment)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Lazy(exp, tpe, _) =>
      visitExp(exp)
      visitType(tpe)

    case Expr.Force(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointConstraintSet(cs, tpe, _) =>
      cs.foreach(visitConstrait)
      visitType(tpe)

    case Expr.FixpointLambda(pparams, exp, tpe, eff, _) =>
      pparams.foreach(visitPParam)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointSolve(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointFilter(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointInject(exp, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointProject(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Error(_, tpe, eff) =>
      visitType(tpe)
      visitType(eff)

    case Expr.GetChannel(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)
  }

  private def visitType(tpe: Type)(implicit sctx: SharedContext): Unit = tpe match {
    case Type.Apply(tpe1, tpe2, _) =>
      visitType(tpe1)
      visitType(tpe2)
    case Type.Alias(_, args, _, _) =>
      args.foreach(visitType)
    case Type.AssocType(_, arg, _, _) => visitType(arg)
    case Type.JvmToType(t, _) => visitType(t)
    case Type.JvmToEff(t, _) => visitType(t)
    case Type.UnresolvedJvmType(_, _) => ()
    case Type.Cst(TypeConstructor.Enum(_, _), _) => ()
    case Type.Cst(TypeConstructor.Struct(_, _), _) => ()
    case Type.Cst(_, _) => ()
    case Type.Var(_, _) => ()
  }

  private def visitBinder(bnd: TypedAst.Binder)(implicit sctx: SharedContext): Unit =
    visitType(bnd.tpe)

  private def visitPParam(pparam: TypedAst.PredicateParam)(implicit sctx: SharedContext): Unit =
    visitType(pparam.tpe)

  private def visitHead(head: TypedAst.Predicate.Head)(implicit sctx: SharedContext): Unit = head match {
    case TypedAst.Predicate.Head.Atom(_, _, terms, tpe, _) => visitType(tpe)
      terms.foreach(visitExp)
  }

  private def visitContraintParam(cp: TypedAst.ConstraintParam)(implicit sctx: SharedContext): Unit = {
    visitType(cp.tpe)
    visitBinder(cp.bnd)
  }

  private def visitConstraintBody(cb: Body)(implicit sctx: SharedContext): Unit = cb match {
    case Body.Atom(_, _, _, _, terms, tpe, _) =>
      terms.foreach(visitPattern)
      visitType(tpe)
    case Body.Functional(outBnds, exp, _) =>
      outBnds.foreach(visitBinder)
      visitExp(exp)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  private def visitSelectChannelRule(rule: TypedAst.SelectChannelRule)(implicit sctx: SharedContext): Unit = {
    visitBinder(rule.bnd)
    visitExp(rule.chan)
    visitExp(rule.exp)
  }

  private def visitParYieldFragment(fragment: TypedAst.ParYieldFragment)(implicit sctx: SharedContext): Unit = {
    visitExp(fragment.exp)
    visitPattern(fragment.pat)
  }

  private def visitConstrait(constraint: TypedAst.Constraint)(implicit sctx: SharedContext): Unit = {
    visitHead(constraint.head)
    constraint.cparams.foreach(visitContraintParam)
    constraint.body.foreach(visitConstraintBody)
  }

  private def visitJvmMethod(method: TypedAst.JvmMethod)(implicit sctx: SharedContext): Unit = {
    visitType(method.retTpe)
    visitType(method.eff)
    visitExp(method.exp)
    method.fparams.foreach(visitFormalParam)
  }

  private def visitHandlerRule(handlerRule: TypedAst.HandlerRule)(implicit sctx: SharedContext): Unit = {
    visitExp(handlerRule.exp)
    handlerRule.fparams.foreach(visitFormalParam)
  }

  private def visitCatchRule(catchRule: TypedAst.CatchRule)(implicit sctx: SharedContext): Unit = {
    visitBinder(catchRule.bnd)
    visitExp(catchRule.exp)
  }

  private def visitVarOrWild(pattern: TypedAst.RestrictableChoosePattern.VarOrWild)(implicit sctx: SharedContext): Unit = pattern match {
    case RestrictableChoosePattern.Var(bnd, tpe, _) =>
      visitBinder(bnd)
      visitType(tpe)
    case RestrictableChoosePattern.Wild(tpe, _) =>
      visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, _) =>
      visitType(tpe)
  }

  private def visitRestrictableChoosePattern(pattern: TypedAst.RestrictableChoosePattern)(implicit sctx: SharedContext): Unit = pattern match {
    case RestrictableChoosePattern.Tag(_, pats, tpe, _) =>
      pats.foreach(visitVarOrWild)
      visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, _) =>
      visitType(tpe)
  }

  private def visitRestrictableChooseRule(rule: TypedAst.RestrictableChooseRule)(implicit sctx: SharedContext): Unit = {
    visitExp(rule.exp)
    visitRestrictableChoosePattern(rule.pat)
  }

  private def visitTypeMatchRule(matchRule: TypedAst.TypeMatchRule)(implicit sctx: SharedContext): Unit = {
    visitBinder(matchRule.bnd)
    visitType(matchRule.tpe)
    visitExp(matchRule.exp)
  }

  private def visitRecordLabelPattern(pattern: Record.RecordLabelPattern)(implicit sctx: SharedContext): Unit = {
    visitPattern(pattern.pat)
    visitType(pattern.tpe)
  }

  private def visitPattern(p: TypedAst.Pattern)(implicit sctx: SharedContext): Unit = p match {
    case Pattern.Var(bnd, tpe, _) =>
      visitBinder(bnd)
      visitType(tpe)
    case Pattern.Tag(_, pats, tpe, _) =>
      pats.foreach(visitPattern)
      visitType(tpe)
    case Pattern.Tuple(pats, tpe, _) =>
      pats.foreach(visitPattern)
      visitType(tpe)
    case Pattern.Record(pats, pat, tpe, _) =>
      pats.foreach(visitRecordLabelPattern)
      visitPattern(pat)
      visitType(tpe)
    case pat =>
      visitType(pat.tpe)
  }

  private def visitMatchRule(r: TypedAst.MatchRule)(implicit sctx: SharedContext): Unit = {
    visitPattern(r.pat)
    visitExp(r.exp)
    r.guard.toList.foreach(visitExp)
  }

  private def visitFormalParam(fparam: TypedAst.FormalParam)(implicit sctx: SharedContext): Unit = {
    visitBinder(fparam.bnd)
    visitType(fparam.tpe)
  }


  /**
    * Verifies that the parent location contains all the children locations.
    *
    * @param parentLoc        the location of the parent node.
    * @param childrenLocation the locations of the child nodes.
    */
  private def verifyParentContainment(parentLoc: SourceLocation, childrenLocation: List[SourceLocation])(implicit sctx: SharedContext): Unit =
    childrenLocation.foreach { loc =>
      if (!parentLoc.contains(loc)) {
        sctx.errors.add(LocationError.ChildOutOfBoundError(parentLoc, loc))
      }
    }

  /**
    * Verifies that the locations are in appearance order.
    *
    * @param locs the list of locations to verify, in the order of appearance.
    */
  private def verifyAppearanceOrder(locs: List[SourceLocation])(implicit sctx: SharedContext): Unit = {
    locs.sliding(2).foreach {
      case List(prevLoc, currLoc) =>
        if (!prevLoc.isBefore(currLoc)) {
          sctx.errors.add(LocationError.AppearanceOrderError(prevLoc, currLoc))
        }
      case _ => ()
    }
  }

  /**
    * Verifies that the parent location and the child location have the same ending.
    *
    * @param parentLoc the location of the parent node.
    * @param loc       the location of the child node.
    */
  private def verifySameEnding(parentLoc: SourceLocation, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    if (parentLoc.sp2 != loc.sp2) {
      sctx.errors.add(LocationError.DifferentEndingError(parentLoc, loc))
    }
  }

  /**
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {

    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the [[LocationError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[LocationError])
}

