/*
 *  Copyright 2017 Magnus Madsen and Jason Mittertreiner
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.shared.LabelledPrecedenceGraph.{Label, LabelledEdge}
import ca.uwaterloo.flix.language.ast.shared.{Denotation, LabelledPrecedenceGraph}
import ca.uwaterloo.flix.language.ast.{ChangeSet, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The [[PredDeps]] class computes the [[LabelledPrecedenceGraph]] of the whole program,
  * which represents all static dependencies between datalog predicates.
  *
  * The computed [[LabelledPrecedenceGraph]] is used in [[Stratifier]].
  */
object PredDeps {

  def run(root: Root, oldRoot: Root, changeSet: ChangeSet)(implicit flix: Flix): (Root, List[CompilationMessage]) = flix.phaseNew("PredDeps") {
    // Compute an over-approximation of the dependency graph for all constraints in the program.
    implicit val sctx: SharedContext = SharedContext.mk()

    val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_)(visitDef))
    val traits = changeSet.updateStaleValues(root.traits, oldRoot.traits)(ParOps.parMapValues(_)(visitTrait))
    val instances = changeSet.updateStaleValueLists(root.instances, oldRoot.instances, (i1: TypedAst.Instance, i2: TypedAst.Instance) => i1.tpe.typeConstructor == i2.tpe.typeConstructor)(ParOps.parMapValueList(_)(visitInstance))

    val g = LabelledPrecedenceGraph(sctx.edges.asScala.toVector)
    (root.copy(defs = defs, traits = traits, instances = instances, precedenceGraph = g), List.empty)
  }

  private def visitDef(defn: Def)(implicit sctx: SharedContext): Def = {
    visitExp(defn.exp)
    defn
  }

  private def visitTrait(trt: Trait)(implicit sctx: SharedContext): Trait = {
    trt.laws.foreach(visitDef)
    trt.sigs.foreach(visitSig)
    trt
  }

  private def visitInstance(inst: Instance)(implicit sctx: SharedContext): Instance = {
    inst.defs.foreach(visitDef)
    inst
  }

  private def visitSig(sig: Sig)(implicit sctx: SharedContext): Unit =
    sig.exp.foreach(visitExp)

  /**
    * Returns the term types of the given relational or latticenal type.
    */
  def termTypesAndDenotation(tpe: Type): (List[Type], Denotation) = {
    val erased = eraseAliases(tpe)

    val den = erased.baseType match {
      case Type.Cst(TypeConstructor.Relation(_), _) => Denotation.Relational
      case Type.Cst(TypeConstructor.Lattice(_), _) => Denotation.Latticenal
      // Resiliency: if the constructor is invalid or unknown, just arbitrarily assume relational
      case _ => Denotation.Relational
    }

    val tpes = erased.typeArguments

    (tpes, den)
  }

  /**
    * Returns the labelled graph of the given expression `exp0`.
    */
  private def visitExp(exp0: Exp)(implicit sctx: SharedContext): Unit = exp0 match {
    case Exp.Cst(_, _, _) => ()

    case Exp.Var(_, _, _) => ()

    case Exp.Hole(_, _, _, _, _) => ()

    case Exp.HoleWithExp(exp, _, _, _, _) =>
      visitExp(exp)

    case Exp.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Exp.Use(_, _, exp, _) =>
      visitExp(exp)

    case Exp.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Exp.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.ApplyDef(_, exps, _, _, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.ApplyOp(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.ApplySig(_, exps, _, _, _, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Exp.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.Region(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Exp.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.Discard(exp, _, _) =>
      visitExp(exp)

    case Exp.Match(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach { case MatchRule(_, g, b, _) =>
        g.foreach(visitExp)
        visitExp(b)
      }

    case Exp.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach { case TypeMatchRule(_, _, b, _) => visitExp(b) }

    case Exp.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach { case RestrictableChooseRule(_, body) => visitExp(body) }

    case Exp.ExtMatch(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach(r => visitExp(r.exp))

    case Exp.Tag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.RestrictableTag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.ExtTag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.Tuple(elms, _, _, _) =>
      elms.foreach(visitExp)

    case Exp.RecordSelect(base, _, _, _, _) =>
      visitExp(base)

    case Exp.RecordExtend(_, value, rest, _, _, _) =>
      visitExp(value)
      visitExp(rest)

    case Exp.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest)

    case Exp.ArrayLit(elms, exp, _, _, _) =>
      elms.foreach(visitExp)
      visitExp(exp)

    case Exp.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Exp.ArrayLoad(base, index, _, _, _) =>
      visitExp(base)
      visitExp(index)

    case Exp.ArrayLength(base, _, _) =>
      visitExp(base)

    case Exp.ArrayStore(base, index, elm, _, _) =>
      visitExp(base)
      visitExp(index)
      visitExp(elm)

    case Exp.StructNew(_, fields, region, _, _, _) =>
      visitExp(region)
      fields.foreach { case (_, e) => visitExp(e) }

    case Exp.StructGet(e, _, _, _, _) =>
      visitExp(e)

    case Exp.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.VectorLit(exps, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.VectorLength(exp, _) =>
      visitExp(exp)

    case Exp.Ascribe(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Exp.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Exp.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Exp.UncheckedCast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Exp.Unsafe(exp, _, _, _, _) =>
      visitExp(exp)

    case Exp.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Exp.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach { case CatchRule(_, _, e, _) => visitExp(e) }

    case Exp.Throw(exp, _, _, _) =>
      visitExp(exp)

    case Exp.Handler(_, rules, _, _, _, _, _) =>
      rules.foreach { case HandlerRule(_, _, e, _) => visitExp(e) }

    case Exp.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.InvokeConstructor(_, args, _, _, _) =>
      args.foreach(visitExp)

    case Exp.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp)
      args.foreach(visitExp)

    case Exp.InvokeStaticMethod(_, args, _, _, _) =>
      args.foreach(visitExp)

    case Exp.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Exp.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.GetStaticField(_, _, _, _) => ()

    case Exp.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Exp.NewObject(_, _, _, _, _, _) => ()

    case Exp.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Exp.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Exp.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.SelectChannel(rules, default, _, _, _) =>
      default.foreach(visitExp)
      rules.foreach {
        case SelectChannelRule(_, exp1, exp2, _) =>
          visitExp(exp1)
          visitExp(exp2)
      }

    case Exp.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.ParYield(frags, exp, _, _, _) =>
      visitExp(exp)
      frags.foreach {
        case ParYieldFragment(_, e, _) => visitExp(e)
      }

    case Exp.Lazy(exp, _, _) =>
      visitExp(exp)

    case Exp.Force(exp, _, _, _) =>
      visitExp(exp)

    case Exp.FixpointConstraintSet(cs, _, _) =>
      cs.foreach(visitConstraint)

    case Exp.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Exp.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Exp.FixpointQueryWithProvenance(exps, Head.Atom(_, _, terms, _, _), _, _, _, _) =>
      exps.foreach(visitExp)
      terms.foreach(visitExp)

    case Exp.FixpointSolveWithProject(exps, _, _, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.FixpointQueryWithSelect(exps, queryExp, selects, _, where, _, _, _, _) =>
      exps.foreach(visitExp)
      visitExp(queryExp)
      selects.foreach(visitExp)
      where.foreach(visitExp)

    case Exp.FixpointInjectInto(exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Exp.Error(_, _, _) => ()
  }

  /**
    * Returns the labelled graph of the given constraint `c0`.
    */
  private def visitConstraint(c: Constraint)(implicit sctx: SharedContext): Unit = c match {
    case Constraint(_, Predicate.Head.Atom(headPred, den, _, headTpe, _), body0, _) =>
      val (headTerms, _) = termTypesAndDenotation(headTpe)

      // We add all body predicates and the head to the labels of each edge
      val bodyLabels: Vector[Label] = body0.collect {
        case Body.Atom(bodyPred, bodyDen, _, _, _, bodyTpe, _) =>
          val (terms, _) = termTypesAndDenotation(bodyTpe)
          Label(bodyPred, bodyDen, terms.length, terms)
      }.toVector

      val labels = bodyLabels :+ Label(headPred, den, headTerms.length, headTerms)

      body0.foreach {
        case body => body match {
          case Body.Atom(bodyPred, _, p, f, _, _, bodyLoc) =>
            sctx.edges.add(LabelledEdge(headPred, p, f, labels, bodyPred, bodyLoc))
          case Body.Functional(_, _, _) => ()
          case Body.Guard(_, _) => ()
        }
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
    * @param edges the [[LabelledEdge]]s to build the graph.
    */
  private case class SharedContext(edges: ConcurrentLinkedQueue[LabelledEdge])
}
