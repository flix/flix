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
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Label, LabelledEdge, LabelledGraph}
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

/**
  * The [[DatalogDependencies]] class computes the [[LabelledGraph]] of the
  * whole program, which represents all static dependencies between datalog
  * predicates.
  *
  * The computed [[LabelledGraph]] is used in [[Stratifier]].
  */
object DatalogDependencies {

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("DatalogDependencies") {
    // Compute an over-approximation of the dependency graph for all constraints in the program.
    val defs = root.defs.values.toList
    val instanceDefs = root.instances.values.flatten.flatMap(_.defs)
    val g = ParOps.parAgg(defs ++ instanceDefs, LabelledGraph.empty)({
      case (acc, d) => acc + labelledGraphOfDef(d)
    }, _ + _)
    root.copy(datalogGlobalGraph = g).toSuccess
  }

  /**
    * Returns the term types of the given relational or latticenal type.
    */
  def termTypesAndDenotation(tpe: Type): (List[Type], Denotation) = eraseAliases(tpe) match {
    case Type.Apply(Type.Cst(tc, _), t, _) =>
      val den = tc match {
        case TypeConstructor.Relation => Denotation.Relational
        case TypeConstructor.Lattice => Denotation.Latticenal
        case _ => throw InternalCompilerException(s"Unexpected non-denotation type constructor: '$tc'", tpe.loc)
      }
      t.baseType match {
        case Type.Cst(TypeConstructor.Tuple(_), _) => (t.typeArguments, den) // Multi-ary
        case Type.Cst(TypeConstructor.Unit, _) => (Nil, den)
        case _ => (List(t), den) // Unary
      }
    case _: Type.Var =>
      // This could occur when querying or projecting a non-existent predicate
      (Nil, Denotation.Relational)
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe.'", tpe.loc)
  }

  /**
    * Returns the labelled graph of the given definition `def0`.
    */
  private def labelledGraphOfDef(def0: Def): LabelledGraph =
    labelledGraphOfExp(def0.exp)


  /**
    * Returns the labelled graph of the given expression `exp0`.
    */
  private def labelledGraphOfExp(exp0: Expr): LabelledGraph = exp0 match {
    case Expr.Cst(_, _, _) => LabelledGraph.empty

    case Expr.Var(_, _, _) => LabelledGraph.empty

    case Expr.Def(_, _, _) => LabelledGraph.empty

    case Expr.Sig(_, _, _) => LabelledGraph.empty

    case Expr.Hole(_, _, _) => LabelledGraph.empty

    case Expr.HoleWithExp(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Use(_, _, exp, _) =>
      labelledGraphOfExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Apply(exp, exps, _, _, _) =>
      val init = labelledGraphOfExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc + labelledGraphOfExp(exp)
      }

    case Expr.Unary(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.Let(_, _, exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.LetRec(_, _, _, exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.Region(_, _) =>
      LabelledGraph.empty

    case Expr.Scope(_, _, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.ScopeExit(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2) + labelledGraphOfExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.Discard(exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      val dg = labelledGraphOfExp(exp)
      rules.foldLeft(dg) {
        case (acc, MatchRule(_, g, b)) => acc + g.map(labelledGraphOfExp).getOrElse(LabelledGraph.empty) + labelledGraphOfExp(b)
      }

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      val dg = labelledGraphOfExp(exp)
      rules.foldLeft(dg) {
        case (acc, TypeMatchRule(_, _, b)) => acc + labelledGraphOfExp(b)
      }

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      val dg1 = labelledGraphOfExp(exp)
      val dg2 = rules.foldLeft(LabelledGraph.empty) {
        case (acc, RestrictableChooseRule(_, body)) => acc + labelledGraphOfExp(body)
      }
      dg1 + dg2

    case Expr.Tag(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.RestrictableTag(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Tuple(elms, _, _, _) =>
      elms.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expr.RecordEmpty(_, _) =>
      LabelledGraph.empty

    case Expr.RecordSelect(base, _, _, _, _) =>
      labelledGraphOfExp(base)

    case Expr.RecordExtend(_, value, rest, _, _, _) =>
      labelledGraphOfExp(value) + labelledGraphOfExp(rest)

    case Expr.RecordRestrict(_, rest, _, _, _) =>
      labelledGraphOfExp(rest)

    case Expr.ArrayLit(elms, exp, _, _, _) =>
      elms.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2) + labelledGraphOfExp(exp3)

    case Expr.ArrayLoad(base, index, _, _, _) =>
      labelledGraphOfExp(base) + labelledGraphOfExp(index)

    case Expr.ArrayLength(base, _, _) =>
      labelledGraphOfExp(base)

    case Expr.ArrayStore(base, index, elm, _, _) =>
      labelledGraphOfExp(base) + labelledGraphOfExp(index) + labelledGraphOfExp(elm)

    case Expr.VectorLit(exps, _, _, _) =>
      exps.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.VectorLength(exp, _) =>
      labelledGraphOfExp(exp)

    case Expr.Ref(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.Deref(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Assign(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.Ascribe(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.UncheckedMaskingCast(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc + labelledGraphOfExp(e)
      }

    case Expr.TryWith(exp, _, rules, _, _, _) =>
      rules.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, HandlerRule(_, _, e)) => acc + labelledGraphOfExp(e)
      }

    case Expr.Do(_, exps, _, _, _) =>
      exps.foldLeft(LabelledGraph.empty) {
        case (acc, exp) => acc + labelledGraphOfExp(exp)
      }

    case Expr.Resume(exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.InvokeConstructor(_, args, _, _, _) =>
      args.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expr.InvokeMethod(_, exp, args, _, _, _) =>
      args.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expr.InvokeStaticMethod(_, args, _, _, _) =>
      args.foldLeft(LabelledGraph.empty) {
        case (acc, e) => acc + labelledGraphOfExp(e)
      }

    case Expr.GetField(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      LabelledGraph.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.NewObject(_, _, _, _, _, _) =>
      LabelledGraph.empty

    case Expr.NewChannel(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.GetChannel(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      val dg = default match {
        case None => LabelledGraph.empty
        case Some(d) => labelledGraphOfExp(d)
      }

      rules.foldLeft(dg) {
        case (acc, SelectChannelRule(_, exp1, exp2)) => acc + labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)
      }

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.foldLeft(labelledGraphOfExp(exp)) {
        case (acc, ParYieldFragment(_, e, _)) => acc + labelledGraphOfExp(e)
      }

    case Expr.Lazy(exp, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Force(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(LabelledGraph.empty) {
        case (dg, c) => dg + labelledGraphOfConstraint(c)
      }

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      labelledGraphOfExp(exp1) + labelledGraphOfExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      labelledGraphOfExp(exp)

    case Expr.Error(_, _, _) =>
      LabelledGraph.empty

  }


  /**
    * Returns the labelled graph of the given constraint `c0`.
    */
  private def labelledGraphOfConstraint(c: Constraint): LabelledGraph = c match {
    case Constraint(_, Predicate.Head.Atom(headPred, den, _, headTpe, _), body0, _) =>
      val (headTerms, _) = termTypesAndDenotation(headTpe)

      // We add all body predicates and the head to the labels of each edge
      val bodyLabels: Vector[Label] = body0.collect {
        case Body.Atom(bodyPred, den, _, _, _, bodyTpe, _) =>
          val (terms, _) = termTypesAndDenotation(bodyTpe)
          Label(bodyPred, den, terms.length, terms)
      }.toVector

      val labels = bodyLabels :+ Label(headPred, den, headTerms.length, headTerms)

      val edges = body0.foldLeft(Vector.empty[LabelledEdge]) {
        case (edges, body) => body match {
          case Body.Atom(bodyPred, _, p, f, _, _, bodyLoc) =>
            edges :+ LabelledEdge(headPred, p, f, labels, bodyPred, bodyLoc)
          case Body.Functional(_, _, _) => edges
          case Body.Guard(_, _) => edges
        }
      }

      LabelledGraph(edges)
  }

}
