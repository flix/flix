/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Denotation
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.ConstraintGeneration.{visitExp, visitPattern}
import ca.uwaterloo.flix.language.phase.constraintgeneration.Constraint.{Context, addTypeConstraintsM, unifyAllTypesM, unifyEffM, unifyType3M, unifyTypeM}
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses

object SchemaConstraintGeneration {

  def visitFixpointConstraintSet(e: KindedAst.Expr.FixpointConstraintSet)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = e match {
    case KindedAst.Expr.FixpointConstraintSet(cs, tvar, loc) =>
      val constraintTypes = cs.map(visitConstraint)
      val schemaRow = unifyAllTypesM(constraintTypes, Kind.SchemaRow, loc)
      unifyTypeM(tvar, Type.mkSchema(schemaRow, loc), loc)
      val resTpe = tvar
      val resEff = Type.Pure
      (resTpe, resEff)
  }

  def visitFixpointLambda(e: KindedAst.Expr.FixpointLambda)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = e match {
    case KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc) =>

      def mkRowExtend(pparam: KindedAst.PredicateParam, restRow: Type): Type = pparam match {
        case KindedAst.PredicateParam(pred, tpe, loc) => Type.mkSchemaRowExtend(pred, tpe, restRow, tpe.loc)
      }

      def mkFullRow(baseRow: Type): Type = pparams.foldRight(baseRow)(mkRowExtend)

      val expectedRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))
      val resultRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))

      val (tpe, eff) = visitExp(exp)
      unifyTypeM(tpe, Type.mkSchema(expectedRowType, loc), loc)
      unifyTypeM(tvar, Type.mkSchema(resultRowType, loc), loc)
      val resTpe = tvar
      val resEff = eff
      (resTpe, resEff)
  }

  def visitFixpointMerge(e: KindedAst.Expr.FixpointMerge)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = e match {
    case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
      //
      //  exp1 : #{...}    exp2 : #{...}
      //  ------------------------------
      //  exp1 <+> exp2 : #{...}
      //
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      unifyType3M(tpe1, tpe2, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
      val resTpe = tpe1
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)
  }

  def visitFixpointSolve(e: KindedAst.Expr.FixpointSolve)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = e match {
    case KindedAst.Expr.FixpointSolve(exp, loc) =>
      //
      //  exp : #{...}
      //  ---------------
      //  solve exp : tpe
      //
      val (tpe, eff) = visitExp(exp)
      unifyTypeM(tpe, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
      val resEff = eff
      val resTpe = tpe
      (resTpe, resEff)
  }


  def visitFixpointFilter(e: KindedAst.Expr.FixpointFilter)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = e match {
    case KindedAst.Expr.FixpointFilter(pred, exp, tvar, loc) =>
      //
      //  exp1 : tpe    exp2 : #{ P : a  | b }
      //  -------------------------------------------
      //  project P exp2 : #{ P : a | c }
      //
      val freshPredicateTypeVar = Type.freshVar(Kind.Predicate, loc)
      val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)
      val freshResultSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

      val (tpe, eff) = visitExp(exp)
      unifyTypeM(tpe, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshRestSchemaTypeVar, loc), loc), loc)
      unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshResultSchemaTypeVar, loc), loc), loc)
      val resTpe = tvar
      val resEff = eff
      (resTpe, resEff)
  }

  def visitFixpointInject(e: KindedAst.Expr.FixpointInject)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = e match {
    case KindedAst.Expr.FixpointInject(exp, pred, tvar, loc) =>
      //
      //  exp : F[freshElmType] where F is Foldable
      //  -------------------------------------------
      //  project exp into A: #{A(freshElmType) | freshRestSchemaType}
      //
      val freshTypeConstructorVar = Type.freshVar(Kind.Star ->: Kind.Star, loc)
      val freshElmTypeVar = Type.freshVar(Kind.Star, loc)
      val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

      // Require Order and Foldable instances.
      val orderSym = PredefinedClasses.lookupClassSym("Order", root)
      val foldableSym = PredefinedClasses.lookupClassSym("Foldable", root)
      val order = Ast.TypeConstraint(Ast.TypeConstraint.Head(orderSym, loc), freshElmTypeVar, loc)
      val foldable = Ast.TypeConstraint(Ast.TypeConstraint.Head(foldableSym, loc), freshTypeConstructorVar, loc)

      addTypeConstraintsM(List(order, foldable), loc)

      val (tpe, eff) = visitExp(exp)
      unifyTypeM(tpe, Type.mkApply(freshTypeConstructorVar, List(freshElmTypeVar), loc), loc)
      unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkRelation(List(freshElmTypeVar), loc), freshRestSchemaTypeVar, loc), loc), loc)
      val resTpe = tvar
      val resEff = eff
      (resTpe, resEff)
  }

  def visitFixpointProject(e: KindedAst.Expr.FixpointProject)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = e match {
    case KindedAst.Expr.FixpointProject(pred, exp1, exp2, tvar, loc) =>
      //
      //  exp1: {$Result(freshRelOrLat, freshTupleVar) | freshRestSchemaVar }
      //  exp2: freshRestSchemaVar
      //  --------------------------------------------------------------------
      //  FixpointQuery pred, exp1, exp2 : Array[freshTupleVar]
      //
      val freshRelOrLat = Type.freshVar(Kind.Star ->: Kind.Predicate, loc)
      val freshTupleVar = Type.freshVar(Kind.Star, loc)
      val freshRestSchemaVar = Type.freshVar(Kind.SchemaRow, loc)
      val expectedSchemaType = Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.Apply(freshRelOrLat, freshTupleVar, loc), freshRestSchemaVar, loc), loc)
      val (tpe1, eff1) = visitExp(exp1)
      val (tpe2, eff2) = visitExp(exp2)
      unifyTypeM(tpe1, expectedSchemaType, loc)
      unifyTypeM(tpe2, Type.mkSchema(freshRestSchemaVar, loc), loc)
      unifyTypeM(tvar, Type.mkVector(freshTupleVar, loc), loc)
      val resTpe = tvar
      val resEff = Type.mkUnion(eff1, eff2, loc)
      (resTpe, resEff)
  }

  private def visitConstraint(con0: KindedAst.Constraint)(implicit c: Context, root: KindedAst.Root, flix: Flix): Type = {
    val KindedAst.Constraint(cparams, head0, body0, loc) = con0
    //
    //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
    //  -----------------------------------
    //  A_0 :- A_1, ..., A_n : tpe
    //
    val headPredicateType = visitHeadPredicate(head0)
    val bodyPredicateTypes = body0.map(b => visitBodyPredicate(b))
    val bodyPredicateType = unifyAllTypesM(bodyPredicateTypes, Kind.SchemaRow, loc)
    unifyTypeM(headPredicateType, bodyPredicateType, loc)
    val resTpe = headPredicateType
    resTpe
  }


  /**
    * Infers the type of the given head predicate.
    */
  private def visitHeadPredicate(head: KindedAst.Predicate.Head)(implicit c: Context, root: KindedAst.Root, flix: Flix): Type = head match {
    case KindedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc) =>
      // Adds additional type constraints if the denotation is a lattice.
      val restRow = Type.freshVar(Kind.SchemaRow, loc)
      val (termTypes, termEffs) = terms.map(visitExp(_)).unzip
      unifyEffM(Type.Pure, Type.mkUnion(termEffs, loc), loc)
      unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
      val tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
      addTypeConstraintsM(tconstrs, loc)
      val resTpe = Type.mkSchemaRowExtend(pred, tvar, restRow, loc)
      resTpe
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def visitBodyPredicate(body0: KindedAst.Predicate.Body)(implicit c: Context, root: KindedAst.Root, flix: Flix): Type = {

    body0 match {
      case KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, tvar, loc) =>
        val restRow = Type.freshVar(Kind.SchemaRow, loc)
        val termTypes = terms.map(visitPattern)
        unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
        val tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
        addTypeConstraintsM(tconstrs, loc)
        val resTpe = Type.mkSchemaRowExtend(pred, tvar, restRow, loc)
        resTpe

      case KindedAst.Predicate.Body.Functional(outVars, exp, loc) =>
        val tupleType = Type.mkTuplish(outVars.map(_.tvar), loc)
        val expectedType = Type.mkVector(tupleType, loc)
        val (tpe, eff) = visitExp(exp)
        unifyTypeM(expectedType, tpe, loc)
        unifyEffM(Type.Pure, eff, loc)
        val resTpe = mkAnySchemaRowType(loc)
        resTpe

      case KindedAst.Predicate.Body.Guard(exp, loc) =>
        val (tpe, eff) = visitExp(exp)
        unifyEffM(Type.Pure, eff, loc)
        unifyTypeM(Type.Bool, tpe, loc)
        val resTpe = mkAnySchemaRowType(loc)
        resTpe
    }
  }

  /**
    * Returns the relation or lattice type of `name` with the term types `ts`.
    */
  private def mkRelationOrLatticeType(name: String, den: Denotation, ts: List[Type], root: KindedAst.Root, loc: SourceLocation)(implicit flix: Flix): Type = den match {
    case Denotation.Relational => Type.mkRelation(ts, loc)
    case Denotation.Latticenal => Type.mkLattice(ts, loc)
  }

  /**
    * Returns the type class constraints for the given term types `ts` with the given denotation `den`.
    */
  private def getTermTypeClassConstraints(den: Ast.Denotation, ts: List[Type], root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = den match {
    case Denotation.Relational =>
      ts.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root, loc))
    case Denotation.Latticenal =>
      ts.init.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root, loc)) ::: mkTypeClassConstraintsForLatticeTerm(ts.last, root, loc)
  }

  /**
    * Constructs the type class constraints for the given relational term type `tpe`.
    */
  private def mkTypeClassConstraintsForRelationalTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Order", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }

  /**
    * Constructs the type class constraints for the given lattice term type `tpe`.
    */
  private def mkTypeClassConstraintsForLatticeTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Order", root),
      PredefinedClasses.lookupClassSym("PartialOrder", root),
      PredefinedClasses.lookupClassSym("LowerBound", root),
      PredefinedClasses.lookupClassSym("JoinLattice", root),
      PredefinedClasses.lookupClassSym("MeetLattice", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }


  private def mkAnySchemaRowType(loc: SourceLocation)(implicit flix: Flix): Type = Type.freshVar(Kind.SchemaRow, loc)
}
