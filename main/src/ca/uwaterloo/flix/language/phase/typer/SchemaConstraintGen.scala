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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.TraitSymUse
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.shared.{Denotation, PredicateAndArity, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.phase.typer.ConstraintGen.{visitExp, visitPattern}
import ca.uwaterloo.flix.language.phase.util.PredefinedTraits
import ca.uwaterloo.flix.util.InternalCompilerException

object SchemaConstraintGen {

  def visitFixpointConstraintSet(e: KindedAst.Expr.FixpointConstraintSet)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    e match {
      case KindedAst.Expr.FixpointConstraintSet(cs, tvar, loc) =>
        val constraintTypes = cs.map(visitConstraint)
        c.unifyAllTypes(constraintTypes, loc)
        val schemaRow = constraintTypes.headOption.getOrElse(Type.freshVar(Kind.SchemaRow, loc))
        c.unifyType(tvar, Type.mkSchema(schemaRow, loc), loc)
        val resTpe = tvar
        val resEff = Type.Pure
        (resTpe, resEff)
    }
  }

  def visitFixpointLambda(e: KindedAst.Expr.FixpointLambda)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    e match {
      case KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc) =>

        def mkRowExtend(pparam: KindedAst.PredicateParam, restRow: Type): Type = pparam match {
          case KindedAst.PredicateParam(pred, paramTpe, _) => Type.mkSchemaRowExtend(pred, paramTpe, restRow, paramTpe.loc)
        }

        def mkFullRow(baseRow: Type): Type = pparams.foldRight(baseRow)(mkRowExtend)

        val expectedRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))
        val resultRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))

        val (tpe, eff) = visitExp(exp)
        c.unifyType(tpe, Type.mkSchema(expectedRowType, loc), loc)
        c.unifyType(tvar, Type.mkSchema(resultRowType, loc), loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)
    }
  }

  def visitFixpointMerge(e: KindedAst.Expr.FixpointMerge)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    e match {
      case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.unifyType(tpe1, tpe2, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
        val resTpe = tpe1
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)
    }
  }

  def visitFixpointQueryWithProvenance(e: KindedAst.Expr.FixpointQueryWithProvenance)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    e match {
      case KindedAst.Expr.FixpointQueryWithProvenance(exps, select, withh, tvar, loc1) =>
        val (tpes, effs) = exps.map(visitExp).unzip
        val selectRow = visitHeadPredicate(select)
        val (withRow, resultRow) = mkSchemaRowPair(withh, loc1)
        c.unifyAllTypes(Type.mkSchema(withRow, loc1) :: Type.mkSchema(selectRow, loc1) :: tpes, loc1)
        val resTpe = Type.mkVector(Type.mkExtensible(resultRow, loc1), loc1)
        val resEff = Type.mkUnion(effs, loc1)
        c.unifyType(tvar, resTpe, loc1)
        (resTpe, resEff)
    }
  }

  def visitFixpointQueryWithSelect(e: KindedAst.Expr.FixpointQueryWithSelect)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    e match {
      case KindedAst.Expr.FixpointQueryWithSelect(exps, queryExp, predArity, pred, tvar, loc) =>
        //
        //  exp = exps[0] <+> exps[1] <+> ... (exp is conceptual; it does not actually exist)
        //
        //  exp: freshRestSchemaVar
        //  queryExp: #{$freshRelOrLat(α₁, α₂, ...) | freshRestSchemaVar }
        //  --------------------------------------------------------------------
        //  FixpointQueryWithSelect(exps, queryExp, ...) : Vector[(α₁, α₂, ...)]
        //
        val freshRelOrLat = Type.freshVar(Kind.mkArrowTo(predArity, Kind.Predicate), loc)
        val freshTermVars = List.range(0, predArity).map(_ => Type.freshVar(Kind.Star, loc))
        val tuple = Type.mkTuplish(freshTermVars, loc)
        val freshRestSchemaVar = Type.freshVar(Kind.SchemaRow, loc)
        val expectedSchemaType = Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkApply(freshRelOrLat, freshTermVars, loc), freshRestSchemaVar, loc), loc)
        val (tpes, effs) = exps.map(visitExp).unzip
        val (tpe, eff) = visitExp(queryExp)
        c.unifyAllTypes(Type.mkSchema(freshRestSchemaVar, loc) :: tpes, loc)
        c.unifyType(tpe, expectedSchemaType, loc)
        c.unifyType(tvar, Type.mkVector(tuple, loc), loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(eff :: effs, loc)
        (resTpe, resEff)
    }
  }

  def visitFixpointSolveWithProject(e: KindedAst.Expr.FixpointSolveWithProject)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    e match {
      case KindedAst.Expr.FixpointSolveWithProject(exps, optPreds, _, tvar, loc) =>
        //
        //  exp = exps₁ <+> exps₂ <+> ... <+> expsₘ
        //
        //  exp : #{ P₁, P₂, ..., Pₖ, Pₖ₊₁, ..., Pₙ | b }    optPreds: P₁ ::  P₂ :: ... :: Pₖ :: Nil
        //  ---------------
        //  solve exp project P₁, P₂, ... : #{ P₁, P₂, ..., Pₖ | c }
        //
        val (tpes, effs) = exps.map(visitExp).unzip
        val freshSchemaRow = Type.freshVar(Kind.SchemaRow, loc)
        c.unifyAllTypes(Type.mkSchema(freshSchemaRow, loc) :: tpes, loc)
        val resultSchemaRow = optPreds match {
          case Some(preds) =>
            val (fullSchemaRow, resultSchemaRow) = mkSchemaRowPair(preds, loc)
            c.unifyType(freshSchemaRow, fullSchemaRow, loc)
            resultSchemaRow
          case None => freshSchemaRow
        }
        c.unifyType(tvar, Type.mkSchema(resultSchemaRow, loc), loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(effs, loc)
        (resTpe, resEff)

    }
  }

  def visitFixpointInjectInto(e: KindedAst.Expr.FixpointInjectInto)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    e match {
      case KindedAst.Expr.FixpointInjectInto(exps, predsAndArities, tvar, evar, loc) =>
        predsAndArities.zip(exps).foreach {
          case (PredicateAndArity(pred, arity), exp) =>
            //
            //  exp : F[(α₁, α₂, ...)] where F is Foldable
            //  -------------------------------------------
            //  project exp into A(_, _, ...): #{A(α₁, α₂, ...) | freshRestSchemaType}
            //
            val freshTypeConstructorVar = Type.freshVar(Kind.Star ->: Kind.Star, loc)
            val freshElmTypeVars = List.range(0, arity).map(_ => Type.freshVar(Kind.Star, loc))
            val tuple = Type.mkTuplish(freshElmTypeVars, loc)
            val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

            // Require Order and Foldable instances.
            val orderSym = PredefinedTraits.lookupTraitSym("Order", root)
            val foldableSym = PredefinedTraits.lookupTraitSym("Foldable", root)
            val order = TraitConstraint(TraitSymUse(orderSym, loc), tuple, loc)
            val foldable = TraitConstraint(TraitSymUse(foldableSym, loc), freshTypeConstructorVar, loc)

            c.addClassConstraints(List(order, foldable), loc)

            val aefSym = new Symbol.AssocTypeSym(foldableSym, "Aef", loc)
            val aefTpe = Type.AssocType(AssocTypeSymUse(aefSym, loc), freshTypeConstructorVar, Kind.Eff, loc)

            val (tpe, eff) = visitExp(exp)
            c.unifyType(tpe, Type.mkApply(freshTypeConstructorVar, List(tuple), loc), loc)
            c.unifyType(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkRelation(freshElmTypeVars, loc), freshRestSchemaTypeVar, loc), loc), loc)
            c.unifyType(evar, Type.mkUnion(eff, aefTpe, loc), loc)
        }
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)
    }
  }

  private def visitConstraint(con0: KindedAst.Constraint)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): Type = {
    implicit val scope: Scope = c.getScope
    val KindedAst.Constraint(_, head0, body0, loc) = con0
    //
    //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
    //  -----------------------------------
    //  A_0 :- A_1, ..., A_n : tpe
    //
    val headPredicateType = visitHeadPredicate(head0)
    val bodyPredicateTypes = body0.map(b => visitBodyPredicate(b))
    c.unifyAllTypes(bodyPredicateTypes, loc)
    val bodyPredicateType = bodyPredicateTypes.headOption.getOrElse(Type.freshVar(Kind.SchemaRow, loc))
    c.unifyType(headPredicateType, bodyPredicateType, loc)
    val resTpe = headPredicateType
    resTpe
  }


  /**
    * Infers the type of the given head predicate.
    */
  private def visitHeadPredicate(head: KindedAst.Predicate.Head)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): Type = {
    implicit val scope: Scope = c.getScope
    head match {
      case KindedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc) =>
        // Adds additional type constraints if the denotation is a lattice.
        val restRow = Type.freshVar(Kind.SchemaRow, loc)
        val (termTypes, termEffs) = terms.map(visitExp(_)).unzip
        c.unifyType(Type.Pure, Type.mkUnion(termEffs, loc), loc)
        c.unifyType(tvar, mkRelationOrLatticeType(den, termTypes, loc), loc)
        val tconstrs = getTermTraitConstraints(den, termTypes, root, loc)
        c.addClassConstraints(tconstrs, loc)
        val resTpe = Type.mkSchemaRowExtend(pred, tvar, restRow, loc)
        resTpe
    }
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def visitBodyPredicate(body0: KindedAst.Predicate.Body)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): Type = {
    implicit val scope: Scope = c.getScope
    body0 match {
      case KindedAst.Predicate.Body.Atom(pred, den, _, _, terms, tvar, loc) =>
        val restRow = Type.freshVar(Kind.SchemaRow, loc)
        val termTypes = terms.map(visitPattern)
        c.unifyType(tvar, mkRelationOrLatticeType(den, termTypes, loc), loc)
        val tconstrs = getTermTraitConstraints(den, termTypes, root, loc)
        c.addClassConstraints(tconstrs, loc)
        val resTpe = Type.mkSchemaRowExtend(pred, tvar, restRow, loc)
        resTpe

      case KindedAst.Predicate.Body.Functional(syms, exp, loc) =>
        val tupleType = Type.mkTuplish(syms.map(_.tvar), loc)
        val expectedType = Type.mkVector(tupleType, loc)
        val (tpe, eff) = visitExp(exp)
        c.unifyType(expectedType, tpe, loc)
        c.unifyType(Type.Pure, eff, loc)
        val resTpe = mkAnySchemaRowType(loc)
        resTpe

      case KindedAst.Predicate.Body.Guard(exp, loc) =>
        val (tpe, eff) = visitExp(exp)
        c.unifyType(Type.Pure, eff, loc)
        c.unifyType(Type.Bool, tpe, loc)
        val resTpe = mkAnySchemaRowType(loc)
        resTpe
    }
  }

  /**
    * Returns the relation or lattice type with the term types `ts`.
    */
  private def mkRelationOrLatticeType(den: Denotation, ts: List[Type], loc: SourceLocation): Type = den match {
    case Denotation.Relational => Type.mkRelation(ts, loc)
    case Denotation.Latticenal => Type.mkLattice(ts, loc)
  }

  /**
    * Returns the trait constraints for the given term types `ts` with the given denotation `den`.
    */
  private def getTermTraitConstraints(den: Denotation, ts: List[Type], root: KindedAst.Root, loc: SourceLocation): List[TraitConstraint] = den match {
    case Denotation.Relational =>
      ts.flatMap(mkTraitConstraintsForRelationalTerm(_, root, loc))
    case Denotation.Latticenal =>
      ts.init.flatMap(mkTraitConstraintsForRelationalTerm(_, root, loc)) ::: mkTraitConstraintsForLatticeTerm(ts.last, root, loc)
  }

  /**
    * Constructs the trait constraints for the given relational term type `tpe`.
    */
  private def mkTraitConstraintsForRelationalTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[TraitConstraint] = {
    val traits = List(
      PredefinedTraits.lookupTraitSym("Eq", root),
      PredefinedTraits.lookupTraitSym("Order", root),
    )
    traits.map(trt => TraitConstraint(TraitSymUse(trt, loc), tpe, loc))
  }

  /**
    * Constructs the trait constraints for the given lattice term type `tpe`.
    */
  private def mkTraitConstraintsForLatticeTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[TraitConstraint] = {
    val traits = List(
      PredefinedTraits.lookupTraitSym("Eq", root),
      PredefinedTraits.lookupTraitSym("Order", root),
      PredefinedTraits.lookupTraitSym("PartialOrder", root),
      PredefinedTraits.lookupTraitSym("LowerBound", root),
      PredefinedTraits.lookupTraitSym("JoinLattice", root),
      PredefinedTraits.lookupTraitSym("MeetLattice", root),
    )
    traits.map(trt => TraitConstraint(TraitSymUse(trt, loc), tpe, loc))
  }

  /**
    * Returns a pair of open schema rows each consisting of predicate names in `predicates`.
    */
  private def mkSchemaRowPair(predicates: List[Name.Pred], loc: SourceLocation)(implicit c: TypeContext, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    predicates.foldRight((mkAnySchemaRowType(loc), mkAnySchemaRowType(loc))) {
      case (pred, (acc1, acc2)) =>
        val fresh = Type.freshVar(Kind.Predicate, loc)
        (Type.mkSchemaRowExtend(pred, fresh, acc1, loc), Type.mkSchemaRowExtend(pred, fresh, acc2, loc))
    }
  }

  private def mkAnySchemaRowType(loc: SourceLocation)(implicit scope: Scope, flix: Flix): Type = Type.freshVar(Kind.SchemaRow, loc)
}
