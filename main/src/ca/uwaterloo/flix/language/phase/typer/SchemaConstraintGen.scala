/*
 * Copyright 2023 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.TraitSymUse
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.shared.{Denotation, PredicateAndArity, RegionScope, TraitConstraint}
import ca.uwaterloo.flix.language.phase.typer.ConstraintGen.{visitExp, visitExpUnchecked, visitPattern}
import ca.uwaterloo.flix.language.phase.util.PredefinedTraits
import ca.uwaterloo.flix.util.InternalCompilerException

object SchemaConstraintGen {

  def visitFixpointConstraintSet(e: KindedAst.Expr.FixpointConstraintSet)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    implicit val scope: RegionScope = c.getScope
    e match {
      case KindedAst.Expr.FixpointConstraintSet(cs0, loc) =>
        val (cs, constraintTypes) = cs0.map(visitConstraint).unzip
        c.unifyAllTypes(constraintTypes, loc)
        val schemaRow = constraintTypes.headOption.getOrElse(Type.freshVar(Kind.SchemaRow, loc))
        val resTpe = Type.mkSchema(schemaRow, loc)
        InferredAst.Expr.FixpointConstraintSet(cs, resTpe, loc)
    }
  }

  def visitFixpointLambda(e: KindedAst.Expr.FixpointLambda)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    implicit val scope: RegionScope = c.getScope
    e match {
      case KindedAst.Expr.FixpointLambda(pparams, exp, loc) =>
        def mkRowExtend(pparam: KindedAst.PredicateParam, restRow: Type): Type = pparam match {
          case KindedAst.PredicateParam(pred, paramTpe, _) => Type.mkSchemaRowExtend(pred, paramTpe, restRow, paramTpe.loc)
        }

        def mkFullRow(baseRow: Type): Type = pparams.foldRight(baseRow)(mkRowExtend)

        val expectedRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))
        val resultRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))

        val e1 = visitExp(exp)
        c.unifyType(e1.tpe, Type.mkSchema(expectedRowType, loc), loc)
        val ps = pparams.map(visitPredicateParam)
        val resTpe = Type.mkSchema(resultRowType, loc)
        val resEff = e1.eff
        InferredAst.Expr.FixpointLambda(ps, e1, resTpe, resEff, loc)
    }
  }

  def visitFixpointMerge(e: KindedAst.Expr.FixpointMerge)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    implicit val scope: RegionScope = c.getScope
    e match {
      case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.unifyType(e1.tpe, e2.tpe, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
        val resTpe = e1.tpe
        val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
        InferredAst.Expr.FixpointMerge(e1, e2, resTpe, resEff, loc)
    }
  }

  def visitFixpointQueryWithProvenance(e: KindedAst.Expr.FixpointQueryWithProvenance)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    e match {
      case KindedAst.Expr.FixpointQueryWithProvenance(exps, select, withh, loc1) =>
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        val (s, selectRow) = visitHeadPredicate(select)
        val (withRow, resultRow) = mkSchemaRowPair(withh, loc1)
        c.unifyAllTypes(Type.mkSchema(withRow, loc1) :: Type.mkSchema(selectRow, loc1) :: tpes, loc1)
        val resTpe = Type.mkVector(Type.mkExtensible(resultRow, loc1), loc1)
        val resEff = Type.mkUnion(effs, loc1)
        InferredAst.Expr.FixpointQueryWithProvenance(es, s, withh, resTpe, resEff, loc1)
    }
  }

  def visitFixpointQueryWithSelect(e: KindedAst.Expr.FixpointQueryWithSelect)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    implicit val scope: RegionScope = c.getScope
    e match {
      case KindedAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, loc) =>
        //
        //  exp = exps[0] <+> exps[1] <+> ... (exp is conceptual; it does not actually exist)
        //
        //  exp: freshRestSchemaVar
        //  queryExp: #{$freshRelOrLat(α₁, α₂, ...) | freshRestSchemaVar }
        //  --------------------------------------------------------------------
        //  FixpointQueryWithSelect(exps, queryExp, ...) : Vector[(α₁, α₂, ...)]
        //
        val predArity = selects.length
        val freshRelOrLat = Type.freshVar(Kind.mkArrowTo(predArity, Kind.Predicate), loc)
        val freshTermVars = List.range(0, predArity).map(_ => Type.freshVar(Kind.Star, loc))
        val tuple = Type.mkTuplish(freshTermVars, loc)
        val freshRestSchemaVar = Type.freshVar(Kind.SchemaRow, loc)
        val expectedSchemaType = Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkApply(freshRelOrLat, freshTermVars, loc), freshRestSchemaVar, loc), loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        val qe = visitExp(queryExp)
        c.unifyAllTypes(Type.mkSchema(freshRestSchemaVar, loc) :: tpes, loc)
        c.unifyType(qe.tpe, expectedSchemaType, loc)
        // The select, from, and where clauses are type checked as part of the query expression.
        val ss = selects.map(visitExpUnchecked)
        val f = from.map(visitBodyPredicateUnchecked)
        val w = where.map(visitExpUnchecked)
        val resTpe = Type.mkVector(tuple, loc)
        val resEff = Type.mkUnion(qe.eff :: effs, loc)
        InferredAst.Expr.FixpointQueryWithSelect(es, qe, ss, f, w, pred, resTpe, resEff, loc)
    }
  }

  def visitFixpointSolveWithProject(e: KindedAst.Expr.FixpointSolveWithProject)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    implicit val scope: RegionScope = c.getScope
    e match {
      case KindedAst.Expr.FixpointSolveWithProject(exps, optPreds, mode, loc) =>
        //
        //  exp = exps₁ <+> exps₂ <+> ... <+> expsₘ
        //
        //  exp : #{ P₁, P₂, ..., Pₖ, Pₖ₊₁, ..., Pₙ | b }    optPreds: P₁ ::  P₂ :: ... :: Pₖ :: Nil
        //  ---------------
        //  solve exp project P₁, P₂, ... : #{ P₁, P₂, ..., Pₖ | c }
        //
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        val freshSchemaRow = Type.freshVar(Kind.SchemaRow, loc)
        c.unifyAllTypes(Type.mkSchema(freshSchemaRow, loc) :: tpes, loc)
        val resultSchemaRow = optPreds match {
          case Some(preds) =>
            val (fullSchemaRow, resultSchemaRow) = mkSchemaRowPair(preds, loc)
            c.unifyType(freshSchemaRow, fullSchemaRow, loc)
            resultSchemaRow
          case None => freshSchemaRow
        }
        val resTpe = Type.mkSchema(resultSchemaRow, loc)
        val resEff = Type.mkUnion(effs, loc)
        InferredAst.Expr.FixpointSolveWithProject(es, optPreds, mode, resTpe, resEff, loc)

    }
  }

  def visitFixpointInjectInto(e: KindedAst.Expr.FixpointInjectInto)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    implicit val scope: RegionScope = c.getScope
    e match {
      case KindedAst.Expr.FixpointInjectInto(exps, predsAndArities, loc) =>
        val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
        val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
        val es = predsAndArities.zip(exps).map {
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

            val e1 = visitExp(exp)
            c.unifyType(e1.tpe, Type.mkApply(freshTypeConstructorVar, List(tuple), loc), loc)
            c.unifyType(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkRelation(freshElmTypeVars, loc), freshRestSchemaTypeVar, loc), loc), loc)
            c.unifyType(evar, Type.mkUnion(e1.eff, aefTpe, loc), loc)
            e1
        }
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.FixpointInjectInto(es, predsAndArities, resTpe, resEff, loc)
    }
  }

  /**
    * Infers the type of the given constraint.
    *
    * Returns the constraint together with its schema row type.
    */
  private def visitConstraint(con0: KindedAst.Constraint)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (InferredAst.Constraint, Type) = {
    implicit val scope: RegionScope = c.getScope
    val KindedAst.Constraint(cparams0, head0, body0, loc) = con0
    //
    //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
    //  -----------------------------------
    //  A_0 :- A_1, ..., A_n : tpe
    //
    val (head, headPredicateType) = visitHeadPredicate(head0)
    val (body, bodyPredicateTypes) = body0.map(b => visitBodyPredicate(b)).unzip
    c.unifyAllTypes(bodyPredicateTypes, loc)
    val bodyPredicateType = bodyPredicateTypes.headOption.getOrElse(Type.freshVar(Kind.SchemaRow, loc))
    c.unifyType(headPredicateType, bodyPredicateType, loc)
    val cparams = cparams0.map(visitConstraintParam)
    val resTpe = headPredicateType
    (InferredAst.Constraint(cparams, head, body, loc), resTpe)
  }

  /**
    * Returns the given constraint parameter annotated with the type of its binder.
    */
  private def visitConstraintParam(cparam0: KindedAst.ConstraintParam): InferredAst.ConstraintParam = cparam0 match {
    case KindedAst.ConstraintParam(sym, loc) =>
      val bnd = InferredAst.Binder(sym, sym.tvar)
      InferredAst.ConstraintParam(bnd, sym.tvar, loc)
  }

  /**
    * Translates the given `KindedAst` predicate parameter into an `InferredAst` predicate parameter.
    *
    * The type of a predicate parameter is already known, so there are no constraints to generate.
    */
  private def visitPredicateParam(pparam0: KindedAst.PredicateParam): InferredAst.PredicateParam = pparam0 match {
    case KindedAst.PredicateParam(pred, tpe, loc) => InferredAst.PredicateParam(pred, tpe, loc)
  }

  /**
    * Infers the type of the given head predicate.
    *
    * Returns the head predicate together with its schema row type.
    */
  private def visitHeadPredicate(head: KindedAst.Predicate.Head)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (InferredAst.Predicate.Head, Type) = {
    implicit val scope: RegionScope = c.getScope
    head match {
      case KindedAst.Predicate.Head.Atom(pred, den, terms, loc) =>
        // Adds additional type constraints if the denotation is a lattice.
        val tvar = Type.freshVar(Kind.Predicate, loc.asSynthetic)
        val restRow = Type.freshVar(Kind.SchemaRow, loc)
        val ts = terms.map(visitExp(_))
        val termTypes = ts.map(_.tpe)
        val termEffs = ts.map(_.eff)
        c.unifyType(Type.Pure, Type.mkUnion(termEffs, loc), loc)
        c.unifyType(tvar, mkRelationOrLatticeType(den, termTypes, loc), loc)
        addTermTraitConstraints(den, termTypes, terms.map(_.loc), root)
        val resTpe = Type.mkSchemaRowExtend(pred, tvar, restRow, loc)
        (InferredAst.Predicate.Head.Atom(pred, den, ts, tvar, loc), resTpe)
    }
  }

  /**
    * Infers the type of the given body predicate.
    *
    * Returns the body predicate together with its schema row type.
    */
  private def visitBodyPredicate(body0: KindedAst.Predicate.Body)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (InferredAst.Predicate.Body, Type) = {
    implicit val scope: RegionScope = c.getScope
    body0 match {
      case KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
        val tvar = Type.freshVar(Kind.Predicate, loc.asSynthetic)
        val restRow = Type.freshVar(Kind.SchemaRow, loc)
        val ts = terms.map(visitPattern)
        val termTypes = ts.map(_.tpe)
        c.unifyType(tvar, mkRelationOrLatticeType(den, termTypes, loc), loc)
        addTermTraitConstraints(den, termTypes, terms.map(_.loc), root)
        val resTpe = Type.mkSchemaRowExtend(pred, tvar, restRow, loc)
        (InferredAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, tvar, loc), resTpe)

      case KindedAst.Predicate.Body.Functional(syms, exp, loc) =>
        val tupleType = Type.mkTuplish(syms.map(_.tvar), loc)
        val expectedType = Type.mkVector(tupleType, loc)
        val e = visitExp(exp)
        c.unifyType(expectedType, e.tpe, loc)
        c.unifyType(Type.Pure, e.eff, loc)
        val outBnds = syms.map(sym => InferredAst.Binder(sym, sym.tvar))
        val resTpe = mkAnySchemaRowType(loc)
        (InferredAst.Predicate.Body.Functional(outBnds, e, loc), resTpe)

      case KindedAst.Predicate.Body.Guard(exp, loc) =>
        val e = visitExp(exp)
        c.unifyType(Type.Pure, e.eff, loc)
        c.unifyType(Type.Bool, e.tpe, loc)
        val resTpe = mkAnySchemaRowType(loc)
        (InferredAst.Predicate.Body.Guard(e, loc), resTpe)
    }
  }

  /**
    * Returns the given body predicate annotated with its types, but discards its constraints.
    *
    * This function is only used by `visitFixpointQueryWithSelect` for the `from` predicates of a query.
    * Those predicates are already type checked: the desugarer copies them into the pseudo-constraint that
    * forms the query expression. Here we only need the annotated predicates to complete the tree.
    */
  private def visitBodyPredicateUnchecked(body0: KindedAst.Predicate.Body)(implicit root: KindedAst.Root, flix: Flix): InferredAst.Predicate.Body = {
    val (body, _) = visitBodyPredicate(body0)(new TypeContext, root, flix)
    body
  }

  /**
    * Returns the relation or lattice type with the term types `ts`.
    */
  private def mkRelationOrLatticeType(den: Denotation, ts: List[Type], loc: SourceLocation): Type = den match {
    case Denotation.Relational => Type.mkRelation(ts, loc)
    case Denotation.Latticenal => Type.mkLattice(ts, loc)
  }

  /**
    * Adds the trait constraints for the given term types `ts` with the given denotation `den`.
    *
    * Each term's constraints are added at that term's own source location (from `locs`), so that a
    * missing instance error points at the specific offending term rather than the whole atom.
    */
  private def addTermTraitConstraints(den: Denotation, ts: List[Type], locs: List[SourceLocation], root: KindedAst.Root)(implicit c: TypeContext): Unit = den match {
    case Denotation.Relational =>
      for ((tpe, loc) <- ts.zip(locs)) {
        c.addClassConstraints(mkTraitConstraintsForRelationalTerm(tpe, root, loc), loc)
      }
    case Denotation.Latticenal =>
      for ((tpe, loc) <- ts.init.zip(locs.init)) {
        c.addClassConstraints(mkTraitConstraintsForRelationalTerm(tpe, root, loc), loc)
      }
      c.addClassConstraints(mkTraitConstraintsForLatticeTerm(ts.last, root, locs.last), locs.last)
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
    implicit val scope: RegionScope = c.getScope
    predicates.foldRight((mkAnySchemaRowType(loc), mkAnySchemaRowType(loc))) {
      case (pred, (acc1, acc2)) =>
        val fresh = Type.freshVar(Kind.Predicate, loc)
        (Type.mkSchemaRowExtend(pred, fresh, acc1, loc), Type.mkSchemaRowExtend(pred, fresh, acc2, loc))
    }
  }

  private def mkAnySchemaRowType(loc: SourceLocation)(implicit scope: RegionScope, flix: Flix): Type = Type.freshVar(Kind.SchemaRow, loc)
}
