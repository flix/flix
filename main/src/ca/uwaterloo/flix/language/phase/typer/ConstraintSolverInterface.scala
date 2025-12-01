/*
 * Copyright 2024 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TraitSymUse}
import ca.uwaterloo.flix.language.ast.shared.{Denotation, EqualityConstraint, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, Substitution, TraitEnv}
import ca.uwaterloo.flix.language.phase.util.PredefinedTraits
import ca.uwaterloo.flix.util.Build

import scala.annotation.tailrec

/**
  * Interface to [[ConstraintSolver2]].
  */
object ConstraintSolverInterface {

  /**
    * Resolves constraints in the given definition using the given inference result.
    */
  def visitDef(defn: KindedAst.Def, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[TraitConstraint], econstrs0: List[EqualityConstraint], tenv0: TraitEnv, eqEnv0: EqualityEnv, root: KindedAst.Root)(implicit flix: Flix): (SubstitutionTree, List[TypeError]) = defn match {
    case KindedAst.Def(sym, spec, _, _) =>
      if (flix.options.xprinttyper.contains(sym.toString)) {
        Debug.startRecording()
      }
      val result = visitSpec(spec, defn.loc, infResult, renv0, tconstrs0, econstrs0, tenv0, eqEnv0, root)
      Debug.stopRecording()
      result
  }

  /**
    * Resolves constraints in the given signature using the given inference result.
    */
  def visitSig(sig: KindedAst.Sig, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[TraitConstraint], tenv0: TraitEnv, eqEnv0: EqualityEnv, root: KindedAst.Root)(implicit flix: Flix): (SubstitutionTree, List[TypeError]) = sig match {
    case KindedAst.Sig(_, _, None, _) => (SubstitutionTree.empty, Nil)
    case KindedAst.Sig(sym, spec, Some(_), _) =>
      if (flix.options.xprinttyper.contains(sym.toString)) {
        Debug.startRecording()
      }
      visitSpec(spec, sig.loc, infResult, renv0, tconstrs0, Nil, tenv0, eqEnv0, root)
  }

  /**
    * Resolves constraints in the given spec using the given inference result.
    */
  def visitSpec(spec: KindedAst.Spec, loc: SourceLocation, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[TraitConstraint], econstrs0: List[EqualityConstraint], tenv0: TraitEnv, eqEnv0: EqualityEnv, root: KindedAst.Root)(implicit flix: Flix): (SubstitutionTree, List[TypeError]) = spec match {
    case KindedAst.Spec(_, _, _, _, fparams, _, tpe, eff, tconstrs, econstrs) =>

      val InfResult(infConstrs, infTpe, infEff, infRenv) = infResult

      // The initial substitution maps from formal parameters to their types
      val initialSubst = fparams.foldLeft(Substitution.empty) {
        case (acc, KindedAst.FormalParam(sym, paramTpe, _, _)) => acc ++ Substitution.singleton(sym.tvar.sym, openOuterSchema(paramTpe)(Scope.Top, flix))
      }

      // Wildcard tparams are not counted in the tparams, so we need to traverse the types to get them.
      val allTparams = tpe.typeVars ++ eff.typeVars ++ fparams.flatMap(_.tpe.typeVars) ++ econstrs.flatMap(_.tpe2.typeVars)

      // The rigidity environment is made up of:
      // 1. rigid variables from the context (e.g. from instance type parameters)
      // 2. rigid variables from type inference (e.g. regions)
      // 3. rigid variables from declared function type parameters
      val renv = allTparams.foldLeft(infRenv ++ renv0) {
        case (acc, Type.Var(sym, _)) => acc.markRigid(sym)
      }

      // The trait and equality environments are made up of:
      // 1. constraints from the context (e.g. constraints on instances and traits, plus global constraints)
      // 2. constraints from the function signature
      val tenv = expandTraitEnv(tenv0, tconstrs ++ tconstrs0)
      val eenv = expandEqualityEnv(eqEnv0, econstrs ++ econstrs0)

      // We add extra constraints for the declared type and effect
      val declaredTpeConstr = TypeConstraint.Equality(tpe, infTpe, Provenance.ExpectType(expected = tpe, actual = infTpe, loc))
      val declaredEffConstr = TypeConstraint.Equality(eff, infEff, Provenance.ExpectEffect(expected = eff, actual = infEff, loc))
      val constrs0 = declaredTpeConstr :: declaredEffConstr :: infConstrs

      // Apply the initial substitution to all the constraints
      val initialTree = SubstitutionTree.shallow(initialSubst)
      val constrs = constrs0.map(initialTree.apply)

      ///////////////////////////////////////////////////////////////////
      //             This is where the stuff happens!                  //
      // We resolve the constraints under the environments we created. //
      ///////////////////////////////////////////////////////////////////

      val (leftovers, subst) = ConstraintSolver2.solveAll(constrs, initialTree)(Scope.Top, renv, tenv, eenv, flix)
      leftovers match {
        case Nil =>
          // All constraints solved. Yay!
          (subst, Nil)
        case _ =>
          // We have one or more type errors. We check whether we are in development or production mode.
          flix.options.build match {
            case Build.Production =>
              // We are in production mode, so we have to return immediately with the *original* type error(s).
              (subst, mkTypeErrors(leftovers, subst, renv, root))

            case Build.Development =>
              // Otherwise we have special logic for the [[Debug]] effect: We solve a new constraint system where [[Debug]] is allowed by the effect signature.
              val declaredEffWithDebug = Type.mkUnion(eff, Type.Debug, loc)
              val declaredEffConstrWithDebug = TypeConstraint.Equality(declaredEffWithDebug, infEff, Provenance.ExpectEffect(expected = declaredEffWithDebug, actual = infEff, loc))
              val constrs0 = declaredTpeConstr :: declaredEffConstrWithDebug :: infConstrs
              val constrsWithDebug = constrs0.map(initialTree.apply)
              val (leftovers2, subst2) = ConstraintSolver2.solveAll(constrsWithDebug, initialTree)(Scope.Top, renv, tenv, eenv, flix)

              leftovers2 match {
                case Nil =>
                  // Success -- We can type check with [[Debug]].
                  (subst2, Nil)
                case _ =>
                  // Failure -- We report the *original* type error(s). We report the original errors to avoid spurious occurrences of the [[Debug]] effect.
                  (subst, mkTypeErrors(leftovers, subst, renv, root))
              }
          }
      }
  }

  /**
    * Constructs a collection of type errors from a list of unsolvable type constraints `constrs`.
    */
  private def mkTypeErrors(constrs: List[TypeConstraint], subst: SubstitutionTree, renv: RigidityEnv, root: KindedAst.Root)(implicit flix: Flix): List[TypeError] =
    constrs.flatMap(mkTypeError(_, subst, renv, root))

  /**
    * Constructs a collection of type errors from a single unsolvable constraint `constr`.
    */
  private def mkTypeError(constr: TypeConstraint, subst: SubstitutionTree, renv: RigidityEnv, root: KindedAst.Root)(implicit flix: Flix): List[TypeError] = constr match {
    case TypeConstraint.Equality(Type.UnresolvedJvmType(member, _), _, prov) =>
      List(mkErrorFromUnresolvedJvmMember(member, renv, subst, prov.loc))

    case TypeConstraint.Equality(_, Type.UnresolvedJvmType(member, _), prov) =>
      List(mkErrorFromUnresolvedJvmMember(member, renv, subst, prov.loc))

    case TypeConstraint.Equality(tpe1, tpe2, Provenance.ExpectType(expected, actual, loc)) =>
      (tpe1.kind, tpe2.kind) match {
        case (Kind.RecordRow, Kind.RecordRow) =>
          mkErrorsFromRecords(tpe1, tpe2, renv, loc)
        case (_, _) =>
          List(TypeError.UnexpectedType(expected = subst(expected), inferred = subst(actual), renv, loc))
      }

    case TypeConstraint.Equality(tpe1, tpe2, Provenance.ExpectArgument(expected, actual, sym, num, loc)) =>
      (tpe1.kind, tpe2.kind) match {
        case (Kind.RecordRow, Kind.RecordRow) =>
          mkErrorsFromRecords(tpe1, tpe2, renv, loc)
        case (_, _) =>
          List(TypeError.UnexpectedArg(sym, num, expected = subst(expected), actual = subst(actual), renv, loc))
      }

    case TypeConstraint.Equality(_, _, Provenance.NonUnitStatement(actual, loc)) =>
      List(TypeError.NonUnitStatement(subst(actual), loc))

    case TypeConstraint.Equality(baseType1, baseType2, Provenance.Match(fullType1, fullType2, loc)) =>
      val default = List(mkMismatchedTypesOrEffects(subst(baseType1), subst(baseType2), subst(fullType1), subst(fullType2), renv, loc))

      (fullType1.typeConstructor, fullType1.typeConstructor) match {
        case (Some(TypeConstructor.SchemaRowExtend(pred1)), Some(TypeConstructor.SchemaRowExtend(pred2))) if pred1 == pred2 =>
          (baseType1.typeConstructor, baseType2.typeConstructor) match {
            case (Some(TypeConstructor.Relation(arity1)), Some(TypeConstructor.Relation(arity2))) if arity1 != arity2 =>
              List(TypeError.MismatchedPredicateArity(pred1, arity1, arity2, baseType1.loc, baseType2.loc, loc))

            case (Some(TypeConstructor.Lattice(arity1)), Some(TypeConstructor.Lattice(arity2))) if arity1 != arity2 =>
              List(TypeError.MismatchedPredicateArity(pred1, arity1, arity2, baseType1.loc, baseType2.loc, loc))

            case (Some(TypeConstructor.Relation(_)), Some(TypeConstructor.Lattice(_))) =>
              List(TypeError.MismatchedPredicateDenotation(pred1, Denotation.Relational, Denotation.Latticenal, baseType1.loc, baseType2.loc, loc))

            case (Some(TypeConstructor.Lattice(_)), Some(TypeConstructor.Relation(_))) =>
              List(TypeError.MismatchedPredicateDenotation(pred1, Denotation.Latticenal, Denotation.Relational, baseType1.loc, baseType2.loc, loc))

            case _ => default
          }

        case _ => default
      }

    case TypeConstraint.Equality(tpe1, tpe2, prov) =>
      List(mkMismatchedTypesOrEffects(subst(tpe1), subst(tpe2), subst(tpe1), subst(tpe2), renv, prov.loc))

    // TODO We have simply duplicated equality here.
    // TODO We should establish invariants on conflicted/equality cases.
    case TypeConstraint.Conflicted(Type.UnresolvedJvmType(member, _), _, prov) =>
      List(mkErrorFromUnresolvedJvmMember(member, renv, subst, prov.loc))

    case TypeConstraint.Conflicted(_, Type.UnresolvedJvmType(member, _), prov) =>
      List(mkErrorFromUnresolvedJvmMember(member, renv, subst, prov.loc))

    case TypeConstraint.Conflicted(_, _, Provenance.ExpectType(expected, actual, loc)) =>
      List(TypeError.UnexpectedType(expected = subst(expected), inferred = subst(actual), renv, loc))

    case TypeConstraint.Conflicted(_, _, Provenance.ExpectArgument(expected, actual, sym, num, loc)) =>
      List(TypeError.UnexpectedArg(sym, num, expected = subst(expected), actual = subst(actual), renv, loc))

    case TypeConstraint.Conflicted(tpe1, tpe2, Provenance.Match(baseTpe1, baseTpe2, loc)) =>
      List(mkMismatchedTypesOrEffects(subst(baseTpe1), subst(baseTpe2), subst(tpe1), subst(tpe2), renv, loc))

    case TypeConstraint.Conflicted(_, _, Provenance.Timeout(msg, loc)) =>
      List(TypeError.TooComplex(msg, loc))

    case TypeConstraint.Conflicted(tpe1, tpe2, prov) =>
      List(mkMismatchedTypesOrEffects(subst(tpe1), subst(tpe2), subst(tpe1), subst(tpe2), renv, prov.loc))

    case TypeConstraint.Trait(sym, tpe, loc) =>
      tpe.typeConstructor match {
        case Some(TypeConstructor.Arrow(_)) => List(TypeError.MissingInstanceArrow(sym, subst(tpe), renv, loc))
        case _ =>
          if (sym == PredefinedTraits.lookupTraitSym("Eq", root)) {
            List(TypeError.MissingInstanceEq(subst(tpe), renv, loc))
          } else if (sym == PredefinedTraits.lookupTraitSym("Order", root)) {
            List(TypeError.MissingInstanceOrder(subst(tpe), renv, loc))
          } else if (sym == PredefinedTraits.lookupTraitSym("ToString", root)) {
            List(TypeError.MissingInstanceToString(subst(tpe), renv, loc))
          } else {
            List(TypeError.MissingInstance(sym, subst(tpe), renv, loc))
          }
      }
    case TypeConstraint.Purification(sym, _, _, _, nested) =>
      nested.flatMap(mkTypeError(_, subst.branches.getOrElse(sym, SubstitutionTree.empty), renv, root))
  }

  /**
    * Create either the MismatchedTypes or MismatchedEffects error based on the kind of the type.
    */
  private def mkMismatchedTypesOrEffects(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): TypeError = {
    baseType1.kind match {
      case Kind.Eff =>
        TypeError.MismatchedEffects(baseType1, baseType2, fullType1, fullType2, renv, loc)
      case _ =>
        TypeError.MismatchedTypes(baseType1, baseType2, fullType1, fullType2, renv, loc)
    }
  }

  /**
    * Creates an appropriate error from the unresolved Jvm member.
    */
  private def mkErrorFromUnresolvedJvmMember(member: Type.JvmMember, renv: RigidityEnv, subst: SubstitutionTree, loc: SourceLocation)(implicit flix: Flix): TypeError = member match {
    case JvmMember.JvmConstructor(clazz, tpes) => TypeError.ConstructorNotFound(clazz, tpes.map(subst.apply), renv, loc)
    case JvmMember.JvmField(base, tpe, name) => TypeError.FieldNotFound(base, name, subst(tpe), loc)
    case JvmMember.JvmMethod(tpe, name, tpes) => TypeError.MethodNotFound(name, subst(tpe), tpes.map(subst.apply), loc)
    case JvmMember.JvmStaticMethod(clazz, name, tpes) => TypeError.StaticMethodNotFound(clazz, name, tpes.map(subst.apply), renv, loc)
  }

  /**
    * Creates appropriate errors for the mismatched record types.
    */
  private def mkErrorsFromRecords(tpe1: Type, tpe2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): List[TypeError] = {
    // TODO: Open records
    (tpe1, tpe2) match {
      case (Type.Var(_, _), _) | (_, Type.Var(_, _)) => List(mkMismatchedTypesOrEffects(tpe1, tpe2, tpe1, tpe2, renv, loc))
      case _ =>
        val tpe1Labels = getRecordLabels(tpe1, List.empty[(Name.Label, Type)])
        val tpe2Labels = getRecordLabels(tpe2, List.empty[(Name.Label, Type)])
        // Extra label only.
        if (tpe1Labels.isEmpty && tpe2Labels.nonEmpty) {
          tpe2Labels.map {
            case (label, labelTpe) =>
              TypeError.ExtraLabel(label, labelTpe, tpe2, renv, loc)
          }
          // Undefined label only.
        } else if (tpe2Labels.map(_._1).toSet.subsetOf(tpe1Labels.map(_._1).toSet)) {
          tpe1Labels.flatMap {
            case (label, labelTpe) =>
              if (!tpe2Labels.map(_._1).contains(label)) {
                List(TypeError.UndefinedLabel(label, labelTpe, tpe2, renv, loc))
              } else Nil
          }
        } else if (tpe2Labels.isEmpty && tpe1Labels.nonEmpty) {
          tpe1Labels.map {
            case (label, labelTpe) =>
              TypeError.UndefinedLabel(label, labelTpe, tpe2, renv, loc)
          }
          // Both undefined and extra labels.
        } else {
          tpe1Labels.filterNot(tpe2Labels.contains(_)).map {
            case (label, labelTpe) =>
              TypeError.UndefinedLabel(label, labelTpe, tpe2, renv, loc)
          } ++
            tpe2Labels.filterNot(tpe1Labels.contains(_)).map {
              case (label, labelTpe) =>
                TypeError.ExtraLabel(label, labelTpe, tpe2, renv, loc)
            }
        }
    }
  }

  /**
    * Collects the record labels from the given type.
    */
  @tailrec
  private def getRecordLabels(tpe: Type, acc: List[(Name.Label, Type)]): List[(Name.Label, Type)] = tpe match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), labelTpe, _), rest, _) =>
      getRecordLabels(rest, (label, labelTpe) :: acc)
    case _ => acc
  }

  /**
    * Opens schema types `#{A(Int32) | {}}` becomes `#{A(Int32) | r}` with a fresh
    * `r`. This only happens for if the row type is the topmost type, i.e. this
    * doesn't happen inside tuples or other such nesting.
    */
  private def openOuterSchema(tpe: Type)(implicit scope: Scope, flix: Flix): Type = {
    @tailrec
    def transformRow(tpe: Type, acc: Type => Type): Type = tpe match {
      case Type.Cst(TypeConstructor.SchemaRowEmpty, loc) =>
        acc(Type.freshVar(TypeConstructor.SchemaRowEmpty.kind, loc))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), rest, loc3) =>
        transformRow(rest, inner =>
          // copy into acc, just replacing `rest` with `inner`
          acc(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), inner, loc3))
        )
      case other => acc(other)
    }

    tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), row, loc2) =>
        Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), transformRow(row, x => x), loc2)
      case other => other
    }
  }

  /**
    * Adds the given type constraints as assumptions to the trait environment.
    *
    * Transitively adds the supertraits of the constraints.
    * For example, given the trait environment:
    *
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    * }}}
    *
    * If we add
    * {{{
    *   instance Order[b]
    * }}}
    *
    * then we get
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    *
    *   instance Eq[b]
    *   instance Order[b]
    * }}}
    */
  def expandTraitEnv(tenv: TraitEnv, tconstrs: List[TraitConstraint]): TraitEnv = {
    tconstrs.foldLeft(tenv) {
      case (acc, TraitConstraint(TraitSymUse(sym, _), tpe, _)) =>
        acc.addInstance(sym, tpe)
    }
  }

  /**
    * Adds the given equality constraints as assumptions to the equality environment.
    *
    * For example, given the equality environment:
    * {{{
    *   Elm[List[a]] ~ a
    * }}}
    *
    * If we add
    * {{{
    *   Elm[b] ~ String
    * }}}
    *
    * then we get
    * {{{
    *   Elm[List[a]] ~ a
    *   Elm[b] ~ String
    * }}}
    */
  def expandEqualityEnv(eqEnv: EqualityEnv, econstrs: List[EqualityConstraint]): EqualityEnv = {
    econstrs.foldLeft(eqEnv) {
      case (acc, EqualityConstraint(AssocTypeSymUse(sym, _), tpe1, tpe2, _)) =>
        acc.addAssocTypeDef(sym, tpe1, tpe2)
    }
  }
}
