/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, Name, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.phase.typer.{Progress, TypeReduction2}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.CofiniteSet

/**
  * Shared definition of what a given (possibly non-ground) monomorph type becomes:
  * effect canonicalization and associated-type reduction.
  *
  * Both [[ConstraintSolver]] and [[Specialize]] must agree on this, or
  * their `(sym, type)` defTable keys diverge for the same instantiation.
  */
private[monomorph2] object Canonicalization {

  /** Puts the effect formula `eff` into canonical form. */
  def canonicalEffect(eff: Type): Type = coSetToType(evalEff(eff), eff.loc)

  /**
    * Evaluates the effect formula `eff` to a set of atomic effects.
    *
    * N.B. Throws [[InternalCompilerException]] if `eff` is not simplified and ground.
    */
  def evalEff(eff: Type): CofiniteSet[Symbol.EffSym] = eff match {
    case Type.Univ                                                                      => CofiniteSet.universe
    case Type.Pure                                                                      => CofiniteSet.empty
    case Type.Cst(TypeConstructor.Effect(sym, _), _)                                    => CofiniteSet.mkSet(sym)
    case Type.Cst(TypeConstructor.Region(_), _)                                         => CofiniteSet.mkSet(Symbol.IO)
    case Type.Alias(_, _, inner, _)                                                     => evalEff(inner)
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), y, _)                      => CofiniteSet.complement(evalEff(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y, _)         => CofiniteSet.union(evalEff(x), evalEff(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y, _)  => CofiniteSet.intersection(evalEff(x), evalEff(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), x, _), y, _)    => CofiniteSet.difference(evalEff(x), evalEff(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x, _), y, _) => CofiniteSet.xor(evalEff(x), evalEff(y))
    case other => throw InternalCompilerException(s"Unexpected effect $other", other.loc)
  }

  /**
    * Renders `set` as a [[Type]] (the inverse of [[evalEff]]).
    * - A finite [[CofiniteSet.Set]] becomes a union of `Effect` constants.
    * - A [[CofiniteSet.Compl]] becomes the complement of one.
    */
  def coSetToType(set: CofiniteSet[Symbol.EffSym], loc: SourceLocation): Type = set match {
    case CofiniteSet.Set(s)   => Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym, Kind.Eff), loc)), loc)
    case CofiniteSet.Compl(s) => Type.mkComplement(Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym, Kind.Eff), loc)), loc), loc)
  }

  /**
    * Reduces the associated type `assoc` (e.g. `Foo.Assoc[a]`) to its concrete definition.
    *
    * N.B. Throws [[InternalCompilerException]] if `assoc` is not actually reducible.
    */
  def reduceAssocType(assoc: Type.AssocType)(implicit root: TypedAst.Root, flix: Flix): Type = {
    val progress = Progress()
    val (res, cs) = TypeReduction2.reduce(assoc)(RegionScope.Top, RigidityEnv.empty, progress, root.eqEnv, flix)
    if (cs.nonEmpty) throw InternalCompilerException(s"unexpected constraints: $cs", assoc.loc)
    if (progress.query()) res
    else throw InternalCompilerException(s"Could not reduce associated type $assoc", assoc.loc)
  }

  /**
    * Normalizes `app` by rebuilding it via the matching `Type.mk*` function for its head (e.g.
    * `Type.mkUnion`, `Type.mkCaseUnion`, `Type.mkRecordRowExtend`).
    *
    * N.B. If `isGround` and the result has effect kind, it is further canonicalized via
    * [[canonicalEffect]].
    */
  def normalizeApply(normalize: Type => Type, app: Type.Apply, isGround: Boolean): Type = {
    val Type.Apply(tpe1, tpe2, loc) = app
    val x = normalize(tpe1)
    val y = normalize(tpe2)
    // Check x's kind, not the original app's: substitution may change a higher-kinded var's kind,
    // and the applied kind is computed from x.kind alone, so this avoids a throwaway Type.Apply.
    (x, y) match {
      case _ if isGround && (x.kind match { case Kind.Arrow(_, k) => k == Kind.Eff; case _ => false }) => canonicalEffect(Type.Apply(x, y, loc))
      case (Type.Cst(TypeConstructor.Complement, _), y) => Type.mkComplement(y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y) => Type.mkUnion(x, y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y) => Type.mkIntersection(x, y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.Difference, _), x, _), y) => Type.mkDifference(x, y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x, _), y) => Type.mkSymmetricDiff(x, y, loc)
      // Bool equations don't need separate canonicalization: unlike effects, these smart
      // constructors fully reduce a ground formula to a single True/False constant.
      case (Type.Cst(TypeConstructor.Not, _), y) => Type.mkNot(y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.And, _), x, _), y) => Type.mkAnd(x, y, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _), y) => Type.mkOr(x, y, loc)
      case (Type.Cst(TypeConstructor.CaseComplement(sym), _), y) => Type.mkCaseComplement(y, sym, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(sym), _), x, _), y) => Type.mkCaseIntersection(x, y, sym, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.CaseUnion(sym), _), x, _), y) => Type.mkCaseUnion(x, y, sym, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), tpe, _), rest) =>
        mkRecordExtendSorted(label, tpe, rest, loc)
      case (Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(label), _), tpe, _), rest) =>
        mkSchemaExtendSorted(label, tpe, rest, loc)
      case (x, y) => app.renew(x, y, loc)
    }
  }

  /**
    * Removes [[Type.Alias]] and [[Type.AssocType]] from `tpe`.
    *
    * N.B. Throws [[InternalCompilerException]] if `tpe` contains a non-reducible [[Type.AssocType]]
    * or an unresolved JVM type.
    *
    * N.B. If `isGround`, `tpe` is also put into canonical form. Only pass `true` once `tpe` has no
    * free type variables left.
    */
  def simplify(tpe: Type, isGround: Boolean)(implicit root: TypedAst.Root, flix: Flix): Type = tpe match {
    case Type.Var(_, _)            => tpe
    case Type.Cst(_, _)            => tpe
    case app@Type.Apply(_, _, _)   => normalizeApply(simplify(_, isGround), app, isGround)
    case Type.Alias(_, _, t, _)    => simplify(t, isGround)
    case Type.AssocType(symUse, arg0, kind, loc) =>
      val arg = simplify(arg0, isGround)
      simplify(reduceAssocType(Type.AssocType(symUse, arg, kind, loc)), isGround)
    case Type.JvmToType(_, loc)         => throw InternalCompilerException("unexpected JVM type", loc)
    case Type.JvmToEff(_, loc)          => throw InternalCompilerException("unexpected JVM eff", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
  }

  /**
    * Returns a sorted record, assuming that `rest` is sorted.
    *
    * labels of the same name are not reordered.
    *
    * N.B. `rest` must not contain [[Type.AssocType]] or [[Type.Alias]].
    */
  private def mkRecordExtendSorted(label: Name.Label, tpe: Type, rest: Type, loc: SourceLocation): Type = rest match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(l), loc1), t, loc2), r, loc3) if l.name < label.name =>
      val newRest = mkRecordExtendSorted(label, tpe, r, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(l), loc1), t, loc2), newRest, loc3)
    case Type.Cst(_, _)                => Type.mkRecordRowExtend(label, tpe, rest, loc)
    case Type.Apply(_, _, _)           => Type.mkRecordRowExtend(label, tpe, rest, loc)
    case Type.Var(_, _)                => Type.mkRecordRowExtend(label, tpe, rest, loc)
    case Type.Alias(_, _, _, _)        => throw InternalCompilerException(s"Unexpected alias '$rest'", rest.loc)
    case Type.AssocType(_, _, _, _)    => throw InternalCompilerException(s"Unexpected associated type '$rest'", rest.loc)
    case Type.JvmToType(_, _)          => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
    case Type.JvmToEff(_, _)           => throw InternalCompilerException(s"Unexpected JVM eff '$rest'", rest.loc)
    case Type.UnresolvedJvmType(_, _)  => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
  }

  /**
    * Returns a sorted schema, assuming that `rest` is sorted.
    *
    * Sorting is stable on duplicate predicates.
    *
    * N.B. `rest` must not contain variables, aliases, or associated types.
    */
  private def mkSchemaExtendSorted(label: Name.Pred, tpe: Type, rest: Type, loc: SourceLocation): Type = rest match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(l), loc1), t, loc2), r, loc3) if l.name < label.name =>
      val newRest = mkSchemaExtendSorted(label, tpe, r, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(l), loc1), t, loc2), newRest, loc3)
    case Type.Cst(_, _)                => Type.mkSchemaRowExtend(label, tpe, rest, loc)
    case Type.Apply(_, _, _)           => Type.mkSchemaRowExtend(label, tpe, rest, loc)
    case Type.Var(_, _)                => Type.mkSchemaRowExtend(label, tpe, rest, loc)
    case Type.Alias(_, _, _, _)        => throw InternalCompilerException(s"Unexpected alias '$rest'", rest.loc)
    case Type.AssocType(_, _, _, _)    => throw InternalCompilerException(s"Unexpected associated type '$rest'", rest.loc)
    case Type.JvmToType(_, _)          => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
    case Type.JvmToEff(_, _)           => throw InternalCompilerException(s"Unexpected JVM eff '$rest'", rest.loc)
    case Type.UnresolvedJvmType(_, _)  => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
  }
}
