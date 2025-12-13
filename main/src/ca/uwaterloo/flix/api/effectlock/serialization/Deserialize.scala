/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock.serialization

import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TraitSymUse}
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, Scope, TraitConstraint, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor}

import scala.collection.immutable.SortedSet

object Deserialize {

  def deserializeDef(defn0: SDef): (Symbol.DefnSym, Scheme) = defn0 match {
    case SDef(namespace, text, scheme) =>
      val ns = namespace.mkString(".")
      val t = if (ns.isBlank || ns.isEmpty) text else s".$text"
      val sym = Symbol.mkDefnSym(ns + t)
      val sc = deserializeScheme(scheme)
      sym -> sc
  }

  private def deserializeScheme(sc0: SScheme): Scheme = sc0 match {
    case SScheme(quantifiers, tconstrs, econstrs, base) =>
      val qs = quantifiers.map(deserializeKindedTypeVarSym)
      Scheme(qs, tconstrs.map(deserializeTraitConstraint), econstrs.map(deserializeEqualityConstraint), deserializeType(base))
  }

  private def deserializeType(tpe0: SType): Type = tpe0 match {
    case Var(sym) => Type.Var(deserializeKindedTypeVarSym(sym), SourceLocation.Unknown)
    case Cst(tc) => Type.Cst(deserializeTypeConstructor(tc), SourceLocation.Unknown)
    case Apply(tpe1, tpe2) => Type.Apply(deserializeType(tpe1), deserializeType(tpe2), SourceLocation.Unknown)
    case AssocType(symUse, arg, kind) => Type.AssocType(deserializeAssocTypeSymUse(symUse), deserializeType(arg), deserializeKind(kind), SourceLocation.Unknown)
  }

  private def deserializeTypeConstructor(tc0: STC): TypeConstructor = tc0 match {
    case Void => TypeConstructor.Void
    case AnyType => TypeConstructor.AnyType
    case Unit => TypeConstructor.Unit
    case Null => TypeConstructor.Null
    case Bool => TypeConstructor.Bool
    case Char => TypeConstructor.Char
    case Float32 => TypeConstructor.Float32
    case Float64 => TypeConstructor.Float64
    case BigDecimal => TypeConstructor.BigDecimal
    case Int8 => TypeConstructor.Int8
    case Int16 => TypeConstructor.Int16
    case Int32 => TypeConstructor.Int32
    case Int64 => TypeConstructor.Int64
    case BigInt => TypeConstructor.BigInt
    case Str => TypeConstructor.Str
    case Regex => TypeConstructor.Regex
    case Arrow(arity) => TypeConstructor.Arrow(arity)
    case ArrowWithoutEffect(arity) => TypeConstructor.ArrowWithoutEffect(arity)
    case RecordRowEmpty => TypeConstructor.RecordRowEmpty
    case RecordRowExtend(label) => TypeConstructor.RecordRowExtend(deserializeLabel(label))
    case Record => TypeConstructor.Record
    case Extensible => TypeConstructor.Extensible
    case SchemaRowEmpty => TypeConstructor.SchemaRowEmpty
    case SchemaRowExtend(pred) => TypeConstructor.SchemaRowExtend(deserializePred(pred))
    case Schema => TypeConstructor.Schema
    case Sender => TypeConstructor.Sender
    case Receiver => TypeConstructor.Receiver
    case Lazy => TypeConstructor.Lazy
    case Enum(sym, kind) => TypeConstructor.Enum(deserializeEnumSym(sym), deserializeKind(kind))
    case Struct(sym, kind) => TypeConstructor.Struct(deserializeStructSym(sym), deserializeKind(kind))
    case RestrictableEnum(sym, kind) => TypeConstructor.RestrictableEnum(deserializeRestrictableEnumSym(sym), deserializeKind(kind))
    case Native(clazz) => TypeConstructor.Native(deserializeJvmClass(Native(clazz)))
    case Array => TypeConstructor.Array
    case ArrayWithoutRegion => TypeConstructor.ArrayWithoutRegion
    case Vector => TypeConstructor.Vector
    case Tuple(arity) => TypeConstructor.Tuple(arity)
    case Relation(arity) => TypeConstructor.Relation(arity)
    case Lattice(arity) => TypeConstructor.Lattice(arity)
    case True => TypeConstructor.True
    case False => TypeConstructor.False
    case Not => TypeConstructor.Not
    case And => TypeConstructor.And
    case Or => TypeConstructor.Or
    case Pure => TypeConstructor.Pure
    case Univ => TypeConstructor.Univ
    case Complement => TypeConstructor.Complement
    case Union => TypeConstructor.Union
    case Intersection => TypeConstructor.Intersection
    case Difference => TypeConstructor.Difference
    case SymmetricDiff => TypeConstructor.SymmetricDiff
    case Effect(sym, kind) => TypeConstructor.Effect(deserializeEffectSym(sym), deserializeKind(kind))
    case CaseComplement(sym) => TypeConstructor.CaseComplement(deserializeRestrictableEnumSym(sym))
    case CaseUnion(sym) => TypeConstructor.CaseUnion(deserializeRestrictableEnumSym(sym))
    case CaseIntersection(sym) => TypeConstructor.CaseIntersection(deserializeRestrictableEnumSym(sym))
    case CaseSymmetricDiff(sym) => TypeConstructor.CaseSymmetricDiff(deserializeRestrictableEnumSym(sym))
    case CaseSet(syms, enumSym) => TypeConstructor.CaseSet(SortedSet.from(syms.map(deserializeRestrictableCaseSym)), deserializeRestrictableEnumSym(enumSym))
    case Region(sym) => TypeConstructor.Region(deserializeRegionSym(sym))
    case RegionToStar => TypeConstructor.RegionToStar
    case RegionWithoutRegion => TypeConstructor.RegionWithoutRegion
  }

  private def deserializeKind(kind0: SKind): Kind = kind0 match {
    case WildKind => Kind.Wild
    case WildCaseSetKind => Kind.WildCaseSet
    case StarKind => Kind.Star
    case EffKind => Kind.Eff
    case BoolKind => Kind.Bool
    case RecordRowKind => Kind.RecordRow
    case SchemaRowKind => Kind.SchemaRow
    case PredicateKind => Kind.Predicate
    case CaseSetKind(sym) => Kind.CaseSet(deserializeRestrictableEnumSym(sym))
    case ArrowKind(k1, k2) => Kind.Arrow(deserializeKind(k1), deserializeKind(k2))
  }

  private def deserializeAssocTypeSym(sym0: AssocTypeSym): Symbol.AssocTypeSym = sym0 match {
    case AssocTypeSym(trt, name) =>
      Symbol.mkAssocTypeSym(deserializeTraitSym(trt), deserializeIdent(name))
  }

  private def deserializeAssocTypeSymUse(symUse0: AssocTypeSym): AssocTypeSymUse = {
    AssocTypeSymUse(deserializeAssocTypeSym(symUse0), SourceLocation.Unknown)
  }

  private def deserializeEffectSym(sym0: EffSym): Symbol.EffSym = sym0 match {
    case EffSym(namespace, name) =>
      Symbol.mkEffSym(deserializeNamespace(namespace), deserializeIdent(name))
  }

  private def deserializeEnumSym(sym0: EnumSym): Symbol.EnumSym = sym0 match {
    case EnumSym(namespace, text) =>
      Symbol.mkEnumSym(deserializeNamespace(namespace), deserializeIdent(text))
  }

  private def deserializeJvmClass(clazz0: Native): Class[?] = clazz0.clazz match {
    case "B" => Byte.getClass
    case "C" => Char.getClass
    case "D" => Double.getClass
    case "F" => Float.getClass
    case "I" => Int.getClass
    case "J" => Long.getClass
    case "S" => Short.getClass
    case "Z" => Boolean.getClass
    case clazz => Class.forName(clazz)
  }

  private def deserializeKindedTypeVarSym(sym0: VarSym): Symbol.KindedTypeVarSym = sym0 match {
    case VarSym(id, text, kind) =>
      val t = deserializeVarText(text)
      val k = deserializeKind(kind)
      new Symbol.KindedTypeVarSym(id, t, k, isSlack = false, Scope.Top, SourceLocation.Unknown)
  }

  private def deserializeLabel(label0: String): Name.Label = {
    Name.Label(label0, SourceLocation.Unknown)
  }

  private def deserializePred(pred0: String): Name.Pred = {
    Name.Pred(pred0, SourceLocation.Unknown)
  }

  private def deserializeRegionSym(sym0: RegionSym): Symbol.RegionSym = sym0 match {
    case RegionSym(id, text) =>
      val ident = deserializeIdent(text)
      new Symbol.RegionSym(id, ident.name, ident.loc)
  }

  private def deserializeRestrictableEnumSym(sym0: RestrictableEnumSym): Symbol.RestrictableEnumSym = sym0 match {
    case RestrictableEnumSym(ns, name, cases) =>
      Symbol.mkRestrictableEnumSym(deserializeNamespace(ns), deserializeIdent(name), cases.map(deserializeIdent))
  }

  private def deserializeRestrictableCaseSym(sym0: RestrictableCaseSym): Symbol.RestrictableCaseSym = sym0 match {
    case RestrictableCaseSym(enumSym, name) =>
      Symbol.mkRestrictableCaseSym(deserializeRestrictableEnumSym(enumSym), deserializeIdent(name))
  }

  private def deserializeStructSym(sym0: StructSym): Symbol.StructSym = sym0 match {
    case StructSym(namespace, text) =>
      Symbol.mkStructSym(deserializeNamespace(namespace), deserializeIdent(text))
  }

  private def deserializeTraitSym(sym0: TraitSym): Symbol.TraitSym = sym0 match {
    case TraitSym(namespace, name) =>
      Symbol.mkTraitSym(deserializeNamespace(namespace), deserializeIdent(name))
  }

  private def deserializeVarText(text0: SVarText): VarText = text0 match {
    case Absent => VarText.Absent
    case Text(s) => VarText.SourceText(s)
  }

  private def deserializeIdent(ident0: String): Name.Ident = {
    Name.Ident(ident0, SourceLocation.Unknown)
  }

  private def deserializeNamespace(ns0: List[String]): Name.NName = {
    Name.NName(ns0.map(deserializeIdent), SourceLocation.Unknown)
  }

  private def deserializeEqualityConstraint(econstr0: EqConstr): EqualityConstraint = econstr0 match {
    case EqConstr(sym, tpe1, tpe2) =>
      EqualityConstraint(deserializeAssocTypeSymUse(sym), deserializeType(tpe1), deserializeType(tpe2), SourceLocation.Unknown)
  }

  private def deserializeTraitConstraint(tconstr0: TraitConstr): TraitConstraint = tconstr0 match {
    case TraitConstr(sym, tpe) =>
      TraitConstraint(TraitSymUse(deserializeTraitSym(sym), SourceLocation.Unknown), deserializeType(tpe), SourceLocation.Unknown)
  }

}
