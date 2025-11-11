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

import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, TraitConstraint, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException

object Serialize {

  def serializeDef(defn0: TypedAst.Def): SDef = defn0 match {
    case TypedAst.Def(sym, spec, _, loc) =>
      val ns = sym.namespace
      val text = sym.name
      val sscheme = serializeSpec(spec)
      val source = loc.source.name
      SDef(ns, text, sscheme, source)
  }

  private def serializeSpec(spec0: TypedAst.Spec): SScheme = spec0 match {
    case TypedAst.Spec(_, _, _, _, _, Scheme(quantifiers, tconstrs, econstrs, base), _, _, _, _) =>
      val qs = quantifiers.map(serializeKindedTypeVarSym)
      val tcs = tconstrs.map(serializeTraitConstraint)
      val ecs = econstrs.map(serializeEqualityConstraint)
      val b = serializeType(base)
      SScheme(qs, tcs, ecs, b)
  }

  private def serializeType(tpe0: Type): SType = tpe0 match {
    case Type.Var(sym, _) => Var(serializeKindedTypeVarSym(sym))
    case Type.Cst(tc, _) => Cst(serializeTypeConstructor(tc))
    case Type.Apply(tpe1, tpe2, _) => Apply(serializeType(tpe1), serializeType(tpe2))
    case Type.Alias(symUse, args, tpe, _) => Alias(serializeTypeAliasSym(symUse.sym), args.map(serializeType), serializeType(tpe))
    case Type.AssocType(symUse, arg, kind, _) => AssocType(serializeAssocTypeSym(symUse.sym), serializeType(arg), serializeKind(kind))
    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JvmToType", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JvmToEff", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected UnresolvedJvmType", loc)
  }

  private def serializeTypeConstructor(tc0: TypeConstructor): STC = tc0 match {
    case TypeConstructor.Void => Void
    case TypeConstructor.AnyType => AnyType
    case TypeConstructor.Unit => Unit
    case TypeConstructor.Null => Null
    case TypeConstructor.Bool => Bool
    case TypeConstructor.Char => Char
    case TypeConstructor.Float32 => Float32
    case TypeConstructor.Float64 => Float64
    case TypeConstructor.BigDecimal => BigDecimal
    case TypeConstructor.Int8 => Int8
    case TypeConstructor.Int16 => Int16
    case TypeConstructor.Int32 => Int32
    case TypeConstructor.Int64 => Int64
    case TypeConstructor.BigInt => BigInt
    case TypeConstructor.Str => Str
    case TypeConstructor.Regex => Regex
    case TypeConstructor.Arrow(arity) => Arrow(arity)
    case TypeConstructor.ArrowWithoutEffect(arity) => ArrowWithoutEffect(arity)
    case TypeConstructor.RecordRowEmpty => RecordRowEmpty
    case TypeConstructor.RecordRowExtend(label) => RecordRowExtend(label.name)
    case TypeConstructor.Record => Record
    case TypeConstructor.Extensible => Extensible
    case TypeConstructor.SchemaRowEmpty => SchemaRowEmpty
    case TypeConstructor.SchemaRowExtend(pred) => SchemaRowExtend(pred.name)
    case TypeConstructor.Schema => Schema
    case TypeConstructor.Sender => Sender
    case TypeConstructor.Receiver => Receiver
    case TypeConstructor.Lazy => Lazy
    case TypeConstructor.Enum(sym, kind) => Enum(serializeEnumSym(sym), serializeKind(kind))
    case TypeConstructor.Struct(sym, kind) => Struct(serializeStructSym(sym), serializeKind(kind))
    case TypeConstructor.RestrictableEnum(sym, kind) => RestrictableEnum(serializeRestrictableEnumSym(sym), serializeKind(kind))
    case TypeConstructor.Native(clazz) => Native(clazz.descriptorString())
    case TypeConstructor.JvmConstructor(constructor) => JvmConstructor(constructor.toGenericString)
    case TypeConstructor.JvmMethod(method) => JvmMethod(method.toGenericString)
    case TypeConstructor.JvmField(field) => JvmField(field.toGenericString)
    case TypeConstructor.Array => Array
    case TypeConstructor.ArrayWithoutRegion => ArrayWithoutRegion
    case TypeConstructor.Vector => Vector
    case TypeConstructor.Tuple(arity) => Tuple(arity)
    case TypeConstructor.Relation(arity) => Relation(arity)
    case TypeConstructor.Lattice(arity) => Lattice(arity)
    case TypeConstructor.True => True
    case TypeConstructor.False => False
    case TypeConstructor.Not => Not
    case TypeConstructor.And => And
    case TypeConstructor.Or => Or
    case TypeConstructor.Pure => Pure
    case TypeConstructor.Univ => Univ
    case TypeConstructor.Complement => Complement
    case TypeConstructor.Union => Union
    case TypeConstructor.Intersection => Intersection
    case TypeConstructor.Difference => Difference
    case TypeConstructor.SymmetricDiff => SymmetricDiff
    case TypeConstructor.Effect(sym, kind) => Effect(serializeEffSym(sym), serializeKind(kind))
    case TypeConstructor.CaseComplement(sym) => CaseComplement(serializeRestrictableEnumSym(sym))
    case TypeConstructor.CaseUnion(sym) => CaseUnion(serializeRestrictableEnumSym(sym))
    case TypeConstructor.CaseIntersection(sym) => CaseIntersection(serializeRestrictableEnumSym(sym))
    case TypeConstructor.CaseSymmetricDiff(sym) => CaseSymmetricDiff(serializeRestrictableEnumSym(sym))
    case TypeConstructor.CaseSet(syms, enumSym) => CaseSet(syms.toList.map(serializeRestrictableCaseSym), serializeRestrictableEnumSym(enumSym))
    case TypeConstructor.Region(sym) => Region(serializeRegionSym(sym))
    case TypeConstructor.RegionToStar => RegionToStar
    case TypeConstructor.RegionWithoutRegion => RegionWithoutRegion
    case TypeConstructor.Error(_, _) => throw InternalCompilerException("unexpected error tc in serialization", SourceLocation.Unknown)
  }

  private def serializeKind(kind0: Kind): SKind = kind0 match {
    case Kind.Wild => WildKind
    case Kind.WildCaseSet => WildCaseSetKind
    case Kind.Star => StarKind
    case Kind.Eff => EffKind
    case Kind.Bool => BoolKind
    case Kind.RecordRow => RecordRowKind
    case Kind.SchemaRow => SchemaRowKind
    case Kind.Predicate => PredicateKind
    case Kind.Jvm => JvmKind
    case Kind.CaseSet(sym) => CaseSetKind(serializeRestrictableEnumSym(sym))
    case Kind.Arrow(k1, k2) => ArrowKind(serializeKind(k1), serializeKind(k2))
    case Kind.Error => throw InternalCompilerException("unexpected error kind in serialization", SourceLocation.Unknown)
  }

  private def serializeAssocTypeSym(sym0: Symbol.AssocTypeSym): AssocTypeSym = {
    AssocTypeSym(serializeTraitSym(sym0.trt), sym0.name)
  }

  private def serializeEffSym(sym0: Symbol.EffSym): EffSym = {
    EffSym(sym0.namespace, sym0.name)
  }

  private def serializeEnumSym(sym0: Symbol.EnumSym): EnumSym = {
    EnumSym(sym0.namespace, sym0.text)
  }

  private def serializeKindedTypeVarSym(sym0: Symbol.KindedTypeVarSym): VarSym = {
    VarSym(serializeVarText(sym0.text), serializeKind(sym0.kind))
  }

  private def serializeRegionSym(sym0: Symbol.RegionSym): RegionSym = {
    RegionSym(sym0.text)
  }

  private def serializeRestrictableCaseSym(sym0: Symbol.RestrictableCaseSym): RestrictableCaseSym = {
    RestrictableCaseSym(serializeRestrictableEnumSym(sym0.enumSym), sym0.name)
  }

  private def serializeRestrictableEnumSym(sym0: Symbol.RestrictableEnumSym): RestrictableEnumSym = {
    RestrictableEnumSym(sym0.namespace, sym0.name, sym0.universe.toList.map(_.name))
  }

  private def serializeStructSym(sym0: Symbol.StructSym): StructSym = {
    StructSym(sym0.namespace, sym0.text)
  }

  private def serializeTraitSym(sym0: Symbol.TraitSym): TraitSym = {
    TraitSym(sym0.namespace, sym0.name)
  }

  private def serializeTypeAliasSym(sym0: Symbol.TypeAliasSym): TypeAliasSym = {
    TypeAliasSym(sym0.namespace, sym0.name)
  }

  private def serializeVarText(text0: VarText): SVarText = text0 match {
    case VarText.Absent => Absent
    case VarText.SourceText(s) => Text(s)
  }

  private def serializeTraitConstraint(tconstr0: TraitConstraint): TraitConstr = {
    TraitConstr(serializeTraitSym(tconstr0.symUse.sym), serializeType(tconstr0.arg))
  }

  private def serializeEqualityConstraint(econstr0: EqualityConstraint): EqConstr = {
    EqConstr(serializeAssocTypeSym(econstr0.symUse.sym), serializeType(econstr0.tpe1), serializeType(econstr0.tpe2))
  }
}
