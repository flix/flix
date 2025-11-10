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
package ca.uwaterloo.flix.api.effectlock

/**
  * Package object for the serialization package.
  * The serializable AST is defined in this package object, so it becomes
  * available to the entire package while keeping names short, i.e., they
  * do not have to be qualified via their parent objects, e.g., `SType$Var`.
  */
package object serialization {

  /** Represents a serializable def. */
  case class SDef(namespace: List[String], text: String, scheme: SScheme, source: String)

  /** Represents a serializable scheme. */
  case class SScheme(quantifiers: List[VarSym], tconstrs: List[TraitConstr], econstrs: List[EqConstr], base: SType)

  /** Represents a serializable type. */
  sealed trait SType

  case class Var(sym: VarSym) extends SType

  case class Cst(tc: STC) extends SType

  case class Apply(tpe1: SType, tpe2: SType) extends SType

  case class Alias(symUse: TypeAliasSym, args: List[SType], tpe: SType) extends SType

  case class AssocType(symUse: AssocTypeSym, arg: SType, kind: SKind) extends SType

  /** Represents a serializable type constructor (STC). */
  sealed trait STC

  case object Void extends STC

  case object AnyType extends STC

  case object Unit extends STC

  case object Null extends STC

  case object Bool extends STC

  case object Char extends STC

  case object Float32 extends STC

  case object Float64 extends STC

  case object BigDecimal extends STC

  case object Int8 extends STC

  case object Int16 extends STC

  case object Int32 extends STC

  case object Int64 extends STC

  case object BigInt extends STC

  case object Str extends STC

  case object Regex extends STC

  case class Arrow(arity: Int) extends STC

  case class ArrowWithoutEffect(arity: Int) extends STC

  case object RecordRowEmpty extends STC

  case class RecordRowExtend(label: String) extends STC

  case object Record extends STC

  case object Extensible extends STC

  case object SchemaRowEmpty extends STC

  case class SchemaRowExtend(pred: String) extends STC

  case object Schema extends STC

  case object Sender extends STC

  case object Receiver extends STC

  case object Lazy extends STC

  case class Enum(sym: EnumSym, kind: SKind) extends STC

  case class Struct(sym: StructSym, kind: SKind) extends STC

  case class RestrictableEnum(sym: RestrictableEnumSym, kind: SKind) extends STC

  case class Native(clazz: String) extends STC

  case class JvmConstructor(name: String) extends STC

  case class JvmMethod(method: String) extends STC

  case class JvmField(field: String) extends STC

  case object Array extends STC

  case object ArrayWithoutRegion extends STC

  case object Vector extends STC

  case class Tuple(arity: Int) extends STC

  case class Relation(arity: Int) extends STC

  case class Lattice(arity: Int) extends STC

  case object True extends STC

  case object False extends STC

  case object Not extends STC

  case object And extends STC

  case object Or extends STC

  case object Pure extends STC

  case object Univ extends STC

  case object Complement extends STC

  case object Union extends STC

  case object Intersection extends STC

  case object Difference extends STC

  case object SymmetricDiff extends STC

  case class Effect(sym: EffSym, kind: SKind) extends STC

  case class CaseComplement(sym: RestrictableEnumSym) extends STC

  case class CaseUnion(sym: RestrictableEnumSym) extends STC

  case class CaseIntersection(sym: RestrictableEnumSym) extends STC

  case class CaseSymmetricDiff(sym: RestrictableEnumSym) extends STC

  case class CaseSet(syms: List[RestrictableCaseSym], enumSym: RestrictableEnumSym) extends STC

  case class Region(sym: RegionSym) extends STC

  case object RegionToStar extends STC

  case object RegionWithoutRegion extends STC

  /** Represents a serializable kind. */
  sealed trait SKind

  case object WildKind extends SKind

  case object WildCaseSetKind extends SKind

  case object StarKind extends SKind

  case object EffKind extends SKind

  case object BoolKind extends SKind

  case object RecordRowKind extends SKind

  case object SchemaRowKind extends SKind

  case object PredicateKind extends SKind

  case object JvmKind extends SKind

  case class CaseSetKind(sym: RestrictableEnumSym) extends SKind

  case class ArrowKind(k1: SKind, k2: SKind) extends SKind

  /** Represents a serializable symbol. */
  sealed trait SSym

  case class VarSym(text: SVarText, kind: SKind) extends SSym

  case class TypeAliasSym(namespace: List[String], name: String) extends SSym

  case class AssocTypeSym(trt: TraitSym, name: String) extends SSym

  case class TraitSym(namespace: List[String], name: String) extends SSym

  case class EnumSym(namespace: List[String], text: String) extends SSym

  case class CaseSym(enumSym: EnumSym, name: String) extends SSym

  case class EffSym(namespace: List[String], name: String) extends SSym

  case class RegionSym(text: String) extends SSym

  case class RestrictableEnumSym(namespace: List[String], name: String, cases: List[String]) extends SSym

  case class RestrictableCaseSym(enumSym: RestrictableEnumSym, name: String) extends SSym

  case class StructSym(namespace: List[String], text: String) extends SSym

  /** Represents serializable VarText. */
  sealed trait SVarText

  case object Absent extends SVarText

  case class Text(s: String) extends SVarText

  case class TraitConstr(sym: TraitSym, tpe: SType)

  case class EqConstr(sym: AssocTypeSym, tpe1: SType, tpe2: SType)

  /** Implicitly defines type hints for json4s for each of the serializable constructors. */
  implicit val formats: org.json4s.Formats = org.json4s.native.Serialization.formats(
    org.json4s.ShortTypeHints(
      List(
        classOf[SDef],
        classOf[SScheme],
        classOf[Var],
        classOf[Cst],
        classOf[Apply],
        classOf[Alias],
        classOf[AssocType],
        Void.getClass,
        AnyType.getClass,
        Unit.getClass,
        Null.getClass,
        Bool.getClass,
        Char.getClass,
        Float32.getClass,
        Float64.getClass,
        BigDecimal.getClass,
        Int8.getClass,
        Int16.getClass,
        Int32.getClass,
        Int64.getClass,
        BigInt.getClass,
        Str.getClass,
        Regex.getClass,
        classOf[Arrow],
        classOf[ArrowWithoutEffect],
        RecordRowEmpty.getClass,
        classOf[RecordRowExtend],
        Record.getClass,
        Extensible.getClass,
        SchemaRowEmpty.getClass,
        classOf[SchemaRowExtend],
        Schema.getClass,
        Sender.getClass,
        Receiver.getClass,
        Lazy.getClass,
        classOf[Enum],
        classOf[Struct],
        classOf[RestrictableEnum],
        classOf[Native],
        classOf[JvmConstructor],
        classOf[JvmMethod],
        classOf[JvmField],
        Array.getClass,
        ArrayWithoutRegion.getClass,
        Vector.getClass,
        classOf[Tuple],
        classOf[Relation],
        classOf[Lattice],
        True.getClass,
        False.getClass,
        Not.getClass,
        And.getClass,
        Or.getClass,
        Pure.getClass,
        Univ.getClass,
        Complement.getClass,
        Union.getClass,
        Intersection.getClass,
        Difference.getClass,
        SymmetricDiff.getClass,
        classOf[Effect],
        classOf[CaseComplement],
        classOf[CaseUnion],
        classOf[CaseIntersection],
        classOf[CaseSymmetricDiff],
        classOf[CaseSet],
        classOf[Region],
        RegionToStar.getClass,
        RegionWithoutRegion.getClass,
        WildKind.getClass,
        WildCaseSetKind.getClass,
        StarKind.getClass,
        EffKind.getClass,
        BoolKind.getClass,
        RecordRowKind.getClass,
        SchemaRowKind.getClass,
        PredicateKind.getClass,
        JvmKind.getClass,
        classOf[CaseSetKind],
        classOf[ArrowKind],
        classOf[VarSym],
        classOf[TypeAliasSym],
        classOf[AssocTypeSym],
        classOf[TraitSym],
        classOf[EnumSym],
        classOf[CaseSym],
        classOf[EffSym],
        classOf[RegionSym],
        classOf[RestrictableEnumSym],
        classOf[RestrictableCaseSym],
        classOf[StructSym],
        Absent.getClass,
        classOf[Text],
        classOf[TraitConstr],
        classOf[EqConstr],
      )))
}

