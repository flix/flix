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

package object serialization {

  /**
    * Represents a serializable type constructor (STC).
    */
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

  case class RecordRowExtend(label: Name.Label) extends STC

  case object Record extends STC

  case object Extensible extends STC

  case object SchemaRowEmpty extends STC

  case class SchemaRowExtend(pred: Name.Pred) extends STC

  case object Schema extends STC

  case object Sender extends STC

  case object Receiver extends STC

  case object Lazy extends STC

  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends TypeConstructor

  case class Struct(sym: Symbol.StructSym, kind: Kind) extends TypeConstructor

  case class RestrictableEnum(sym: Symbol.RestrictableEnumSym, kind: Kind) extends TypeConstructor

  case class Native(clazz: Class[?]) extends STC

  case class JvmConstructor(constructor: Constructor[?]) extends STC

  case class JvmMethod(method: Method) extends STC

  case class JvmField(field: Field) extends STC

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

  case class Effect(sym: Symbol.EffSym, kind: Kind) extends TypeConstructor

  case class CaseComplement(sym: Symbol.RestrictableEnumSym) extends STC

  case class CaseUnion(sym: Symbol.RestrictableEnumSym) extends STC

  case class CaseIntersection(sym: Symbol.RestrictableEnumSym) extends STC

  case class CaseSymmetricDiff(sym: Symbol.RestrictableEnumSym) extends STC

  case class CaseSet(syms: SortedSet[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym) extends STC

  case class Region(sym: Symbol.RegionSym) extends STC

  case object RegionToStar extends STC

  case object RegionWithoutRegion extends STC

  case class Error(id: Int, kind: Kind) extends TypeConstructor
}

