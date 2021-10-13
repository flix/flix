/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * A well-kinded type in an easily-printable format.
  */
sealed trait SimpleType

object SimpleType {

  // Tycons

  case object Unit extends SimpleType

  case object Null extends SimpleType

  case object Bool extends SimpleType

  case object Char extends SimpleType

  case object Float32 extends SimpleType

  case object Float64 extends SimpleType

  case object Int8 extends SimpleType

  case object Int16 extends SimpleType

  case object Int32 extends SimpleType

  case object Int64 extends SimpleType

  case object BigInt extends SimpleType

  case object Str extends SimpleType

  case object Array extends SimpleType

  case object ScopedRef extends SimpleType

  case object Channel extends SimpleType

  case object Lazy extends SimpleType

  case object True extends SimpleType

  case object False extends SimpleType

  ///
  /// Compound Types.
  ///
  case object RecordRowEmpty extends SimpleType

  case object RecordEmpty extends SimpleType

  case object RecordConstructor extends SimpleType

  case class RecordRowConstructor(field: String) extends SimpleType

  // Fully-applied stuff
  case class Apply(tpe: SimpleType, tpes: List[SimpleType]) extends SimpleType

  case class Var(id: Int, text: String) extends SimpleType

  case class Tuple(length: Int, fields: List[SimpleType]) extends SimpleType

  case class PureArrow(args: List[SimpleType], ret: SimpleType) extends SimpleType

  case class ImpureArrow(args: List[SimpleType], ret: SimpleType) extends SimpleType

  case class Record(fields: List[RecordFieldType], rest: Option[SimpleType.Var]) extends SimpleType

  case class RecordRow(fields: List[RecordFieldType], rest: Option[SimpleType.Var]) extends SimpleType

  // Partially-applied stuff

  case class RecordRowHead(name: String, tpe: SimpleType) extends SimpleType

  // MATT change class name
  case class Name(name: String) extends SimpleType // MATT use only for non-builtins

  case class RecordFieldType(name: String, tpe: SimpleType)

  def fromWellKindedType(t: Type): SimpleType = t.baseType match {
    case Type.KindedVar(id, kind, loc, rigidity, text) => Var(id, text.get) // MATT how to handle vars? alpha-renaming, etc.
    case _: Type.UnkindedVar => throw InternalCompilerException("") // MATT
    case _: Type.Ascribe => throw InternalCompilerException("") // MATT
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Unit => Unit
      case TypeConstructor.Null => Null
      case TypeConstructor.Bool => Bool
      case TypeConstructor.Char => Char
      case TypeConstructor.Float32 => Float32
      case TypeConstructor.Float64 => Float64
      case TypeConstructor.Int8 => Int8
      case TypeConstructor.Int16 => Int16
      case TypeConstructor.Int32 => Int32
      case TypeConstructor.Int64 => Int64
      case TypeConstructor.BigInt => BigInt
      case TypeConstructor.Str => Str
      case TypeConstructor.Arrow(arity) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => ??? // unapplied arrow: ??? -> ??? & ???
          case True :: tpes if tpes.length == arity => ??? // fully-applied pure arrow
          case False :: tpes if tpes.length == arity => ??? // fully-applied impure arrow
          case eff :: tpes if tpes.length == arity => ??? // fully applied poly arrow
          case True :: tpes => ??? // partially applied pure arrow
          case False :: tpes => ??? // partially applied impure arrow
          case eff :: tpes => ??? // partially applied poly arrow
        }
      case TypeConstructor.RecordRowEmpty => RecordRowEmpty
      case TypeConstructor.RecordRowExtend(field) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => RecordRowConstructor(field.name)
          case tpe :: Nil => RecordRowHead(field.name, tpe)
          case _ :: _ :: Nil => fromRecordRow(t)
          case _ => ??? // MATT ICE
        }
      case TypeConstructor.Record =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => RecordConstructor
          case tpe :: Nil => tpe match {
            case RecordRowEmpty => RecordEmpty
            case RecordRow(fields, rest) => Record(fields, rest)
            case tvar: Var => Record(Nil, Some(tvar))
            case _ => ??? // MATT ICE
          }
        }
      case TypeConstructor.SchemaRowEmpty =>
      case TypeConstructor.SchemaRowExtend(pred) =>
      case TypeConstructor.Schema =>
      case TypeConstructor.Array => mkApply(Array, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Channel => mkApply(Channel, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Lazy => mkApply(Lazy, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Tag(sym, tag) => ??? // MATT ?
      case TypeConstructor.KindedEnum(sym, kind) => mkApply(Name(sym.name), t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.UnkindedEnum(sym) => // MATT ICE
      case TypeConstructor.Native(clazz) => ??? // MATT ?
      case TypeConstructor.ScopedRef => mkApply(ScopedRef, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Tuple(l) =>
      case TypeConstructor.Relation =>
      case TypeConstructor.Lattice =>
      case TypeConstructor.True =>
      case TypeConstructor.False =>
      case TypeConstructor.Not =>
      case TypeConstructor.And =>
      case TypeConstructor.Or =>
      case TypeConstructor.Region =>
    }

    case Type.Lambda(tvar, tpe, loc) =>
  }

  private def mkApply(base: SimpleType, args: List[SimpleType]): SimpleType = args match {
    case Nil => base
    case _ :: _ => Apply(base, args)
  }

  // MATT docs
  private def fromRecordRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // MATT case docs
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(name), _), tpe, _), rest, _) =>
        val fieldType = RecordFieldType(name.name, fromWellKindedType(tpe))
        fromRecordRow(rest) match {
          case SimpleType.RecordRowEmpty => SimpleType.RecordRow(fieldType :: Nil, None)
          case SimpleType.RecordRow(fields, rest) => SimpleType.RecordRow(fieldType :: fields, rest) // MATT shadow
          case tvar: SimpleType.Var => SimpleType.RecordRow(fieldType :: Nil, Some(tvar))
          case _ => ??? // MATT ICE
        }
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) => SimpleType.RecordRowEmpty
      case Type.KindedVar(id, kind, loc, rigidity, text) => SimpleType.Var(id, text.get) // MATT handle no text
      case _ => ??? // MATT ICE
    }

    // sort the fields after converting
    visit(row0) match {
      case RecordRowEmpty => RecordRowEmpty
      case RecordRow(fields, rest) => RecordRow(fields.sortBy(_.name), rest)
      case _ => ??? // MATT ICE (do I need var here?)
    }
  }
}
