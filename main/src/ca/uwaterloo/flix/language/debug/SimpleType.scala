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

  case object Unit
  // MATT ... primitives
  // MATT other stuff

  case object True extends SimpleType

  case object False extends SimpleType

  case object RecordRowEmpty extends SimpleType

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
  case class Name(name: String) extends SimpleType

  case class RecordFieldType(name: String, tpe: SimpleType)

  def fromWellKindedType(t: Type): SimpleType = t.baseType match {
    case Type.KindedVar(id, kind, loc, rigidity, text) => Var(id, text.get) // MATT how to handle vars? alpha-renaming, etc.
    case _: Type.UnkindedVar => throw InternalCompilerException("") // MATT
    case _: Type.Ascribe => throw InternalCompilerException("") // MATT
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Unit => Name("Unit")
      case TypeConstructor.Null => Name("Null")
      case TypeConstructor.Bool => Name("Bool")
      case TypeConstructor.Char => Name("Char")
      case TypeConstructor.Float32 => Name("Float32")
      case TypeConstructor.Float64 => Name("Float64")
      case TypeConstructor.Int8 => Name("Int8")
      case TypeConstructor.Int16 => Name("Int16")
      case TypeConstructor.Int32 => Name("Int32")
      case TypeConstructor.Int64 => Name("Int64")
      case TypeConstructor.BigInt => Name("BigInt")
      case TypeConstructor.Str => Name("String")
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
        // MATT flatten somehow
        // MATT then go by case
        // MATT alphabetize fields
      case TypeConstructor.Record =>
      case TypeConstructor.SchemaRowEmpty =>
      case TypeConstructor.SchemaRowExtend(pred) =>
      case TypeConstructor.Schema =>
      case TypeConstructor.Array =>
      case TypeConstructor.Channel =>
      case TypeConstructor.Lazy =>
      case TypeConstructor.Tag(sym, tag) =>
      case TypeConstructor.KindedEnum(sym, kind) =>
      case TypeConstructor.UnkindedEnum(sym) =>
      case TypeConstructor.Native(clazz) =>
      case TypeConstructor.ScopedRef =>
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
}
