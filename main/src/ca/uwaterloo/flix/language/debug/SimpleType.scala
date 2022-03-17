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

  private val IllKindedException = InternalCompilerException("Unexpected ill-kinded type")

  // MATT add examples for anything nontrivial

  // Hole

  case object Hole extends SimpleType

  // Primitives

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

  case object ScopedArray extends SimpleType

  case object ScopedRef extends SimpleType

  case object Channel extends SimpleType

  case object Lazy extends SimpleType

  case object True extends SimpleType

  case object False extends SimpleType

  case object Region extends SimpleType

  // Records

  case class Record(fields: List[FieldType], rest: Option[SimpleType.Var]) extends SimpleType

  case class RecordRow(fields: List[FieldType], rest: Option[SimpleType.Var]) extends SimpleType

  case object RecordRowEmpty extends SimpleType

  case object RecordEmpty extends SimpleType

  case object RecordConstructor extends SimpleType

  case class RecordRowConstructor(field: String) extends SimpleType

  case class RecordRowHead(name: String, tpe: SimpleType) extends SimpleType

  // Schemas

  case class Schema(fields: List[FieldType], rest: Option[SimpleType.Var]) extends SimpleType

  case class SchemaRow(fields: List[FieldType], rest: Option[SimpleType.Var]) extends SimpleType

  case object SchemaRowEmpty extends SimpleType

  case object SchemaEmpty extends SimpleType

  case object SchemaConstructor extends SimpleType

  case class SchemaRowConstructor(field: String) extends SimpleType

  case class SchemaRowHead(name: String, tpe: SimpleType) extends SimpleType

  // Boolean Operators

  case class Not(tpe: SimpleType) extends SimpleType

  // MATT use Hole for all the partial stuff
  case class And(tpes: List[SimpleType]) extends SimpleType

  case class Or(tpes: List[SimpleType]) extends SimpleType

  // Relations and Lattices

  case object RelationConstructor extends SimpleType

  case class Relation(tpes: List[SimpleType]) extends SimpleType

  case object LatticeConstructor extends SimpleType

  case class Lattice(tpes: List[SimpleType]) extends SimpleType

  // Arrow Stuff

  case class PureArrow(arg: SimpleType, ret: SimpleType) extends SimpleType

  case class PolyArrow(arg: SimpleType, eff: SimpleType, ret: SimpleType) extends SimpleType

  // Tag Stuff

  case class TagConstructor(name: String) extends SimpleType

  case class Tag(name: String, args: List[SimpleType], ret: SimpleType) extends SimpleType

  // Auxiliary Types

  case class Name(name: String) extends SimpleType

  case class FieldType(name: String, tpe: SimpleType)

  case class Apply(tpe: SimpleType, tpes: List[SimpleType]) extends SimpleType

  case class Var(id: Int) extends SimpleType

  case class Tuple(length: Int, fields: List[SimpleType]) extends SimpleType


  /**
    * Creates a simple type from the well-kinded type `t`.
   */
  def fromWellKindedType(t: Type): SimpleType = t.baseType match {
    case Type.KindedVar(id, kind, loc, rigidity, text) => Var(id) // MATT ignoring name
    case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.Ascribe => throw InternalCompilerException("Unexpected kind ascription.")
    case Type.Alias(cst, args, tpe, loc) => Apply(Name(cst.sym.name), args.map(fromWellKindedType))
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
          case Nil => PolyArrow(Hole, Hole, Hole)
          case True :: tpes =>
            // NB: safe to reduce because arity is always at least 2
            tpes.padTo(arity, Hole).reduceRight(PureArrow)
          case eff :: tpes =>
            // NB: safe to take last 2 because arity is always at least 2
            val List(lastArg, ret) = tpes.padTo(arity, Hole).takeRight(2)
            val lastArrow: SimpleType = PolyArrow(lastArg, eff, ret)
            tpes.dropRight(2).foldRight(lastArrow)(PureArrow)
        }
      case TypeConstructor.RecordRowEmpty => RecordRowEmpty
      case TypeConstructor.RecordRowExtend(field) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => RecordRowConstructor(field.name)
          case tpe :: Nil => RecordRowHead(field.name, tpe)
          case _ :: _ :: Nil => fromRecordRow(t)
          case _ => throw IllKindedException
        }
      case TypeConstructor.Record =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => RecordConstructor
          case tpe :: Nil => tpe match {
            case RecordRowEmpty => RecordEmpty
            case RecordRow(fields, rest) => Record(fields, rest)
            case tvar: Var => Record(Nil, Some(tvar))
            case _ => throw IllKindedException
          }
          case _ => throw IllKindedException
        }
      case TypeConstructor.SchemaRowEmpty => SchemaRowEmpty
      case TypeConstructor.SchemaRowExtend(pred) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => SchemaRowConstructor(pred.name)
          case tpe :: Nil => SchemaRowHead(pred.name, tpe)
          case _ :: _ :: Nil => fromSchemaRow(t)
          case _ => throw IllKindedException
        }
      case TypeConstructor.Schema =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => SchemaConstructor
          case tpe :: Nil => tpe match {
            case SchemaRowEmpty => SchemaEmpty
            case SchemaRow(fields, rest) => Schema(fields, rest)
            case tvar: Var => Schema(Nil, Some(tvar))
            case _ => throw IllKindedException
          }
          case _ => throw IllKindedException
        }
      case TypeConstructor.ScopedArray => mkApply(ScopedArray, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Channel => mkApply(Channel, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Lazy => mkApply(Lazy, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Tag(sym, tag) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => TagConstructor(tag.name)
          case tpe :: Nil => Tag(tag.name, destructTuple(tpe), Hole)
          case tpe :: ret :: Nil => Tag(tag.name, destructTuple(tpe), ret)
          case _ => throw IllKindedException
        }
      case TypeConstructor.KindedEnum(sym, kind) => mkApply(Name(sym.name), t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.UnkindedEnum(sym) => throw InternalCompilerException("Unexpected unkinded type.")
      case TypeConstructor.Native(clazz) => Name(clazz.getSimpleName)
      case TypeConstructor.ScopedRef => mkApply(ScopedRef, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Tuple(l) => Tuple(l, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Relation =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => RelationConstructor
          case tpe :: Nil => Relation(destructTuple(tpe))
          case _ => throw IllKindedException
        }
      case TypeConstructor.Lattice =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => LatticeConstructor
          case tpe :: Nil => Lattice(destructTuple(tpe))
          case _ => throw IllKindedException
        }
      case TypeConstructor.True => True
      case TypeConstructor.False => False
      case TypeConstructor.Not =>
        t.typeArguments.map(fromWellKindedType) match {
          case Nil => Not(Hole)
          case arg :: Nil => Not(arg)
          case _ :: _ :: _ => throw IllKindedException
        }

      case TypeConstructor.And =>
        // collapse into a chain of ands
        t.typeArguments.map(fromWellKindedType).map(splitAnds) match {
          // MATT case docs
          case Nil => And(Hole :: Hole :: Nil)
          case args :: Nil => And(args :+ Hole)
          case args1 :: args2 :: Nil => And(args1 ++ args2)
          case _ :: _ :: _ :: _ => throw IllKindedException
        }

      case TypeConstructor.Or =>
        // collapse into a chain of ors
        t.typeArguments.map(fromWellKindedType).map(splitOrs) match {
          // MATT case docs
          case Nil => Or(Hole :: Hole :: Nil)
          case args :: Nil => Or(args :+ Hole)
          case args1 :: args2 :: Nil => Or(args1 ++ args2)
          case _ :: _ :: _ :: _ => throw IllKindedException
        }

      case TypeConstructor.Region => mkApply(Region, t.typeArguments.map(fromWellKindedType))
      case _: TypeConstructor.UnappliedAlias => throw InternalCompilerException("Unexpected unapplied alias.")
    }
  }

  // MATT docs
  private def mkApply(base: SimpleType, args: List[SimpleType]): SimpleType = args match {
    case Nil => base
    case _ :: _ => Apply(base, args)
  }

  // MATT docs
  private def destructTuple(tpe: SimpleType): List[SimpleType] = tpe match {
    case Tuple(_, fields) => fields
    case Unit => Nil
    case t => t :: Nil
  }

  // MATT docs
  private def fromRecordRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // MATT case docs
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(name), _), tpe, _), rest, _) =>
        val fieldType = FieldType(name.name, fromWellKindedType(tpe))
        fromRecordRow(rest) match {
          case SimpleType.RecordRowEmpty => SimpleType.RecordRow(fieldType :: Nil, None)
          case SimpleType.RecordRow(fields, rest) => SimpleType.RecordRow(fieldType :: fields, rest) // MATT shadow
          case tvar: SimpleType.Var => SimpleType.RecordRow(fieldType :: Nil, Some(tvar))
          case _ => ??? // MATT ICE
        }
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) => SimpleType.RecordRowEmpty
      case Type.KindedVar(id, kind, loc, rigidity, text) => SimpleType.Var(id) // MATT ignoring text
      case _ => ??? // MATT ICE
    }

    // sort the fields after converting
    visit(row0) match {
      case RecordRowEmpty => RecordRowEmpty
      case RecordRow(fields, rest) => RecordRow(fields.sortBy(_.name), rest)
      case Var(id) => Var(id)
      case _ => ??? // MATT ICE (do I need var here?)
    }
  }

  // MATT docs
  private def fromSchemaRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // MATT case docs
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(name), _), tpe, _), rest, _) =>
        val fieldType = FieldType(name.name, fromWellKindedType(tpe))
        fromSchemaRow(rest) match {
          case SimpleType.SchemaRowEmpty => SimpleType.SchemaRow(fieldType :: Nil, None)
          case SimpleType.SchemaRow(fields, rest) => SimpleType.SchemaRow(fieldType :: fields, rest) // MATT shadow
          case tvar: SimpleType.Var => SimpleType.SchemaRow(fieldType :: Nil, Some(tvar))
          case _ => ??? // MATT ICE
        }
      case Type.Cst(TypeConstructor.SchemaRowEmpty, _) => SimpleType.SchemaRowEmpty
      case Type.KindedVar(id, kind, loc, rigidity, text) => SimpleType.Var(id) // MATT ignoring text
      case _ => ??? // MATT ICE
    }

    // sort the fields after converting
    visit(row0) match {
      case SchemaRowEmpty => SchemaRowEmpty
      case SchemaRow(fields, rest) => SchemaRow(fields.sortBy(_.name), rest)
      case Var(id) => Var(id)
      case _ => ??? // MATT ICE (do I need var here?)
    }
  }

  private def splitAnds(tpe: SimpleType): List[SimpleType] = tpe match {
    case And(tpes) => tpes
    case t => List(t)
  }

  private def splitOrs(tpe: SimpleType): List[SimpleType] = tpe match {
    case Or(tpes) => tpes
    case t => List(t)
  }
}
