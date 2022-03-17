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

import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Type, TypeConstructor}
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

  case class RecordConstructor(arg: SimpleType) extends SimpleType

  case class Record(fields: List[FieldType]) extends SimpleType

  case class RecordExtend(fields: List[FieldType], rest: SimpleType) extends SimpleType

  case class RecordRow(fields: List[FieldType]) extends SimpleType

  case class RecordRowExtend(fields: List[FieldType], rest: SimpleType) extends SimpleType

  // Schemas

  case class SchemaConstructor(arg: SimpleType) extends SimpleType

  case class Schema(fields: List[FieldType]) extends SimpleType

  case class SchemaExtend(fields: List[FieldType], rest: SimpleType) extends SimpleType

  case class SchemaRow(fields: List[FieldType]) extends SimpleType

  case class SchemaRowExtend(fields: List[FieldType], rest: SimpleType) extends SimpleType

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

  case class Var(id: Int, kind: Kind, rigidity: Rigidity, text: Option[String]) extends SimpleType

  case class Tuple(fields: List[SimpleType]) extends SimpleType


  /**
    * Creates a simple type from the well-kinded type `t`.
   */
  def fromWellKindedType(t: Type): SimpleType = t.baseType match {
    case Type.KindedVar(id, kind, loc, rigidity, text) => Var(id, kind, rigidity, text)
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
      case TypeConstructor.RecordRowEmpty => RecordRow(Nil)
      case TypeConstructor.RecordRowExtend(field) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          // MATT case docs
          case Nil => RecordRowExtend(FieldType(field.name, Hole) :: Nil, Hole)
          case tpe :: Nil => RecordRowExtend(FieldType(field.name, tpe) :: Nil, Hole)
          case _ :: _ :: Nil => fromRecordRow(t)
          case _ => throw IllKindedException
        }
      case TypeConstructor.Record =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => RecordConstructor(Hole)
          case tpe :: Nil => tpe match {
            case RecordRow(fields) => Record(fields)
            case RecordRowExtend(fields, rest) => RecordExtend(fields, rest)
            case tvar: Var => RecordConstructor(tvar)
            case _ => throw IllKindedException
          }
          case _ => throw IllKindedException
        }
      case TypeConstructor.SchemaRowEmpty => SchemaRow(Nil)
      case TypeConstructor.SchemaRowExtend(pred) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => SchemaRowExtend(FieldType(pred.name, Hole) :: Nil, Hole)
          case tpe :: Nil => SchemaRowExtend(FieldType(pred.name, tpe) :: Nil, Hole)
          case _ :: _ :: Nil => fromSchemaRow(t)
          case _ => throw IllKindedException
        }
      case TypeConstructor.Schema =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => SchemaConstructor(Hole)
          case tpe :: Nil => tpe match {
            case SchemaRow(fields) => Schema(fields)
            case SchemaRowExtend(fields, rest) => SchemaExtend(fields, rest)
            case tvar: Var => SchemaConstructor(tvar)
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
      case TypeConstructor.Tuple(l) =>
        val tpes = t.typeArguments.map(fromWellKindedType).padTo(l, Hole)
        Tuple(tpes)
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
    case Tuple(fields) => fields
    case Unit => Nil
    case t => t :: Nil
  }

  // MATT docs
  private def fromRecordRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // MATT case docs
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(name), _), tpe, _), rest, _) =>
        val fieldType = FieldType(name.name, fromWellKindedType(tpe))
        visit(rest) match {
          // MATT case docs
          case SimpleType.RecordRow(fields) => SimpleType.RecordRow(fieldType :: fields)
          case SimpleType.RecordRowExtend(fields, rest) => SimpleType.RecordRowExtend(fieldType :: fields, rest) // MATT shadow
          case tvar: SimpleType.Var => SimpleType.RecordRowExtend(fieldType :: Nil, tvar)
          case _ => throw IllKindedException
        }
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) => SimpleType.RecordRow(Nil)
      case Type.KindedVar(id, kind, loc, rigidity, text) => SimpleType.Var(id, kind, rigidity, text)
      case _ => throw IllKindedException
    }

    // sort the fields after converting
    visit(row0) match {
      case RecordRowExtend(fields, rest) => RecordRowExtend(fields.sortBy(_.name), rest)
      case RecordRow(fields) => RecordRow(fields.sortBy(_.name))
      case v: Var => v
      case _ => throw IllKindedException
    }
  }

  // MATT docs
  private def fromSchemaRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // MATT case docs
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(name), _), tpe, _), rest, _) =>
        val fieldType = FieldType(name.name, fromWellKindedType(tpe))
        visit(rest) match {
          case SimpleType.SchemaRow(fields) => SimpleType.SchemaRow(fieldType :: fields)
          case SimpleType.SchemaRowExtend(fields, rest) => SimpleType.SchemaRowExtend(fieldType :: fields, rest) // MATT shadow
          case tvar: SimpleType.Var => SimpleType.SchemaRowExtend(fieldType :: Nil, tvar)
          case _ => throw IllKindedException
        }
      case Type.Cst(TypeConstructor.SchemaRowEmpty, _) => SimpleType.SchemaRow(Nil)
      case Type.KindedVar(id, kind, loc, rigidity, text) => SimpleType.Var(id, kind, rigidity, text)
      case _ => throw IllKindedException
    }

    // sort the fields after converting
    visit(row0) match {
      case SchemaRow(fields) => SchemaRow(fields.sortBy(_.name))
      case SchemaRowExtend(fields, rest) => SchemaRowExtend(fields.sortBy(_.name), rest)
      case v: Var => v
      case _ => throw IllKindedException
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
