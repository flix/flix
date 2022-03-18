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

  ///////
  // Hole
  ///////

  /**
    * An unfilled parameter in a partially-applied type-level function.
    * For example, `Not` (applied to nothing) is `Not(Hole)`.
    */
  case object Hole extends SimpleType

  /////////////
  // Primitives
  /////////////

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

  //////////
  // Records
  //////////

  /**
    * A record constructor. `arg` should be a variable or a Hole.
    */
  case class RecordConstructor(arg: SimpleType) extends SimpleType

  /**
    * An unextended record.
    */
  case class Record(fields: List[RecordFieldType]) extends SimpleType

  /**
    * An extended record. `arg` should be a variable or a Hole.
    */
  case class RecordExtend(fields: List[RecordFieldType], rest: SimpleType) extends SimpleType

  /**
    * An unextended record row.
    */
  case class RecordRow(fields: List[RecordFieldType]) extends SimpleType

  /**
    * An extended record row. `arg` should be a variable or a Hole.
    */
  case class RecordRowExtend(fields: List[RecordFieldType], rest: SimpleType) extends SimpleType

  //////////
  // Schemas
  //////////

  /**
    * A schema constructor. `arg` should be a variable or a Hole.
    */
  case class SchemaConstructor(arg: SimpleType) extends SimpleType

  /**
    * An unextended schema.
    */
  case class Schema(fields: List[PredicateFieldType]) extends SimpleType

  /**
    * An extended schema. `arg` should be a variable or a Hole.
    */
  case class SchemaExtend(fields: List[PredicateFieldType], rest: SimpleType) extends SimpleType

  /**
    * An unextended schema row.
    */
  case class SchemaRow(fields: List[PredicateFieldType]) extends SimpleType

  /**
    * An extended schema row. `arg` should be a variable or a Hole.
    */
  case class SchemaRowExtend(fields: List[PredicateFieldType], rest: SimpleType) extends SimpleType

  ////////////////////
  // Boolean Operators
  ////////////////////

  /**
    * Boolean negation.
    */
  case class Not(tpe: SimpleType) extends SimpleType

  /**
    * A chain of types connected by `and`.
    */
  case class And(tpes: List[SimpleType]) extends SimpleType

  /**
    * A chain of types connected by `or`.
    */
  case class Or(tpes: List[SimpleType]) extends SimpleType

  /////////////
  // Predicates
  /////////////

  case object RelationConstructor extends SimpleType

  /**
    * A relation over a list of types.
    */
  case class Relation(tpes: List[SimpleType]) extends SimpleType

  case object LatticeConstructor extends SimpleType

  /**
    * A lattice over a list of types.
    */
  case class Lattice(tpes: List[SimpleType], lat: SimpleType) extends SimpleType

  ////////////
  // Functions
  ////////////

  /**
    * A pure function.
    */
  case class PureArrow(arg: SimpleType, ret: SimpleType) extends SimpleType

  /**
    * A function with an effect.
    */
  case class PolyArrow(arg: SimpleType, eff: SimpleType, ret: SimpleType) extends SimpleType

  ///////
  // Tags
  ///////

  case class TagConstructor(name: String) extends SimpleType

  case class Tag(name: String, args: List[SimpleType], ret: SimpleType) extends SimpleType

  //////////////////////
  // Miscellaneous Types
  //////////////////////

  /**
    * A simple named type (e.g., enum or type alias).
    */
  case class Name(name: String) extends SimpleType

  /**
    * A type applied to one or more types.
    */
  case class Apply(tpe: SimpleType, tpes: List[SimpleType]) extends SimpleType

  /**
    * A type variable.
    */
  case class Var(id: Int, kind: Kind, rigidity: Rigidity, text: Option[String]) extends SimpleType

  /**
    * A tuple.
    */
  case class Tuple(fields: List[SimpleType]) extends SimpleType

  /////////
  // Fields
  /////////

  /**
    * A record field name and its type.
    */
  case class RecordFieldType(name: String, tpe: SimpleType)

  /**
    * A common supertype for schema predicates.
    */
  sealed trait PredicateFieldType {
    val name: String
  }

  /**
    * A relation field name and its types.
    */
  case class RelationFieldType(name: String, tpes: List[SimpleType]) extends PredicateFieldType

  /**
    * A lattice field name, its types, and its lattice.
    */
  case class LatticeFieldType(name: String, tpes: List[SimpleType], lat: SimpleType) extends PredicateFieldType

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
          // Case 1: No args. Fill everything with a hole.
          case Nil =>
            val lastArrow: SimpleType = PolyArrow(Hole, Hole, Hole)
            // NB: safe to subtract 2 since arity is always at least 2
            List.fill(arity - 2)(Hole).foldRight(lastArrow)(PureArrow)
          // Case 2: Pure function.
          case True :: tpes =>
            // NB: safe to reduce because arity is always at least 2
            tpes.padTo(arity, Hole).reduceRight(PureArrow)
          // Case 3: Impure function.
          case eff :: tpes =>
            // NB: safe to take last 2 because arity is always at least 2
            val allTpes = tpes.padTo(arity, Hole)
            val List(lastArg, ret) = allTpes.takeRight(2)
            val lastArrow: SimpleType = PolyArrow(lastArg, eff, ret)
            allTpes.dropRight(2).foldRight(lastArrow)(PureArrow)
        }

      case TypeConstructor.RecordRowEmpty => RecordRow(Nil)

      case TypeConstructor.RecordRowExtend(field) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          // Case 1: No args. ( name: ? | ? )
          case Nil => RecordRowExtend(RecordFieldType(field.name, Hole) :: Nil, Hole)
          // Case 2: One arg. ( name: tpe | ? )
          case tpe :: Nil => RecordRowExtend(RecordFieldType(field.name, tpe) :: Nil, Hole)
          // Case 3: Fully applied. Dispatch to proper record handler.
          case _ :: _ :: Nil => fromRecordRow(t)
          // Case 4: Too many args. Error.
          case _ => throw IllKindedException
        }

      case TypeConstructor.Record =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          // Case 1: No args. { ? }
          case Nil => RecordConstructor(Hole)
          // Case 2: One row argument. Extract its values.
          case tpe :: Nil => tpe match {
            case RecordRow(fields) => Record(fields)
            case RecordRowExtend(fields, rest) => RecordExtend(fields, rest)
            case tvar: Var => RecordConstructor(tvar)
            case _ => throw IllKindedException
          }
          // Case 3: Too many args. Error.
          case _ => throw IllKindedException
        }

      case TypeConstructor.SchemaRowEmpty => SchemaRow(Nil)

      case TypeConstructor.SchemaRowExtend(pred) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          // Case 1: No args. #( Name(?) | ? )
          case Nil => SchemaRowExtend(RelationFieldType(pred.name, Hole :: Nil) :: Nil, Hole)
          // Case 2: One relation arg. #( Name(tpe1, tpe2) | ? )
          case Relation(tpes) :: Nil => SchemaRowExtend(RelationFieldType(pred.name, tpes) :: Nil, Hole)
          // Case 3: One lattice arg. #( Name(tpe1; tpe2) | ? )
          case Lattice(tpes, lat) :: Nil => SchemaRowExtend(LatticeFieldType(pred.name, tpes, lat) :: Nil, Hole)
          // Case 4: Fully applied. Dispatch to proper schema handler.
          case _ :: _ :: Nil => fromSchemaRow(t)
          // Case 5: Too many or invalid args. Error.
          case _ => throw IllKindedException
        }

      case TypeConstructor.Schema =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          // Case 1: No args. { ? }
          case Nil => SchemaConstructor(Hole)
          // Case 2: One row argument. Extract its values.
          case tpe :: Nil => tpe match {
            case SchemaRow(fields) => Schema(fields)
            case SchemaRowExtend(fields, rest) => SchemaExtend(fields, rest)
            case tvar: Var => SchemaConstructor(tvar)
            case _ => throw IllKindedException
          }
          // Case 3: Too many args. Error.
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
          case tpe :: Nil =>
            val tpesAndLat = destructTuple(tpe)
            // NB: safe to take init/last since every lattice has a lattice field
            val tpes = tpesAndLat.init
            val lat = tpesAndLat.last
            Lattice(tpes, lat)
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
          // Case 1: No args. ? and ?
          case Nil => And(Hole :: Hole :: Nil)
          // Case 2: One arg. Take the left and put a hole at the end: tpe1 and tpe2 and ?
          case args :: Nil => And(args :+ Hole)
          // Case 3: Multiple args. Concatenate them: tpe1 and tpe2 and tpe3 and tpe4
          case args1 :: args2 :: Nil => And(args1 ++ args2)
          // Case 4: Too many args. Error.
          case _ :: _ :: _ :: _ => throw IllKindedException
        }

      case TypeConstructor.Or =>
        // collapse into a chain of ors
        t.typeArguments.map(fromWellKindedType).map(splitOrs) match {
          // Case 1: No args. ? or ?
          case Nil => Or(Hole :: Hole :: Nil)
          // Case 2: One arg. Take the left and put a hole at the end: tpe1 or tpe2 or ?
          case args :: Nil => Or(args :+ Hole)
          // Case 3: Multiple args. Concatenate them: tpe1 or tpe2 or tpe3 or tpe4
          case args1 :: args2 :: Nil => Or(args1 ++ args2)
          // Case 4: Too many args. Error.
          case _ :: _ :: _ :: _ => throw IllKindedException
        }

      case TypeConstructor.Region => mkApply(Region, t.typeArguments.map(fromWellKindedType))
      case _: TypeConstructor.UnappliedAlias => throw InternalCompilerException("Unexpected unapplied alias.")
    }
  }

  /**
    * Builds an Apply type.
    */
  private def mkApply(base: SimpleType, args: List[SimpleType]): SimpleType = args match {
    case Nil => base
    case _ :: _ => Apply(base, args)
  }

  /**
    * Extracts the types from a tuple, treating non-tuples as singletons.
    */
  private def destructTuple(tpe: SimpleType): List[SimpleType] = tpe match {
    case Tuple(fields) => fields
    case Unit => Nil
    case t => t :: Nil
  }

  /**
    * Transforms the given type, assuming it is a record row.
    */
  private def fromRecordRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // Case 1: A fully applied record row.
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(name), _), tpe, _), rest, _) =>
        val fieldType = RecordFieldType(name.name, fromWellKindedType(tpe))
        visit(rest) match {
          // Case 1.1: Unextended row. Put the fields together.
          case SimpleType.RecordRow(fields) => SimpleType.RecordRow(fieldType :: fields)
          // Case 1.2: Extended row. Put the fields together.
          case SimpleType.RecordRowExtend(fields, rest) => SimpleType.RecordRowExtend(fieldType :: fields, rest) // MATT shadow
          // Case 1.3: Var. Put it in the "rest" position.
          case tvar: SimpleType.Var => SimpleType.RecordRowExtend(fieldType :: Nil, tvar)
          // Case 1.4: Not a row. Error.
          case _ => throw IllKindedException
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) => SimpleType.RecordRow(Nil)
      // Case 3: Variable.
      case Type.KindedVar(id, kind, _, rigidity, text) => SimpleType.Var(id, kind, rigidity, text)
      // Case 4: Not a row. Error.
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

  /**
    * Transforms the given type, assuming it is a schema row.
    */
  private def fromSchemaRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // Case 1: A fully applied record row.
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(name), _), tpe, _), rest, _) =>
        // create the right field/type for the field
        val fieldType = fromWellKindedType(tpe) match {
          case Relation(tpes) => RelationFieldType(name.name, tpes)
          case Lattice(tpes, lat) => LatticeFieldType(name.name, tpes, lat)
          case _ => throw IllKindedException
        }
        visit(rest) match {
          // Case 1.1: Unextended row. Put the fields together.
          case SimpleType.SchemaRow(fields) => SimpleType.SchemaRow(fieldType :: fields)
          // Case 1.2: Extended row. Put the fields together.
          case SimpleType.SchemaRowExtend(fields, rest) => SimpleType.SchemaRowExtend(fieldType :: fields, rest) // MATT shadow
          // Case 1.3: Var. Put it in the "rest" position.
          case tvar: SimpleType.Var => SimpleType.SchemaRowExtend(fieldType :: Nil, tvar)
          // Case 1.4: Not a row. Error.
          case _ => throw IllKindedException
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.SchemaRowEmpty, _) => SimpleType.SchemaRow(Nil)
      // Case 3: Variable.
      case Type.KindedVar(id, kind, _, rigidity, text) => SimpleType.Var(id, kind, rigidity, text)
      // Case 4: Not a row. Error.
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

  /**
    * Splits `t1 and t2` into `t1 :: t2 :: Nil`,
    * and leaves non-and types as singletons.
    */
  private def splitAnds(tpe: SimpleType): List[SimpleType] = tpe match {
    case And(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 or t2` into `t1 :: t2 :: Nil`,
    * and leaves non-or types as singletons.
    */
  private def splitOrs(tpe: SimpleType): List[SimpleType] = tpe match {
    case Or(tpes) => tpes
    case t => List(t)
  }
}
