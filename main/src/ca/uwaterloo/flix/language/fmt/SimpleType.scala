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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * A well-kinded type in an easily-printable format.
  */
sealed trait SimpleType

object SimpleType {

  private class OverAppliedType extends InternalCompilerException("Unexpected over-applied type.")

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

  case object Array extends SimpleType

  case object Ref extends SimpleType

  case object Channel extends SimpleType

  case object Lazy extends SimpleType

  case object True extends SimpleType

  case object False extends SimpleType

  case object Region extends SimpleType

  case object Empty extends SimpleType

  case object All extends SimpleType

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

  ////////////////
  // Set Operators
  ////////////////

  /**
    * Set complement.
    */
  case class Complement(tpe: SimpleType) extends SimpleType

  /**
    * A chain of types connected by `+`.
    */
  case class Union(tpes: List[SimpleType]) extends SimpleType

  /**
    * A chain of types connected by `&`.
    */
  case class Intersection(tpes: List[SimpleType]) extends SimpleType

  /**
    * Difference of two types.
    */
  case class Difference(tpe1: SimpleType, tpe2: SimpleType) extends SimpleType

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
    * A function with a purity.
    */
  case class PolyPurArrow(arg: SimpleType, pur: SimpleType, ret: SimpleType) extends SimpleType

  /**
    * A function with an effect.
    */
  case class PolyEffArrow(arg: SimpleType, eff: SimpleType, ret: SimpleType) extends SimpleType

  /**
    * A function with effect and purity.
    */
  case class PolyPurAndEffArrow(arg: SimpleType, pur: SimpleType, eff: SimpleType, ret: SimpleType) extends SimpleType

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
  case class Var(id: Int, kind: Kind, isRegion: Boolean, text: Ast.VarText) extends SimpleType

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
    * A predicate field type that's not actually a predicate.
    */
  case class NonPredFieldType(name: String, tpe: SimpleType) extends PredicateFieldType

  /**
    * Creates a simple type from the well-kinded type `t`.
   */
  def fromWellKindedType(t: Type): SimpleType = t.baseType match {
    case Type.KindedVar(sym, _) =>
      mkApply(Var(sym.id, sym.kind, sym.isRegion, sym.text), t.typeArguments.map(fromWellKindedType))
    case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.UnkindedArrow => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.ReadWrite => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.Ascribe => throw InternalCompilerException("Unexpected kind ascription.")
    case Type.Alias(cst, args, _, _) =>
      mkApply(Name(cst.sym.name), (args ++ t.typeArguments).map(fromWellKindedType))
    case Type.Cst(tc, _) => tc match {
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
            val lastArrow: SimpleType = PolyPurAndEffArrow(Hole, Hole, Hole, Hole)
            // NB: safe to subtract 2 since arity is always at least 2
            List.fill(arity - 2)(Hole).foldRight(lastArrow)(PureArrow)

          // Case 2: Only applied to purity but not effect
          case pur :: Nil =>
            val lastArrow: SimpleType = PolyPurAndEffArrow(Hole, pur, Hole, Hole)
            // NB: safe to subtract 2 since arity is always at least 2
            List.fill(arity - 2)(Hole).foldRight(lastArrow)(PureArrow)

          // Case 3: Pure function.
          case True :: Empty :: tpes =>
            // NB: safe to reduce because arity is always at least 2
            tpes.padTo(arity, Hole).reduceRight(PureArrow)

          // Case 4: Impure in effect only.
          case True :: eff :: tpes =>
            // NB: safe to take last 2 because arity is always at least 2
            val allTpes = tpes.padTo(arity, Hole)
            val List(lastArg, ret) = allTpes.takeRight(2)
            val lastArrow: SimpleType = PolyEffArrow(lastArg, eff, ret)
            allTpes.dropRight(2).foldRight(lastArrow)(PureArrow)

          // Case 5: Impure in purity only.
          case pur :: Empty :: tpes =>
            // NB: safe to take last 2 because arity is always at least 2
            val allTpes = tpes.padTo(arity, Hole)
            val List(lastArg, ret) = allTpes.takeRight(2)
            val lastArrow: SimpleType = PolyPurArrow(lastArg, pur, ret)
            allTpes.dropRight(2).foldRight(lastArrow)(PureArrow)

          // Case 6: Impure function.
          case pur :: eff :: tpes =>
            // NB: safe to take last 2 because arity is always at least 2
            val allTpes = tpes.padTo(arity, Hole)
            val List(lastArg, ret) = allTpes.takeRight(2)
            val lastArrow: SimpleType = PolyPurAndEffArrow(lastArg, pur, eff, ret)
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
          case _ :: _ :: _ :: _ => throw new OverAppliedType
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
            case nonRecord => RecordConstructor(nonRecord)
          }
          // Case 3: Too many args. Error.
          case _ :: _ :: _ => throw new OverAppliedType
        }

      case TypeConstructor.SchemaRowEmpty => SchemaRow(Nil)

      case TypeConstructor.SchemaRowExtend(pred) =>
        // erase aliases over the Schema/Relation
        val args = mapHead(t.typeArguments, Type.eraseTopAliases).map(fromWellKindedType)
        args match {
          // Case 1: No args. #( Name(?) | ? )
          case Nil => SchemaRowExtend(RelationFieldType(pred.name, Hole :: Nil) :: Nil, Hole)
          // Case 2: One relation arg. #( Name(tpe1, tpe2) | ? )
          case Relation(tpes) :: Nil => SchemaRowExtend(RelationFieldType(pred.name, tpes) :: Nil, Hole)
          // Case 3: One lattice arg. #( Name(tpe1; tpe2) | ? )
          case Lattice(tpes, lat) :: Nil => SchemaRowExtend(LatticeFieldType(pred.name, tpes, lat) :: Nil, Hole)
          // Case 4: Some non-predicate type.
          case _ :: Nil => SchemaRowExtend(NonPredFieldType(pred.name, args.head) :: Nil, Hole)
          // Case 5: Fully applied. Dispatch to proper schema handler.
          case _ :: _ :: Nil => fromSchemaRow(t)
          // Case 6: Too many args. Error.
          case _ :: _ :: _ :: _ => throw new OverAppliedType
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
            case nonSchema => SchemaConstructor(nonSchema)
          }
          // Case 3: Too many args. Error.
          case _ :: _ :: _ => throw new OverAppliedType
        }
      case TypeConstructor.Array => mkApply(Array, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Channel => mkApply(Channel, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Lazy => mkApply(Lazy, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Tag(sym, tag) =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          // Case 1: Bare tag.
          case Nil => PureArrow(Hole, Hole)
          // Case 2: Tag with arguments.
          case tpe :: Nil => PureArrow(tpe, Hole)
          // Case 3: Fully applied tag.
          case tpe :: ret :: Nil => PureArrow(tpe, ret)
          // Case 4: Too many arguments. Error.
          case _ :: _ :: _ :: _ => throw new OverAppliedType
        }
      case TypeConstructor.KindedEnum(sym, kind) => mkApply(Name(sym.name), t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.UnkindedEnum(sym) => throw InternalCompilerException("Unexpected unkinded type.")
      case TypeConstructor.Native(clazz) => Name(clazz.getSimpleName)
      case TypeConstructor.Ref => mkApply(Ref, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Tuple(l) =>
        val tpes = t.typeArguments.map(fromWellKindedType).padTo(l, Hole)
        Tuple(tpes)
      case TypeConstructor.Relation =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => RelationConstructor
          case tpe :: Nil => Relation(destructTuple(tpe))
          case _ :: _ :: _ => throw new OverAppliedType
        }
      case TypeConstructor.Lattice =>
        val args = t.typeArguments.map(fromWellKindedType)
        args match {
          case Nil => LatticeConstructor
          case tpe :: Nil =>
            val tpesAndLat = destructTuple(tpe)
            // NB: safe to take init/last since every lattice has a lattice field
            // MATT not safe in case of alias!
            val tpes = tpesAndLat.init
            val lat = tpesAndLat.last
            Lattice(tpes, lat)
          case _ :: _ :: _ => throw new OverAppliedType
        }
      case TypeConstructor.True => True
      case TypeConstructor.False => False
      case TypeConstructor.Not =>
        t.typeArguments.map(fromWellKindedType) match {
          case Nil => Not(Hole)
          case arg :: Nil => Not(arg)
          case _ :: _ :: _ => throw new OverAppliedType
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
          case _ :: _ :: _ :: _ => throw new OverAppliedType
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
          case _ :: _ :: _ :: _ => throw new OverAppliedType
        }

      case TypeConstructor.Complement =>
        t.typeArguments.map(fromWellKindedType) match {
          case Nil => Complement(Hole)
          case arg :: Nil => Complement(arg)
          case _ :: _ :: _ => throw new OverAppliedType
        }

      case TypeConstructor.Union =>
        // collapse into a chain of unions
        t.typeArguments.map(fromWellKindedType).map(splitUnions) match {
          // Case 1: No args. ? + ?
          case Nil => Union(Hole :: Hole :: Nil)
          // Case 2: One arg. Take the left and put a hole at the end: tpe1 + tpe2 + ?
          case args :: Nil => Union(args :+ Hole)
          // Case 3: Multiple args. Concatenate them: tpe1 + tpe2 + tpe3 + tpe4
          case args1 :: args2 :: Nil => Union(args1 ++ args2)
          // Case 4: Too many args. Error.
          case _ :: _ :: _ :: _ => throw new OverAppliedType
        }

      case TypeConstructor.Intersection =>
        // collapse into a chain of intersections
        t.typeArguments.map(fromWellKindedType).map(splitIntersections) match {
          // Case 1: No args. ? & ?
          case Nil => Intersection(Hole :: Hole :: Nil)
          // Case 2: One arg. Take the left and put a hole at the end: tpe1 & tpe2 & ?
          case args :: Nil => Intersection(args :+ Hole)
          // Case 3: Complement on the right: sugar it into a difference.
          case args :: List(Complement(tpe)) :: Nil => Difference(joinIntersection(args), tpe)
          // Case 4: Complement on the left: sugar it into a difference.
          case List(Complement(tpe)) :: args :: Nil => Difference(joinIntersection(args), tpe)
          // Case 5: Multiple args. Concatenate them: tpe1 & tpe2 & tpe3 & tpe4
          case args1 :: args2 :: Nil => Intersection(args1 ++ args2)
          // Case 6: Too many args. Error.
          case _ :: _ :: _ :: _ => throw new OverAppliedType
        }

      case TypeConstructor.Effect(sym) => mkApply(SimpleType.Name(sym.name), t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Region => mkApply(Region, t.typeArguments.map(fromWellKindedType))
      case TypeConstructor.Empty => SimpleType.Empty
      case TypeConstructor.All => SimpleType.All
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
          case SimpleType.RecordRowExtend(fields, restOfRest) => SimpleType.RecordRowExtend(fieldType :: fields, restOfRest)
          // Case 1.3: Non-row. Put it in the "rest" position.
          case nonRecord => SimpleType.RecordRowExtend(fieldType :: Nil, nonRecord)
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) => SimpleType.RecordRow(Nil)
      // Case 3: Non-row.
      case nonRecord => fromWellKindedType(nonRecord)
    }

    // sort the fields after converting
    visit(row0) match {
      case RecordRowExtend(fields, rest) => RecordRowExtend(fields.sortBy(_.name), rest)
      case RecordRow(fields) => RecordRow(fields.sortBy(_.name))
      case nonRecord => nonRecord
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
        val fieldType = fromWellKindedType(Type.eraseTopAliases(tpe)) match {
          case Relation(tpes) => RelationFieldType(name.name, tpes)
          case Lattice(tpes, lat) => LatticeFieldType(name.name, tpes, lat)
          // If it's not a relation or lattice, keep any aliases.
          case _ => NonPredFieldType(name.name, fromWellKindedType(tpe))
        }
        visit(rest) match {
          // Case 1.1: Unextended row. Put the fields together.
          case SimpleType.SchemaRow(fields) => SimpleType.SchemaRow(fieldType :: fields)
          // Case 1.2: Extended row. Put the fields together.
          case SimpleType.SchemaRowExtend(fields, restOfRest) => SimpleType.SchemaRowExtend(fieldType :: fields, restOfRest)
          // Case 1.3: Non-row. Put it in the "rest" position.
          case nonSchema => SimpleType.SchemaRowExtend(fieldType :: Nil, nonSchema)
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.SchemaRowEmpty, _) => SimpleType.SchemaRow(Nil)
      // Case 3: Non-row.
      case nonSchema => fromWellKindedType(nonSchema)
    }

    // sort the fields after converting
    visit(row0) match {
      case SchemaRow(fields) => SchemaRow(fields.sortBy(_.name))
      case SchemaRowExtend(fields, rest) => SchemaRowExtend(fields.sortBy(_.name), rest)
      case nonSchema => nonSchema
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

  /**
    * Splits `t1 + t2` into `t1 :: t2 :: Nil`,
    * and leaves non-union types as singletons.
    */
  private def splitUnions(tpe: SimpleType): List[SimpleType] = tpe match {
    case Union(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 & t2` into `t1 :: t2 :: Nil`,
    * and leaves non-intersection types as singletons.
    */
  private def splitIntersections(tpe: SimpleType): List[SimpleType] = tpe match {
    case Intersection(tpes) => tpes
    case t => List(t)
  }

  /**
    * Joins `t1 :: t2 :: Nil` into `t1 & t2`
    * and leaves singletons as non-intersection types.
    */
  private def joinIntersection(tpes: List[SimpleType]): SimpleType = tpes match {
    case Nil => throw InternalCompilerException("unexpected empty type list")
    case t :: Nil => t
    case ts => Intersection(ts)
  }

  /**
    * Map over the first element in the list, if it exists.
    */
  private def mapHead[A](l: List[A], f: A => A): List[A] = l match {
    case Nil => Nil
    case hd :: tl => f(hd) :: tl
  }
}
