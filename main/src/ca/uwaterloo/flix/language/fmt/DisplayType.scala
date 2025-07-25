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

import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.shared.VarText
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.reflect.{Constructor, Field, Method}

/**
  * A well-kinded type in an easily-printable format.
  */
sealed trait DisplayType

object DisplayType {

  private class OverAppliedType(loc: SourceLocation) extends InternalCompilerException("Unexpected over-applied type.", loc)

  ///////
  // Hole
  ///////

  /**
    * An unfilled parameter in a partially-applied type-level function.
    * For example, `Not` (applied to nothing) is `Not(Hole)`.
    */
  case object Hole extends DisplayType

  /////////////
  // Primitives
  /////////////

  case object Void extends DisplayType

  case object AnyType extends DisplayType

  case object Unit extends DisplayType

  case object Null extends DisplayType

  case object Bool extends DisplayType

  case object Char extends DisplayType

  case object Float32 extends DisplayType

  case object Float64 extends DisplayType

  case object BigDecimal extends DisplayType

  case object Int8 extends DisplayType

  case object Int16 extends DisplayType

  case object Int32 extends DisplayType

  case object Int64 extends DisplayType

  case object BigInt extends DisplayType

  case object Str extends DisplayType

  case object Regex extends DisplayType

  case object Array extends DisplayType

  case object ArrayWithoutRegion extends DisplayType

  case object Vector extends DisplayType

  case object Sender extends DisplayType

  case object Receiver extends DisplayType

  case object Lazy extends DisplayType

  case object Pure extends DisplayType

  case object Univ extends DisplayType

  case object False extends DisplayType

  case object True extends DisplayType

  case object RegionToStar extends DisplayType

  case object RegionWithoutRegion extends DisplayType

  //////////
  // Records
  //////////

  /**
    * A record constructor. `arg` should be a variable or a Hole.
    */
  case class RecordConstructor(arg: DisplayType) extends DisplayType

  /**
    * An unextended record.
    */
  case class Record(labels: List[RecordLabelType]) extends DisplayType

  /**
    * An extended record. `arg` should be a variable or a Hole.
    */
  case class RecordExtend(labels: List[RecordLabelType], rest: DisplayType) extends DisplayType

  /**
    * An unextended record row.
    */
  case class RecordRow(labels: List[RecordLabelType]) extends DisplayType

  /**
    * An extended record row. `arg` should be a variable or a Hole.
    */
  case class RecordRowExtend(labels: List[RecordLabelType], rest: DisplayType) extends DisplayType

  //////////
  // Schemas
  //////////

  /**
    * A schema constructor. `arg` should be a variable or a Hole.
    */
  case class SchemaConstructor(arg: DisplayType) extends DisplayType

  /**
    * An unextended schema.
    */
  case class Schema(fields: List[PredicateFieldType]) extends DisplayType

  /**
    * An extended schema. `arg` should be a variable or a Hole.
    */
  case class SchemaExtend(fields: List[PredicateFieldType], rest: DisplayType) extends DisplayType

  /**
    * An unextended schema row.
    */
  case class SchemaRow(fields: List[PredicateFieldType]) extends DisplayType

  /**
    * An extended schema row. `arg` should be a variable or a Hole.
    */
  case class SchemaRowExtend(fields: List[PredicateFieldType], rest: DisplayType) extends DisplayType

  //////////////////////////////
  // Extensible Variants Schemas
  //////////////////////////////

  /**
    * A schema constructor. `arg` should be a variable or a Hole.
    */
  case class ExtSchemaConstructor(arg: DisplayType) extends DisplayType

  /**
    * An unextended schema.
    */
  case class ExtSchema(fields: List[PredicateFieldType]) extends DisplayType

  /**
    * An extended schema. `arg` should be a variable or a Hole.
    */
  case class ExtSchemaExtend(fields: List[PredicateFieldType], rest: DisplayType) extends DisplayType

  /**
    * An unextended schema row.
    */
  case class ExtSchemaRow(fields: List[PredicateFieldType]) extends DisplayType

  /**
    * An extended schema row. `arg` should be a variable or a Hole.
    */
  case class ExtSchemaRowExtend(fields: List[PredicateFieldType], rest: DisplayType) extends DisplayType

  ////////////////////
  // Boolean Operators
  ////////////////////

  /**
    * Boolean negation.
    */
  case class Not(tpe: DisplayType) extends DisplayType

  /**
    * A chain of types connected by `and`.
    */
  case class And(tpes: List[DisplayType]) extends DisplayType

  /**
    * A chain of types connected by `or`.
    */
  case class Or(tpes: List[DisplayType]) extends DisplayType

  ////////////////
  // Set Operators
  ////////////////

  /**
    * Set complement.
    */
  case class Complement(tpe: DisplayType) extends DisplayType

  /**
    * A chain of types in a set.
    */
  case class Union(tpes: List[DisplayType]) extends DisplayType

  /**
    * A chain of types connected by `&`.
    */
  case class Plus(tpes: List[DisplayType]) extends DisplayType

  /**
    * A chain of types connected by `&`.
    */
  case class Intersection(tpes: List[DisplayType]) extends DisplayType

  /**
    * A chain of types connected by `⊕`.
    */
  case class SymmetricDiff(tpes: List[DisplayType]) extends DisplayType

  /**
    * A chain of types connected by `-`.
    */
  case class Difference(tpes: List[DisplayType]) extends DisplayType

  /////////////
  // Predicates
  /////////////

  case object RelationConstructor extends DisplayType

  /**
    * A relation over a list of types.
    */
  case class Relation(tpes: List[DisplayType]) extends DisplayType

  case object LatticeConstructor extends DisplayType

  /**
    * A lattice over a list of types.
    */
  case class Lattice(tpes: List[DisplayType], lat: DisplayType) extends DisplayType

  ////////////
  // Functions
  ////////////

  /**
    * A pure function.
    */
  case class PureArrow(arg: DisplayType, ret: DisplayType) extends DisplayType

  /**
    * A function with a purity.
    */
  case class PolyArrow(arg: DisplayType, eff: DisplayType, ret: DisplayType) extends DisplayType

  /**
    * A backend function (no effect).
    */
  case class ArrowWithoutEffect(arg: DisplayType, ret: DisplayType) extends DisplayType

  ///////
  // Tags
  ///////

  case class TagConstructor(name: String) extends DisplayType

  ////////////
  // JVM Types
  ////////////

  case class JvmToType(tpe: DisplayType) extends DisplayType

  case class JvmToEff(tpe: DisplayType) extends DisplayType

  case class JvmUnresolvedConstructor(name: String, tpes: List[DisplayType]) extends DisplayType

  case class JvmUnresolvedField(tpe: DisplayType, name: String) extends DisplayType

  case class JvmUnresolvedMethod(tpe: DisplayType, name: String, tpes: List[DisplayType]) extends DisplayType

  case class JvmUnresolvedStaticMethod(clazz: String, name: String, tpes: List[DisplayType]) extends DisplayType

  case class JvmConstructor(constructor: Constructor[?]) extends DisplayType

  case class JvmField(field: Field) extends DisplayType

  case class JvmMethod(method: Method) extends DisplayType

  //////////////////////
  // Miscellaneous Types
  //////////////////////

  /**
    * A simple named type (e.g., enum or type alias).
    */
  case class Name(name: String) extends DisplayType

  /**
    * A type applied to one or more types.
    */
  case class Apply(tpe: DisplayType, tpes: List[DisplayType]) extends DisplayType

  /**
    * A type variable.
    */
  case class Var(id: Int, kind: Kind, text: VarText) extends DisplayType

  /**
    * A tuple.
    */
  case class Tuple(tpes: List[DisplayType]) extends DisplayType

  /**
    * A region.
    */
  case class Region(name: String) extends DisplayType

  /**
    * An error type.
    */
  case object Error extends DisplayType

  /////////
  // Fields
  /////////

  /**
    * A record label name and its type.
    */
  case class RecordLabelType(name: String, tpe: DisplayType)

  /**
    * A common supertype for schema predicates.
    */
  sealed trait PredicateFieldType {
    val name: String
  }

  /**
    * A relation field name and its types.
    */
  case class RelationFieldType(name: String, tpes: List[DisplayType]) extends PredicateFieldType

  /**
    * A lattice field name, its types, and its lattice.
    */
  case class LatticeFieldType(name: String, tpes: List[DisplayType], lat: DisplayType) extends PredicateFieldType

  /**
    * A predicate field type that's not actually a predicate.
    */
  case class NonPredFieldType(name: String, tpe: DisplayType) extends PredicateFieldType

  /**
    * Creates a simple type from the well-kinded type `t`.
    */
  def fromWellKindedType(t0: Type): DisplayType = {

    def visit(t: Type): DisplayType = t.baseType match {
      case Type.Var(sym, _) =>
        mkApply(Var(sym.id, sym.kind, sym.text), t.typeArguments.map(visit))
      case Type.Alias(cst, args, _, _) =>
        mkApply(Name(cst.sym.name), (args ++ t.typeArguments).map(visit))
      case Type.AssocType(cst, arg, _, _) =>
        mkApply(Name(cst.sym.name), (arg :: t.typeArguments).map(visit))
      case Type.JvmToType(tpe, _) =>
        mkApply(DisplayType.JvmToType(visit(tpe)), t.typeArguments.map(visit))
      case Type.JvmToEff(tpe, _) =>
        mkApply(DisplayType.JvmToEff(visit(tpe)), t.typeArguments.map(visit))
      case Type.UnresolvedJvmType(member, _) => member match {
        case JvmMember.JvmConstructor(clazz, tpes) => DisplayType.JvmUnresolvedConstructor(clazz.getSimpleName, tpes.map(visit))
        case JvmMember.JvmMethod(tpe, name, tpes) => DisplayType.JvmUnresolvedMethod(visit(tpe), name.name, tpes.map(visit))
        case JvmMember.JvmField(_, tpe, name) => DisplayType.JvmUnresolvedField(visit(tpe), name.name)
        case JvmMember.JvmStaticMethod(clazz, name, tpes) => DisplayType.JvmUnresolvedStaticMethod(clazz.getSimpleName, name.name, tpes.map(visit))
      }
      case Type.Cst(tc, _) => tc match {
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

        case TypeConstructor.Arrow(arity) =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. Fill everything with a hole.
            case Nil =>
              val lastArrow: DisplayType = PolyArrow(Hole, Hole, Hole)
              // NB: safe to subtract 2 since arity is always at least 2
              List.fill(arity - 2)(Hole).foldRight(lastArrow)(PureArrow.apply)

            // Case 2: Pure function.
            case eff :: tpes if eff == Pure =>
              // NB: safe to reduce because arity is always at least 2
              tpes.padTo(arity, Hole).reduceRight(PureArrow.apply)

            // Case 3: Impure function.
            case eff :: tpes =>
              // NB: safe to take last 2 because arity is always at least 2
              val allTpes = tpes.padTo(arity, Hole)
              val List(lastArg, ret) = allTpes.takeRight(2)
              val lastArrow: DisplayType = PolyArrow(lastArg, eff, ret)
              allTpes.dropRight(2).foldRight(lastArrow)(PureArrow.apply)
          }

        case TypeConstructor.ArrowWithoutEffect(arity) =>
          val args = t.typeArguments.map(visit)
          args.padTo(arity, Hole).reduceRight(ArrowWithoutEffect.apply)

        case TypeConstructor.RecordRowEmpty => RecordRow(Nil)

        case TypeConstructor.RecordRowExtend(label) =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. ( name: ? | ? )
            case Nil => RecordRowExtend(RecordLabelType(label.name, Hole) :: Nil, Hole)
            // Case 2: One arg. ( name: tpe | ? )
            case tpe :: Nil => RecordRowExtend(RecordLabelType(label.name, tpe) :: Nil, Hole)
            // Case 3: Fully applied. Dispatch to proper record handler.
            case _ :: _ :: Nil => fromRecordRow(t)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(label.loc)
          }

        case TypeConstructor.Record =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. { ? }
            case Nil => RecordConstructor(Hole)
            // Case 2: One row argument. Extract its values.
            case tpe :: Nil => tpe match {
              case RecordRow(labels) => Record(labels)
              case RecordRowExtend(labels, rest) => RecordExtend(labels, rest)
              case nonRecord => RecordConstructor(nonRecord)
            }
            // Case 3: Too many args. Error.
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Extensible =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. { ? }
            case Nil => ExtSchemaConstructor(Hole)
            // Case 2: One row argument. Extract its values.
            case tpe :: Nil => tpe match {
              case SchemaRow(fields) => ExtSchema(fields)
              case SchemaRowExtend(fields, rest) => ExtSchemaExtend(fields, rest)
              case nonSchema => ExtSchemaConstructor(nonSchema)
            }
            // Case 3: Too many args. Error.
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.SchemaRowEmpty => SchemaRow(Nil)

        case TypeConstructor.SchemaRowExtend(pred) =>
          // erase aliases over the Schema/Relation
          val args = mapHead(t.typeArguments, Type.eraseTopAliases).map(visit)
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
            case _ :: _ :: _ :: _ => throw new OverAppliedType(pred.loc)
          }

        case TypeConstructor.Schema =>
          val args = t.typeArguments.map(visit)
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
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Array => mkApply(Array, t.typeArguments.map(visit))
        case TypeConstructor.ArrayWithoutRegion => mkApply(ArrayWithoutRegion, t.typeArguments.map(visit))
        case TypeConstructor.Vector => mkApply(Vector, t.typeArguments.map(visit))
        case TypeConstructor.Sender => mkApply(Sender, t.typeArguments.map(visit))
        case TypeConstructor.Receiver => mkApply(Receiver, t.typeArguments.map(visit))
        case TypeConstructor.Lazy => mkApply(Lazy, t.typeArguments.map(visit))
        case TypeConstructor.Enum(sym, _) => mkApply(Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.Struct(sym, _) => mkApply(Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.RestrictableEnum(sym, _) => mkApply(Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.Native(clazz) => mkApply(Name(clazz.getName), t.typeArguments.map(visit))
        case TypeConstructor.JvmConstructor(constructor) => mkApply(JvmConstructor(constructor), t.typeArguments.map(visit))
        case TypeConstructor.JvmMethod(method) => mkApply(JvmMethod(method), t.typeArguments.map(visit))
        case TypeConstructor.JvmField(field) => mkApply(JvmField(field), t.typeArguments.map(visit))
        case TypeConstructor.Tuple(l) =>
          val tpes = t.typeArguments.map(visit).padTo(l, Hole)
          Tuple(tpes)
        case TypeConstructor.Relation(l) =>
          val tpes = t.typeArguments.map(visit).padTo(l, Hole)
          Relation(tpes)
        case TypeConstructor.Lattice(l) =>
          val tpesAndLat = t.typeArguments.map(visit).padTo(l, Hole)
          val tpes = tpesAndLat.init
          val lat = tpesAndLat.last
          Lattice(tpes, lat)
        case TypeConstructor.Pure => Pure
        case TypeConstructor.Univ => Univ

        case TypeConstructor.True => True
        case TypeConstructor.False => False

        case TypeConstructor.And =>
          // collapse into a chain of ands
          t.typeArguments.map(visit).map(splitAnds) match {
            // Case 1: No args. ? and ?
            case Nil => And(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 and tpe2 and ?
            case args :: Nil => And(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 and tpe2 and tpe3 and tpe4
            case args1 :: args2 :: Nil => And(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Or =>
          // collapse into a chain of ors
          t.typeArguments.map(visit).map(splitOrs) match {
            // Case 1: No args. ? or ?
            case Nil => Or(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 or tpe2 or ?
            case args :: Nil => Or(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 or tpe2 or tpe3 or tpe4
            case args1 :: args2 :: Nil => Or(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Not => DisplayType.Not(visit(t.typeArguments.head))

        case TypeConstructor.Complement =>
          t.typeArguments.map(visit) match {
            case Nil => Complement(Hole)
            case arg :: Nil => Complement(arg)
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Union =>
          // collapse into a chain of pluses
          t.typeArguments.map(visit).map(splitPluses) match {
            // Case 1: No args. ? + ?
            case Nil => Plus(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 + tpe2 + ?
            case args :: Nil => Plus(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 + tpe2 + tpe3 + tpe4
            case args1 :: args2 :: Nil => Plus(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Intersection =>
          // collapse into a chain of intersections
          t.typeArguments.map(visit).map(splitIntersections) match {
            // Case 1: No args. ? & ?
            case Nil => Intersection(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 & tpe2 & ?
            case args :: Nil => Intersection(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 & tpe2 & tpe3 & tpe4
            case args1 :: args2 :: Nil => Intersection(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Difference =>
          // collapse into a chain of differences
          // take care not to change A - (B - C) into A - B - C
          t.typeArguments.map(visit) match {
            // Case 1: No args. ? - ?
            case Nil => Difference(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 - tpe2 - ?
            case arg :: Nil => Difference(splitDifference(arg) :+ Hole)
            // Case 3: Multiple args. Concatenate the left differences: tpe1 - tpe2 - tpe3 - tpe4
            case arg1 :: arg2 :: Nil => Difference(splitDifference(arg1) :+ arg2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.SymmetricDiff =>
          val args = t.typeArguments.map(visit)
          SymmetricDiff(args)

        case TypeConstructor.CaseSet(syms, _) =>
          val names = syms.toList.map(sym => DisplayType.Name(sym.name))
          val set = DisplayType.Union(names)
          mkApply(set, t.typeArguments.map(visit))

        case TypeConstructor.CaseComplement(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Complement(Hole)
            case arg :: Nil => Complement(arg)
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.CaseIntersection(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Intersection(Hole :: Hole :: Nil)
            case arg :: Nil => Intersection(arg :: Hole :: Nil)
            case arg1 :: arg2 :: Nil => Intersection(arg1 :: arg2 :: Nil)
            case _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.CaseUnion(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Plus(Hole :: Hole :: Nil)
            case arg :: Nil => Plus(arg :: Hole :: Nil)
            case arg1 :: arg2 :: Nil => Plus(arg1 :: arg2 :: Nil)
            case _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Effect(sym, _) => mkApply(DisplayType.Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.Region(sym) => mkApply(DisplayType.Region(sym.text), t.typeArguments.map(visit))
        case TypeConstructor.RegionToStar => mkApply(RegionToStar, t.typeArguments.map(visit))
        case TypeConstructor.RegionWithoutRegion => mkApply(RegionWithoutRegion, t.typeArguments.map(visit))

        case TypeConstructor.Error(_, _) => DisplayType.Error
      }
    }

    visit(t0)
  }

  /**
    * Builds an Apply type.
    */
  private def mkApply(base: DisplayType, args: List[DisplayType]): DisplayType = args match {
    case Nil => base
    case _ :: _ => Apply(base, args)
  }

  /**
    * Transforms the given type, assuming it is a record row.
    */
  private def fromRecordRow(row0: Type): DisplayType = {
    def visit(row: Type): DisplayType = row match {
      // Case 1: A fully applied record row.
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(name), _), tpe, _), rest, _) =>
        val labelType = RecordLabelType(name.name, fromWellKindedType(tpe))
        visit(rest) match {
          // Case 1.1: Unextended row. Put the labels together.
          case DisplayType.RecordRow(labels) => DisplayType.RecordRow(labelType :: labels)
          // Case 1.2: Extended row. Put the labels together.
          case DisplayType.RecordRowExtend(labels, restOfRest) => DisplayType.RecordRowExtend(labelType :: labels, restOfRest)
          // Case 1.3: Non-row. Put it in the "rest" position.
          case nonRecord => DisplayType.RecordRowExtend(labelType :: Nil, nonRecord)
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) => DisplayType.RecordRow(Nil)
      // Case 3: Non-row.
      case nonRecord => fromWellKindedType(nonRecord)
    }

    // sort the labels after converting
    visit(row0) match {
      case RecordRowExtend(labels, rest) => RecordRowExtend(labels.sortBy(_.name), rest)
      case RecordRow(labels) => RecordRow(labels.sortBy(_.name))
      case nonRecord => nonRecord
    }
  }

  /**
    * Transforms the given type, assuming it is a schema row.
    */
  private def fromSchemaRow(row0: Type): DisplayType = {
    def visit(row: Type): DisplayType = row match {
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
          case DisplayType.SchemaRow(fields) => DisplayType.SchemaRow(fieldType :: fields)
          // Case 1.2: Extended row. Put the fields together.
          case DisplayType.SchemaRowExtend(fields, restOfRest) => DisplayType.SchemaRowExtend(fieldType :: fields, restOfRest)
          // Case 1.3: Non-row. Put it in the "rest" position.
          case nonSchema => DisplayType.SchemaRowExtend(fieldType :: Nil, nonSchema)
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.SchemaRowEmpty, _) => DisplayType.SchemaRow(Nil)
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
  private def splitAnds(tpe: DisplayType): List[DisplayType] = tpe match {
    case And(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 or t2` into `t1 :: t2 :: Nil`,
    * and leaves non-or types as singletons.
    */
  private def splitOrs(tpe: DisplayType): List[DisplayType] = tpe match {
    case Or(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 + t2` into `t1 :: t2 :: Nil`,
    * and leaves non-plus types as singletons.
    */
  private def splitPluses(tpe: DisplayType): List[DisplayType] = tpe match {
    case Plus(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 & t2` into `t1 :: t2 :: Nil`,
    * and leaves other types as singletons.
    */
  private def splitIntersections(tpe: DisplayType): List[DisplayType] = tpe match {
    case Intersection(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 - t2` into `t1 :: t2 :: Nil`,
    * and leaves other types as singletons.
    */
  private def splitDifference(tpe: DisplayType): List[DisplayType] = tpe match {
    case Difference(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 ⊕ t2` into `t1 :: t2 :: Nil`,
    * and leaves other types as singletons.
    */
  private def splitSymmetricDiff(tpe: DisplayType): List[DisplayType] = tpe match {
    case SymmetricDiff(tpes) => tpes
    case t => List(t)
  }

  /**
    * Map over the first element in the list, if it exists.
    */
  private def mapHead[A](l: List[A], f: A => A): List[A] = l match {
    case Nil => Nil
    case hd :: tl => f(hd) :: tl
  }
}
