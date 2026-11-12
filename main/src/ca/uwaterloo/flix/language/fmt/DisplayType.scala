/*
 * Copyright 2021 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.jvm.{JavaField, JavaMethod}
import ca.uwaterloo.flix.language.jvm.ClassDescs
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ClassDesc

/**
  * A well-kinded type in an easily-printable format.
  */
sealed trait DisplayType

object DisplayType {

  /**
    * An operator node: `not a`, `a and b`, `a -> b`, ...
    *
    * An operator is not atomic, so it is parenthesized when it stands as an operand of another
    * operator or in an argument position. Every other node is atomic and never parenthesized.
    */
  sealed trait Operator extends DisplayType

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
  // Extensible Variants
  //////////////////////////////

  /**
    * An extensible constructor.
    *
    * `arg` should be a variable or a Hole.
    */
  case class ExtensibleUnknown(arg: DisplayType) extends DisplayType

  /** An unextended extensible type. */
  case class Extensible(fields: List[PredicateFieldType]) extends DisplayType

  /**
    * An extended extensible type.
    *
    * `arg` should be a variable or a Hole.
    */
  case class ExtensibleExtend(fields: List[PredicateFieldType], rest: DisplayType) extends DisplayType

  ////////////////////
  // Boolean Operators
  ////////////////////

  /**
    * Boolean negation.
    */
  case class Not(tpe: DisplayType) extends Operator

  /**
    * A chain of types connected by `and`.
    */
  case class And(tpes: List[DisplayType]) extends Operator

  /**
    * A chain of types connected by `or`.
    */
  case class Or(tpes: List[DisplayType]) extends Operator

  ////////////////
  // Set Operators
  ////////////////

  /**
    * Set complement.
    */
  case class Complement(tpe: DisplayType) extends Operator

  /**
    * A chain of types in a set.
    */
  case class Union(tpes: List[DisplayType]) extends DisplayType

  /**
    * A chain of types connected by `&`.
    */
  case class Plus(tpes: List[DisplayType]) extends Operator

  /**
    * A chain of types connected by `&`.
    */
  case class Intersection(tpes: List[DisplayType]) extends Operator

  /**
    * A chain of types connected by `⊕`.
    */
  case class SymmetricDiff(tpes: List[DisplayType]) extends Operator

  /**
    * A chain of types connected by `-`.
    */
  case class Difference(tpes: List[DisplayType]) extends Operator

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
  case class PureArrow(arg: DisplayType, ret: DisplayType) extends Operator

  /**
    * A function with a purity.
    */
  case class PolyArrow(arg: DisplayType, eff: DisplayType, ret: DisplayType) extends Operator

  /**
    * A backend function (no effect).
    */
  case class ArrowWithoutEffect(arg: DisplayType, ret: DisplayType) extends Operator

  ////////////
  // JVM Types
  ////////////

  case class JvmToType(tpe: DisplayType) extends DisplayType

  case class JvmToEff(tpe: DisplayType) extends DisplayType

  case class JvmUnresolvedConstructor(name: String, tpes: List[DisplayType]) extends DisplayType

  case class JvmUnresolvedField(tpe: DisplayType, name: String) extends DisplayType

  case class JvmUnresolvedMethod(tpe: DisplayType, name: String, tpes: List[DisplayType]) extends DisplayType

  case class JvmUnresolvedStaticMethod(clazz: String, name: String, tpes: List[DisplayType]) extends DisplayType

  case class JvmConstructor(constructor: JavaMethod) extends DisplayType

  case class JvmField(field: JavaField) extends DisplayType

  case class JvmMethod(method: JavaMethod) extends DisplayType

  ////////////////
  // Declarations
  ////////////////

  /**
    * An enum, by its symbol.
    */
  case class Enum(sym: Symbol.EnumSym) extends DisplayType

  /**
    * A struct, by its symbol.
    */
  case class Struct(sym: Symbol.StructSym) extends DisplayType

  /**
    * A restrictable enum, by its symbol.
    */
  case class RestrictableEnum(sym: Symbol.RestrictableEnumSym) extends DisplayType

  /**
    * A case of a restrictable enum in a case set, by its symbol.
    */
  case class Case(sym: Symbol.RestrictableCaseSym) extends DisplayType

  /**
    * A type alias, by its symbol.
    */
  case class Alias(sym: Symbol.TypeAliasSym) extends DisplayType

  /**
    * An associated type, by its symbol.
    */
  case class Assoc(sym: Symbol.AssocTypeSym) extends DisplayType

  /**
    * An effect, by its symbol.
    */
  case class Effect(sym: Symbol.EffSym) extends DisplayType

  /**
    * A Java class, by its descriptor.
    */
  case class Native(desc: ClassDesc) extends DisplayType

  //////////////////////
  // Miscellaneous Types
  //////////////////////

  /**
    * A type applied to one or more types.
    */
  case class Apply(tpe: DisplayType, tpes: List[DisplayType]) extends DisplayType

  /**
    * A type variable, by its symbol.
    */
  case class Var(sym: Symbol.KindedTypeVarSym) extends DisplayType

  /**
    * A tuple.
    */
  case class Tuple(tpes: List[DisplayType]) extends DisplayType

  /**
    * A region, by its symbol.
    */
  case class Region(sym: Symbol.RegionSym) extends DisplayType

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
        mkApply(Var(sym), t.typeArguments.map(visit))
      case Type.Alias(cst, args, _, _) =>
        mkApply(Alias(cst.sym), (args ++ t.typeArguments).map(visit))
      case Type.AssocType(cst, arg, _, _) =>
        mkApply(Assoc(cst.sym), (arg :: t.typeArguments).map(visit))
      case Type.JvmToType(tpe, _) =>
        mkApply(DisplayType.JvmToType(visit(tpe)), t.typeArguments.map(visit))
      case Type.JvmToEff(tpe, _) =>
        mkApply(DisplayType.JvmToEff(visit(tpe)), t.typeArguments.map(visit))
      case Type.UnresolvedJvmType(member, _) =>
        val base = member match {
          case JvmMember.JvmConstructor(clazz, tpes) => DisplayType.JvmUnresolvedConstructor(ClassDescs.simpleNameOf(clazz), tpes.map(visit))
          case JvmMember.JvmMethod(tpe, name, tpes) => DisplayType.JvmUnresolvedMethod(visit(tpe), name.name, tpes.map(visit))
          case JvmMember.JvmField(_, tpe, name) => DisplayType.JvmUnresolvedField(visit(tpe), name.name)
          case JvmMember.JvmStaticMethod(clazz, name, tpes) => DisplayType.JvmUnresolvedStaticMethod(ClassDescs.simpleNameOf(clazz), name.name, tpes.map(visit))
        }
        mkApply(base, t.typeArguments.map(visit))
      case Type.Cst(tc, _) => tc match {
        case TypeConstructor.Void => mkApply(Void, t.typeArguments.map(visit))
        case TypeConstructor.AnyType => mkApply(AnyType, t.typeArguments.map(visit))
        case TypeConstructor.Unit => mkApply(Unit, t.typeArguments.map(visit))
        case TypeConstructor.Null => mkApply(Null, t.typeArguments.map(visit))
        case TypeConstructor.Bool => mkApply(Bool, t.typeArguments.map(visit))
        case TypeConstructor.Char => mkApply(Char, t.typeArguments.map(visit))
        case TypeConstructor.Float32 => mkApply(Float32, t.typeArguments.map(visit))
        case TypeConstructor.Float64 => mkApply(Float64, t.typeArguments.map(visit))
        case TypeConstructor.BigDecimal => mkApply(BigDecimal, t.typeArguments.map(visit))
        case TypeConstructor.Int8 => mkApply(Int8, t.typeArguments.map(visit))
        case TypeConstructor.Int16 => mkApply(Int16, t.typeArguments.map(visit))
        case TypeConstructor.Int32 => mkApply(Int32, t.typeArguments.map(visit))
        case TypeConstructor.Int64 => mkApply(Int64, t.typeArguments.map(visit))
        case TypeConstructor.BigInt => mkApply(BigInt, t.typeArguments.map(visit))
        case TypeConstructor.Str => mkApply(Str, t.typeArguments.map(visit))
        case TypeConstructor.Regex => mkApply(Regex, t.typeArguments.map(visit))

        case TypeConstructor.Arrow(arity) =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. Fill everything with a hole.
            case Nil =>
              val lastArrow: DisplayType = PolyArrow(Hole, Hole, Hole)
              // NB: an arity below 2 gives no further holes
              List.fill(arity - 2)(Hole).foldRight(lastArrow)(PureArrow.apply)

            // Case 2: Too many args. Apply the type to the rest.
            case _ :: tpes if tpes.lengthIs > arity => overApplied(t, arity + 1)

            // Case 3: Pure function.
            case eff :: tpes if eff == Pure =>
              // NB: safe to reduce because there are at least 2 types
              arrowTypes(tpes, arity).reduceRight(PureArrow.apply)

            // Case 4: Impure function.
            case eff :: tpes =>
              // NB: safe to take last 2 because there are at least 2 types
              val allTpes = arrowTypes(tpes, arity)
              val List(lastArg, ret) = allTpes.takeRight(2)
              val lastArrow: DisplayType = PolyArrow(lastArg, eff, ret)
              allTpes.dropRight(2).foldRight(lastArrow)(PureArrow.apply)
          }

        case TypeConstructor.ArrowWithoutEffect(arity) =>
          t.typeArguments.map(visit) match {
            // Case 1: Too many args. Apply the type to the rest.
            case args if args.lengthIs > arity => overApplied(t, arity)
            // Case 2: Fill the missing args with holes.
            case args => arrowTypes(args, arity).reduceRight(ArrowWithoutEffect.apply)
          }

        case TypeConstructor.RecordRowEmpty => mkApply(RecordRow(Nil), t.typeArguments.map(visit))

        case TypeConstructor.RecordRowExtend(label) =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. ( name: ? | ? )
            case Nil => RecordRowExtend(RecordLabelType(label.name, Hole) :: Nil, Hole)
            // Case 2: One arg. ( name: tpe | ? )
            case tpe :: Nil => RecordRowExtend(RecordLabelType(label.name, tpe) :: Nil, Hole)
            // Case 3: Fully applied. Dispatch to proper record handler.
            case _ :: _ :: Nil => fromRecordRow(t)
            // Case 4: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
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
            // Case 3: Too many args. Apply the type to the rest.
            case _ :: _ :: _ => overApplied(t, 1)
          }

        case TypeConstructor.Extensible =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. { ? }
            case Nil => ExtensibleUnknown(Hole)
            // Case 2: One row argument. Extract its values.
            case tpe :: Nil => tpe match {
              case SchemaRow(fields) => Extensible(fields)
              case SchemaRowExtend(fields, rest) => ExtensibleExtend(fields, rest)
              case nonSchema => ExtensibleUnknown(nonSchema)
            }
            // Case 3: Too many args. Apply the type to the rest.
            case _ :: _ :: _ => overApplied(t, 1)
          }

        case TypeConstructor.SchemaRowEmpty => mkApply(SchemaRow(Nil), t.typeArguments.map(visit))

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
            // Case 6: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
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
            // Case 3: Too many args. Apply the type to the rest.
            case _ :: _ :: _ => overApplied(t, 1)
          }

        case TypeConstructor.Array => mkApply(Array, t.typeArguments.map(visit))
        case TypeConstructor.ArrayWithoutRegion => mkApply(ArrayWithoutRegion, t.typeArguments.map(visit))
        case TypeConstructor.Vector => mkApply(Vector, t.typeArguments.map(visit))
        case TypeConstructor.Sender => mkApply(Sender, t.typeArguments.map(visit))
        case TypeConstructor.Receiver => mkApply(Receiver, t.typeArguments.map(visit))
        case TypeConstructor.Lazy => mkApply(Lazy, t.typeArguments.map(visit))
        case TypeConstructor.Enum(sym, _) => mkApply(Enum(sym), t.typeArguments.map(visit))
        case TypeConstructor.Struct(sym, _) => mkApply(Struct(sym), t.typeArguments.map(visit))
        case TypeConstructor.RestrictableEnum(sym, _) => mkApply(RestrictableEnum(sym), t.typeArguments.map(visit))
        case TypeConstructor.Native(desc, _) => mkApply(Native(desc), t.typeArguments.map(visit))
        case TypeConstructor.JvmConstructor(constructor) => mkApply(JvmConstructor(constructor), t.typeArguments.map(visit))
        case TypeConstructor.JvmMethod(method, _) => mkApply(JvmMethod(method), t.typeArguments.map(visit))
        case TypeConstructor.JvmField(field) => mkApply(JvmField(field), t.typeArguments.map(visit))
        case TypeConstructor.Tuple(l) =>
          t.typeArguments.map(visit) match {
            // Case 1: Too many args. Apply the type to the rest.
            case tpes if tpes.lengthIs > l => overApplied(t, l)
            // Case 2: Fill the missing args with holes.
            case tpes => Tuple(tpes.padTo(l, Hole))
          }
        case TypeConstructor.Relation(l) =>
          t.typeArguments.map(visit) match {
            // Case 1: Too many args. Apply the type to the rest.
            case tpes if tpes.lengthIs > l => overApplied(t, l)
            // Case 2: Fill the missing args with holes.
            case tpes => Relation(tpes.padTo(l, Hole))
          }
        case TypeConstructor.Lattice(l) =>
          t.typeArguments.map(visit) match {
            // Case 1: Too many args. Apply the type to the rest.
            case tpes if tpes.lengthIs > l => overApplied(t, l)
            // Case 2: A lattice of no types has no lattice type to show.
            case _ if l == 0 => LatticeConstructor
            // Case 3: Fill the missing args with holes.
            case tpes =>
              val tpesAndLat = tpes.padTo(l, Hole)
              Lattice(tpesAndLat.init, tpesAndLat.last)
          }
        case TypeConstructor.Pure => mkApply(Pure, t.typeArguments.map(visit))
        case TypeConstructor.Univ => mkApply(Univ, t.typeArguments.map(visit))

        case TypeConstructor.True => mkApply(True, t.typeArguments.map(visit))
        case TypeConstructor.False => mkApply(False, t.typeArguments.map(visit))

        case TypeConstructor.And =>
          // collapse into a chain of ands
          t.typeArguments.map(visit).map(splitAnds) match {
            // Case 1: No args. ? and ?
            case Nil => And(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 and tpe2 and ?
            case args :: Nil => And(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 and tpe2 and tpe3 and tpe4
            case args1 :: args2 :: Nil => And(args1 ++ args2)
            // Case 4: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
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
            // Case 4: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
          }

        case TypeConstructor.Not =>
          t.typeArguments.map(visit) match {
            case Nil => DisplayType.Not(Hole)
            case arg :: Nil => DisplayType.Not(arg)
            case _ :: _ :: _ => overApplied(t, 1)
          }

        case TypeConstructor.Complement =>
          t.typeArguments.map(visit) match {
            case Nil => Complement(Hole)
            case arg :: Nil => Complement(arg)
            case _ :: _ :: _ => overApplied(t, 1)
          }

        case TypeConstructor.Union =>
          // collapse into a chain of pluses
          t.typeArguments.map(visit).map(splitPluses) match {
            // Case 1: No args. ? + ?
            case Nil => Plus(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 + tpe2 + ?
            case args :: Nil => Plus(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 + tpe2 + tpe3 + tpe4
            case args1 :: args2 :: Nil => mkPlus(args1 ++ args2)
            // Case 4: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
          }

        case TypeConstructor.Intersection =>
          // collapse into a chain of intersections
          t.typeArguments match {
            // Case 1: No args. ? & ?
            case Nil => Intersection(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 & tpe2 & ?
            case arg :: Nil => Intersection(splitIntersections(visit(arg)) :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 & tpe2 & tpe3 & tpe4
            // The whole chain is collected before any of it is visited, so that a complement is
            // subtracted from the chain and not from the operand it happens to be nested with.
            case _ :: _ :: Nil => mkIntersection(intersectionOperands(t).map(visit).flatMap(splitIntersections))
            // Case 4: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
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
            // Case 4: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
          }

        case TypeConstructor.SymmetricDiff =>
          t.typeArguments.map(visit).map(splitSymmetricDiff) match {
            // Case 1: No args. ? ⊕ ?
            case Nil => SymmetricDiff(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 ⊕ ?
            case arg :: Nil => SymmetricDiff(arg :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 ⊕ tpe2
            case arg1 :: arg2 :: Nil => SymmetricDiff(arg1 ++ arg2)
            // Case 4: Too many args. Apply the type to the rest.
            case _ :: _ :: _ :: _ => overApplied(t, 2)
          }

        case TypeConstructor.CaseSet(syms, _) =>
          val names = syms.toList.map(sym => DisplayType.Case(sym))
          val set = DisplayType.Union(names)
          mkApply(set, t.typeArguments.map(visit))

        case TypeConstructor.CaseComplement(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Complement(Hole)
            case arg :: Nil => Complement(arg)
            case _ :: _ :: _ => overApplied(t, 1)
          }

        case TypeConstructor.CaseIntersection(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Intersection(Hole :: Hole :: Nil)
            case arg :: Nil => Intersection(arg :: Hole :: Nil)
            case arg1 :: arg2 :: Nil => Intersection(arg1 :: arg2 :: Nil)
            case _ => overApplied(t, 2)
          }

        case TypeConstructor.CaseUnion(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Plus(Hole :: Hole :: Nil)
            case arg :: Nil => Plus(arg :: Hole :: Nil)
            case arg1 :: arg2 :: Nil => Plus(arg1 :: arg2 :: Nil)
            case _ => overApplied(t, 2)
          }

        case TypeConstructor.CaseSymmetricDiff(_) =>
          t.typeArguments.map(visit) match {
            case Nil => SymmetricDiff(Hole :: Hole :: Nil)
            case arg :: Nil => SymmetricDiff(arg :: Hole :: Nil)
            case arg1 :: arg2 :: Nil => SymmetricDiff(arg1 :: arg2 :: Nil)
            case _ => overApplied(t, 2)
          }

        case TypeConstructor.Effect(sym, _) => mkApply(DisplayType.Effect(sym), t.typeArguments.map(visit))
        case TypeConstructor.Region(sym) => mkApply(DisplayType.Region(sym), t.typeArguments.map(visit))
        case TypeConstructor.RegionToStar => mkApply(RegionToStar, t.typeArguments.map(visit))
        case TypeConstructor.RegionWithoutRegion => mkApply(RegionWithoutRegion, t.typeArguments.map(visit))

        // NB: An error type can have any kind, so its arguments say nothing and are not shown.
        case TypeConstructor.Error(_, _) => DisplayType.Error
      }
      case Type.Apply(_, _, loc) => throw InternalCompilerException("unexpected Type.Apply", loc)
    }

    /**
      * Returns the view of the type `t`, which has more arguments than the `n` its constructor takes:
      * the view of `t` with its first `n` arguments, applied to the rest.
      *
      * A well-kinded type is never over-applied, but an ill-kinded one is shown as it stands, e.g.
      * `{ x = Int32 }[String]`, so that an error message can print it.
      */
    def overApplied(t: Type, n: Int): DisplayType = {
      val (args, rest) = t.typeArguments.splitAt(n)
      mkApply(visit(Type.mkApply(t.baseType, args, t.loc)), rest.map(visit))
    }

    visit(t0)
  }

  /**
    * Returns the parameter types and the result type of an arrow of the given `arity` that is applied
    * to the types `tpes`, with a hole for each type that is missing.
    *
    * An arrow is written with a parameter and a result, so there are at least two types. An arrow of
    * an arity below 2, which no source program has, gets a hole for the parameter, and for the result,
    * that it has no place for, e.g. `? -> a`.
    */
  private def arrowTypes(tpes: List[DisplayType], arity: Int): List[DisplayType] = {
    val padded = tpes.padTo(arity, Hole)
    List.fill(2 - padded.length)(Hole) ::: padded
  }

  /**
    * Builds an Apply type.
    */
  private def mkApply(base: DisplayType, args: List[DisplayType]): DisplayType = args match {
    case Nil => base
    case _ :: _ => Apply(base, args)
  }

  /**
    * Returns the union of the given `tpes`, simplified: a pure member and a repeated member are
    * dropped, and the rest keep their order. A union with the universe as a member is the universe.
    *
    * Only the union is simplified. A member that is an intersection, a difference or a complement
    * is kept as it is.
    */
  private def mkPlus(tpes: List[DisplayType]): DisplayType = {
    if (tpes.contains(Hole)) {
      // A hole stands for a missing argument, so two holes are not the same member.
      Plus(tpes)
    } else if (tpes.contains(Univ)) {
      Univ
    } else {
      tpes.filterNot(_ == Pure).distinct match {
        case Nil => Pure
        case tpe :: Nil => tpe
        case members => Plus(members)
      }
    }
  }

  /**
    * Returns the operands of the intersection `tpe`, where an operand that is itself an intersection
    * gives its operands.
    */
  private def intersectionOperands(tpe: Type): List[Type] = tpe match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) =>
      intersectionOperands(tpe1) ::: intersectionOperands(tpe2)
    case _ => List(tpe)
  }

  /**
    * Returns the intersection of the given `tpes`, as a difference if one of them is a complement:
    * `ef & ~A` is `ef - A`, which is how the effect of a handler is written in a signature.
    *
    * An intersection of nothing but complements is kept as it is.
    */
  private def mkIntersection(tpes: List[DisplayType]): DisplayType = {
    val (complements, rest) = tpes.partition {
      case Complement(_) => true
      case _ => false
    }
    if (complements.isEmpty || rest.isEmpty) {
      Intersection(tpes)
    } else {
      val base = rest match {
        case tpe :: Nil => tpe
        case _ => Intersection(rest)
      }
      val subtracted = complements.collect { case Complement(tpe) => tpe }
      Difference(splitDifference(base) ++ subtracted)
    }
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
