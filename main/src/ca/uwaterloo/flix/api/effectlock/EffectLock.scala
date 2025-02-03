package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object EffectLock {

  def isSafe(sc1: Scheme, sc2: Scheme): Boolean = {
    println(sc1)
    println(sc2)
    isSafe(sc1.base, sc2.base)
  }

  private def isSafe(tpe1: Type, tpe2: Type): Boolean = (tpe1, tpe2) match {
    case (Type.Var(sym1, loc1), Type.Var(sym2, loc2)) => throw InternalCompilerException("not impl var", loc1)

    case (Type.Cst(tc1, loc1), Type.Cst(tc2, loc2)) =>
      println(tc1)
      println(tc2)
      isSafe(tc1, tc2)

    case (Type.Apply(tpe11, tpe12, loc1), Type.Apply(tpe21, tpe22, loc2)) =>
      print(s"apply: $tpe11      to      $tpe12")
      print(s"apply: $tpe21      to      $tpe22")
      tpe22.effects.subsetOf(tpe12.effects) &&
        isSafe(tpe11, tpe21) &&
        isSafe(tpe12, tpe22)

    case (Type.Alias(symUse1, args1, tpe1, loc1), Type.Alias(symUse2, args2, tpe2, loc2)) =>
      throw InternalCompilerException("not impl alias", loc1)

    case (Type.AssocType(symUse1, arg1, kind1, loc1), Type.AssocType(symUse2, arg2, kind2, loc2)) =>
      throw InternalCompilerException("not impl assoc type", loc1)

    case _ => false
  }

  private def isSafe(tc1: TypeConstructor, tc2: TypeConstructor): Boolean =
    (tc1, tc2) match {
      case (TypeConstructor.Void, TypeConstructor.Void) => true
      case (TypeConstructor.AnyType, TypeConstructor.AnyType) => true
      case (TypeConstructor.Unit, TypeConstructor.Unit) => true
      case (TypeConstructor.Null, TypeConstructor.Null) => true
      case (TypeConstructor.Bool, TypeConstructor.Bool) => true
      case (TypeConstructor.Char, TypeConstructor.Char) => true
      case (TypeConstructor.Float32, TypeConstructor.Float32) => true
      case (TypeConstructor.Float64, TypeConstructor.Float64) => true
      case (TypeConstructor.BigDecimal, TypeConstructor.BigDecimal) => true
      case (TypeConstructor.Int8, TypeConstructor.Int8) => true
      case (TypeConstructor.Int16, TypeConstructor.Int16) => true
      case (TypeConstructor.Int32, TypeConstructor.Int32) => true
      case (TypeConstructor.Int64, TypeConstructor.Int64) => true
      case (TypeConstructor.BigInt, TypeConstructor.BigInt) => true
      case (TypeConstructor.Str, TypeConstructor.Str) => true
      case (TypeConstructor.Regex, TypeConstructor.Regex) => true
      case (TypeConstructor.Arrow(arity1), TypeConstructor.Arrow(arity2)) => arity1 == arity2
      case (TypeConstructor.RecordRowEmpty, TypeConstructor.RecordRowEmpty) => true
      case (TypeConstructor.RecordRowExtend(label1), TypeConstructor.RecordRowExtend(label2)) => true
      case (TypeConstructor.Record, TypeConstructor.Record) => true
      case (TypeConstructor.SchemaRowEmpty, TypeConstructor.SchemaRowEmpty) => true
      case (TypeConstructor.SchemaRowExtend(pred1), TypeConstructor.SchemaRowExtend(pred2)) => true
      case (TypeConstructor.Schema, TypeConstructor.Schema) => true
      case (TypeConstructor.Sender, TypeConstructor.Sender) => true
      case (TypeConstructor.Receiver, TypeConstructor.Receiver) => true
      case (TypeConstructor.Lazy, TypeConstructor.Lazy) => true
      case (TypeConstructor.Enum(sym1, kind1), TypeConstructor.Enum(sym2, kind2)) => true
      case (TypeConstructor.Struct(sym1, kind1), TypeConstructor.Struct(sym2, kind2)) => true
      case (TypeConstructor.RestrictableEnum(sym1, kind1), TypeConstructor.RestrictableEnum(sym2, kind2)) => true
      case (TypeConstructor.Native(clazz1), TypeConstructor.Native(clazz2)) => true
      case (TypeConstructor.Array, TypeConstructor.Array) => true
      case (TypeConstructor.ArrayWithoutRegion, TypeConstructor.ArrayWithoutRegion) => true
      case (TypeConstructor.Vector, TypeConstructor.Vector) => true
      case (TypeConstructor.Tuple(l1), TypeConstructor.Tuple(l2)) => l1 == l2
      case (TypeConstructor.Relation, TypeConstructor.Relation) => true
      case (TypeConstructor.Lattice, TypeConstructor.Lattice) => true
      case (TypeConstructor.True, TypeConstructor.True) => true
      case (TypeConstructor.False, TypeConstructor.False) => true
      case (TypeConstructor.Not, TypeConstructor.Not) => true
      case (TypeConstructor.And, TypeConstructor.And) => true
      case (TypeConstructor.Or, TypeConstructor.Or) => true
      case (TypeConstructor.Pure, TypeConstructor.Pure) => true
      case (TypeConstructor.Univ, TypeConstructor.Univ) => true
      case (TypeConstructor.Complement, TypeConstructor.Complement) => true
      case (TypeConstructor.Union, TypeConstructor.Union) => true
      case (TypeConstructor.Intersection, TypeConstructor.Intersection) => true
      case (TypeConstructor.Difference, TypeConstructor.Difference) => true
      case (TypeConstructor.SymmetricDiff, TypeConstructor.SymmetricDiff) => true
      case (TypeConstructor.Effect(sym1), TypeConstructor.Effect(sym2)) => sym1 == sym2
      case (TypeConstructor.CaseComplement(sym1), TypeConstructor.CaseComplement(sym2)) => true
      case (TypeConstructor.CaseUnion(sym1), TypeConstructor.CaseUnion(sym2)) => true
      case (TypeConstructor.CaseIntersection(sym1), TypeConstructor.CaseIntersection(sym2)) => true
      case (TypeConstructor.CaseSet(syms1, enumSym1), TypeConstructor.CaseSet(syms2, enumSym2)) => true
      case (TypeConstructor.Region(sym1), TypeConstructor.Region(sym2)) => true
      case (TypeConstructor.RegionToStar, TypeConstructor.RegionToStar) => true
      case (TypeConstructor.RegionWithoutRegion, TypeConstructor.RegionWithoutRegion) => true
      case (TypeConstructor.Error(id1, kind1), TypeConstructor.Error(id2, kind2)) => true
      case _ => false
    }

}
