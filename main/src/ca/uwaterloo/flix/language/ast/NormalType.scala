package ca.uwaterloo.flix.language.ast

import java.util.Objects

sealed trait NormalType {

}

object NormalType {

  /**
    * A type represented by the type constructor `tc`.
    */
  case class Cst(tc: TypeConstructor) extends NormalType {
    override def hashCode(): Int = tc.hashCode()

    override def equals(o: Any): Boolean = o match {
      case that: Cst => this.tc == that.tc
      case _ => false
    }
  }

  /**
    * A type expression that a represents a type application tpe1[tpe2].
    */
  case class Apply(tpe1: NormalType, tpe2: NormalType) extends NormalType {

    override def equals(o: Any): Boolean = o match {
      case that: Apply => this.tpe1 == that.tpe1 && this.tpe2 == that.tpe2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tpe1, tpe2)
  }

  /**
    * Returns the type `Complement(tpe0)`.
    */
  def mkComplement(tpe0: NormalType): NormalType = tpe0 match {
    case NormalType.Cst(TypeConstructor.Pure) => NormalType.Cst(TypeConstructor.EffUniv)
    case NormalType.Cst(TypeConstructor.EffUniv) => NormalType.Cst(TypeConstructor.Pure)
    case _ => ???
  }

  /**
    * Returns the type `Not(tpe0)`.
    */
  def mkNot(tpe0: NormalType): NormalType = tpe0 match {
    case NormalType.Cst(TypeConstructor.True) => NormalType.Cst(TypeConstructor.False)
    case NormalType.Cst(TypeConstructor.False) => NormalType.Cst(TypeConstructor.True)
    case _ => ???
  }

  /**
    * Returns the type `And(tpe1, tpe2)`.
    */
  def mkAnd(tpe1: NormalType, tpe2: NormalType): NormalType = (tpe1, tpe2) match {
    case (NormalType.Cst(TypeConstructor.True), NormalType.Cst(TypeConstructor.True)) => NormalType.Cst(TypeConstructor.True)
    case (NormalType.Cst(TypeConstructor.True), NormalType.Cst(TypeConstructor.False)) => NormalType.Cst(TypeConstructor.False)
    case (NormalType.Cst(TypeConstructor.False), NormalType.Cst(TypeConstructor.True)) => NormalType.Cst(TypeConstructor.False)
    case (NormalType.Cst(TypeConstructor.False), NormalType.Cst(TypeConstructor.False)) => NormalType.Cst(TypeConstructor.False)
    case _ => ???
  }

  /**
    * Returns the type `Or(tpe1, tpe2)`.
    */
  def mkOr(tpe1: NormalType, tpe2: NormalType): NormalType = (tpe1, tpe2) match {
    case (NormalType.Cst(TypeConstructor.True), NormalType.Cst(TypeConstructor.True)) => NormalType.Cst(TypeConstructor.True)
    case (NormalType.Cst(TypeConstructor.True), NormalType.Cst(TypeConstructor.False)) => NormalType.Cst(TypeConstructor.True)
    case (NormalType.Cst(TypeConstructor.False), NormalType.Cst(TypeConstructor.True)) => NormalType.Cst(TypeConstructor.True)
    case (NormalType.Cst(TypeConstructor.False), NormalType.Cst(TypeConstructor.False)) => NormalType.Cst(TypeConstructor.False)
    case _ => ???
  }

  /**
    * Returns the type `Union(tpe1, tpe2)`.
    */
  def mkUnion(tpe1: NormalType, tpe2: NormalType): NormalType = (tpe1, tpe2) match {
    case (NormalType.Cst(TypeConstructor.Pure), NormalType.Cst(TypeConstructor.Pure)) => NormalType.Cst(TypeConstructor.Pure)
    case (NormalType.Cst(TypeConstructor.Pure), NormalType.Cst(TypeConstructor.EffUniv)) => NormalType.Cst(TypeConstructor.EffUniv)
    case (NormalType.Cst(TypeConstructor.EffUniv), NormalType.Cst(TypeConstructor.Pure)) => NormalType.Cst(TypeConstructor.EffUniv)
    case (NormalType.Cst(TypeConstructor.EffUniv), NormalType.Cst(TypeConstructor.EffUniv)) => NormalType.Cst(TypeConstructor.EffUniv)
    case _ => ???
  }

  /**
    * Returns the type `Intersection(tpe1, tpe2)`.
    */
  def mkIntersection(tpe1: NormalType, tpe2: NormalType): NormalType = (tpe1, tpe2) match {
    case (NormalType.Cst(TypeConstructor.Pure), NormalType.Cst(TypeConstructor.Pure)) => NormalType.Cst(TypeConstructor.Pure)
    case (NormalType.Cst(TypeConstructor.Pure), NormalType.Cst(TypeConstructor.EffUniv)) => NormalType.Cst(TypeConstructor.Pure)
    case (NormalType.Cst(TypeConstructor.EffUniv), NormalType.Cst(TypeConstructor.Pure)) => NormalType.Cst(TypeConstructor.Pure)
    case (NormalType.Cst(TypeConstructor.EffUniv), NormalType.Cst(TypeConstructor.EffUniv)) => NormalType.Cst(TypeConstructor.EffUniv)
    case _ => ???
  }

  /**
    * Returns the complement of the given type.
    */
  def mkCaseComplement(tpe: NormalType): NormalType = tpe match {
    case NormalType.Cst(TypeConstructor.CaseSet(syms, enumSym)) =>
      NormalType.Cst(TypeConstructor.CaseSet(enumSym.universe - syms, enumSym))
  }

  /**
    * Returns the type `tpe1 + tpe2`
    */
  def mkCaseUnion(tpe1: NormalType, tpe2: NormalType): NormalType = (tpe1, tpe2) match {
    case (NormalType.Cst(TypeConstructor.CaseSet(syms1, enumSym)), NormalType.Cst(TypeConstructor.CaseSet(syms2, alsoEnumSym))) =>
      assert(enumSym == alsoEnumSym)
      NormalType.Cst(TypeConstructor.CaseSet(syms1.union(syms2), enumSym))
    case _ => ???
  }

  /**
    * Returns the type `tpe1 & tpe2`
    */
  def mkCaseIntersection(tpe1: NormalType, tpe2: NormalType): NormalType = (tpe1, tpe2) match {
    case (NormalType.Cst(TypeConstructor.CaseSet(syms1, enumSym)), NormalType.Cst(TypeConstructor.CaseSet(syms2, alsoEnumSym))) =>
      assert(enumSym == alsoEnumSym)
      NormalType.Cst(TypeConstructor.CaseSet(syms1.intersect(syms2), enumSym))
    case _ => ???
  }


  def toType(tpe: NormalType): Type = tpe match {
    case Cst(tc) => Type.Cst(tc, SourceLocation.Unknown)
    case Apply(tpe1, tpe2) => Type.Apply(toType(tpe1), toType(tpe2), SourceLocation.Unknown)
  }

}
