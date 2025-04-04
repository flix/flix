package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.CofiniteEffSet

import java.util.Objects
import scala.collection.mutable


/** This type simplification is purely focused on user readability. */
object TypeSimplifier {

  /**
    * Simplifies types and effects intended for user readability.
    *
    * This function will never crash, but non-well-kinded types are not simplified.
    */
  def simplify(tpe: Type): Type = {
    if (tpe.kind == Kind.Eff) {
      simplifyEffect(tpe)
    } else {
      simplifyType(tpe)
    }
  }

  /** Simplify `tpe` by simplifying effects inside. */
  private def simplifyType(tpe: Type): Type = {
    val (base0, args0) = tpe.asFullApply
    base0 match {
      case tvar@Type.Var(_, _) =>
        val base = tvar
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case cst@Type.Cst(_, _) =>
        val base = cst
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.Alias(cst, aliasArgs, tpe, loc) =>
        val base = Type.Alias(cst, aliasArgs.map(simplify), simplify(tpe), loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.AssocType(cst, arg, kind, loc) =>
        val base = Type.AssocType(cst, simplify(arg), kind, loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.JvmToType(tpe, loc) =>
        val base = Type.JvmToType(simplify(tpe), loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.JvmToEff(tpe, loc) =>
        val base = Type.JvmToEff(simplify(tpe), loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case jvmType@Type.UnresolvedJvmType(_, _) =>
        val base = jvmType
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.Apply(_, _, _) =>
        // `tpe.asFullApply` never returns `Type.Apply` as the base.
        // Leave the input as is to avoid throwing exceptions.
        tpe
    }
  }

  /** Simplifies `tpe` as an effect, also simplifying each non-formula subtype within `tpe`. */
  private def simplifyEffect(tpe: Type): Type = {
    toFormula(tpe).toType(tpe.loc)
  }

  /** Convert well-formed formulas into [[Formula]], leaving nonsense as [[ChunkVar]]. */
  private def toFormula(tpe: Type): Union = {
    val (base0, args0) = tpe.asFullApply
    (base0, args0) match {
      case (Type.Var(sym, _), List()) => Union(List(Intersection(List(Var(sym, isCompl = false)))))
      case (Type.Cst(TypeConstructor.Pure, _), Nil) => Empty
      case (Type.Cst(TypeConstructor.Univ, _), Nil) => Univ
      case (Type.Cst(TypeConstructor.Effect(sym), _), Nil) => eff(sym)
      case (Type.Cst(TypeConstructor.Complement, _), List(x)) => compl(toFormula(x))
      case (Type.Cst(TypeConstructor.Union, _), List(x, y)) => union(List(toFormula(x), toFormula(y)))
      case (Type.Cst(TypeConstructor.Intersection, _), List(x, y)) => intersection(List(toFormula(x), toFormula(y)))
      case (Type.Cst(TypeConstructor.Difference, _), List(x, y)) => difference(toFormula(x), toFormula(y))
      case (Type.Cst(TypeConstructor.SymmetricDiff, _), List(x, y)) => symmetricDifference(toFormula(x), toFormula(y))
      case (v@Type.Var(_, _), _) =>
        val base = v
        val args = args0.map(simplify)
        chunkVar(base, args, tpe.loc)
      case (cst@Type.Cst(_, _), _) =>
        val base = cst
        val args = args0.map(simplify)
        chunkVar(base, args, tpe.loc)
      case (Type.Alias(cst, aliasArgs, tpe, loc), _) =>
        val base = Type.Alias(cst, aliasArgs.map(simplify), simplify(tpe), loc)
        val args = args0.map(simplify)
        chunkVar(base, args, tpe.loc)
      case (Type.AssocType(cst, arg, kind, loc), _) =>
        val base = Type.AssocType(cst, simplify(arg), kind, loc)
        val args = args0.map(simplify)
        chunkVar(base, args, tpe.loc)
      case (Type.JvmToType(tpe, loc), _) =>
        val base = Type.JvmToType(simplify(tpe), loc)
        val args = args0.map(simplify)
        chunkVar(base, args, tpe.loc)
      case (Type.JvmToEff(tpe, loc), _) =>
        val base = Type.JvmToEff(simplify(tpe), loc)
        val args = args0.map(simplify)
        chunkVar(base, args, tpe.loc)
      case (jvmType@Type.UnresolvedJvmType(_, _), _) =>
        val base = jvmType
        val args = args0.map(simplify)
        chunkVar(base, args, tpe.loc)
      case (app@Type.Apply(_, _, _), _) =>
        // `tpe.asFullApply` never returns `Type.Apply` as the base.
        // Leave the input as is to avoid throwing exceptions.
        chunkVar(app, args0.map(simplify), tpe.loc)
    }
  }

  private sealed trait Formula {
    /** Converts `this` into a type with location `loc`. */
    def toType(loc: SourceLocation): Type = this match {
      case Var(sym, isCompl) =>
        if (isCompl) Type.mkComplement(Type.Var(sym, loc), loc)
        else Type.Var(sym, loc)
      case Eff(s) => TypeSimplifier.toType(s, loc)
      case ChunkVar(tpe, isCompl) =>
        if (isCompl) Type.mkComplement(tpe, loc)
        else tpe
      case Union(List(one)) => one.toType(loc)
      case Union(fs) => Type.mkUnion(fs.map(_.toType(loc)), loc)
      case Intersection(List(one)) => one.toType(loc)
      case i@Intersection(_) =>
        val (cst, us) = i.split()
        val (neg0, pos0) = us.partition(_.isCompl)
        val (neg, pos) = cst match {
          case CofiniteEffSet.empty => (Nil, List(Eff(cst)))
          case CofiniteEffSet.universe => (neg0, pos0)
          case CofiniteEffSet.Set(_) => (neg0, Eff(cst) :: pos0)
          case CofiniteEffSet.Compl(_) => (Eff(cst) :: neg0, pos0)
        }
        val negUnion = Type.mkUnion(neg.map(compl).map(_.toType(loc)), loc)
        Type.mkDifference(Type.mkIntersection(pos.map(_.toType(loc)), loc), negUnion, loc)
    }

    /** Returns the string representation of `this`. */
    override def toString: String = this match {
      case Eff(s) => s match {
        case CofiniteEffSet.empty => "Empty"
        case CofiniteEffSet.universe => "Univ"
        case CofiniteEffSet.Set(s) => s.mkString("+")
        case CofiniteEffSet.Compl(s) => s.mkString("!(", "+", ")")
      }
      case Var(sym, isCompl) =>
        if (isCompl) s"!${sym.toString}"
        else sym.toString
      case ChunkVar(_, isCompl) =>
        if (isCompl) s"!chunkVar"
        else s"chunkVar"
      case Union(fs) => fs.mkString(" ∪ ")
      case Intersection(fs) => fs.mkString(" ∩ ")
    }
  }

  /** Atoms are the singular sub-parts of [[Formula]]s (e.g. concrete effects or variables). */
  private sealed trait Atom extends Formula

  /** Unknowns are [[Atom]]s with unknown set evaluations (e.g. variables). */
  private sealed trait Unknown extends Atom {
    def isCompl: Boolean
  }

  /** A concrete co-finite effect set (e.g. `{Crash, IO}` or `-{Flip}`). */
  private case class Eff(s: CofiniteEffSet) extends Atom

  /** A variable that is potentially complemented. */
  private case class Var(sym: Symbol.KindedTypeVarSym, isCompl: Boolean) extends Unknown

  /**
    * A variable that represents some arbitrary type with equivalence defined by `id` and `isCompl`.
    *
    * This allows formulas with "broken" parts, like `IO + ef + Error` or even malformed formulas
    * like `IO + List[Int32]`. The types are given an identifier such that equivalence with itself
    * remains known, even if the atom is duplicated.
    */
  private case class ChunkVar(private val tpe: Type, isCompl: Boolean) extends Unknown {
    override def equals(obj: Any): Boolean = obj match {
      case ChunkVar(tpe, isCompl) => (tpe eq this.tpe) && isCompl == this.isCompl
      case _ => false
    }

    override def hashCode(): Int = {
      Objects.hash(tpe, isCompl)
    }
  }

  /** A non-empty union of intersections. */
  private case class Union(fs: List[Intersection]) extends Formula {
    assert(fs.nonEmpty)
  }

  /** A non-empty intersection of atoms. */
  private case class Intersection(fs: List[Atom]) extends Formula {
    assert(fs.nonEmpty)

    def split(): (CofiniteEffSet, List[Unknown]) = {
      var ces = CofiniteEffSet.universe
      val us = mutable.Set[Unknown]()
      fs.foreach {
        case Var(sym, isCompl) if us.contains(Var(sym, !isCompl)) => return (CofiniteEffSet.empty, Nil)
        case v@Var(_, _) => us.add(v)
        case ChunkVar(tpe, isCompl) if us.contains(ChunkVar(tpe, !isCompl)) => return (CofiniteEffSet.empty, Nil)
        case c@ChunkVar(_, _) => us.add(c)
        case Eff(s) =>
          ces = CofiniteEffSet.intersection(ces, s)
          if (ces.isEmpty) return (ces, Nil)
      }
      (ces, us.toList)
    }
  }

  /** The universe formula as a [[Union]]. */
  private val Univ: Union = Union(List(Intersection(List(Eff(CofiniteEffSet.universe)))))

  /** The empty formula as a [[Union]]. */
  private val Empty: Union = Union(List(Intersection(List(Eff(CofiniteEffSet.empty)))))

  /** A singular effect as a [[Union]]. */
  private def eff(sym: Symbol.EffectSym): Union = {
    Union(List(Intersection(List(Eff(CofiniteEffSet.mkSet(sym))))))
  }

  /** A singular type as a [[Union]], interpreted as a variable of unknown effect meaning. */
  private def chunkVar(base: Type, args: List[Type], loc: SourceLocation): Union = {
    Union(List(Intersection(List(ChunkVar(Type.mkApply(base, args, loc), isCompl = false)))))
  }

  private def compl(f: Atom): Atom = f match {
    case Var(sym, isCompl) => Var(sym, !isCompl)
    case Eff(sym) => Eff(CofiniteEffSet.complement(sym))
    case ChunkVar(tpe, isCompl) => ChunkVar(tpe, !isCompl)
  }

  private def compl(f: Intersection): Union = f match {
    case Intersection(fs) => Union(fs.map(f => Intersection(List(compl(f)))))
  }

  private def compl(f: Union): Union = f match {
    case Union(fs) => intersection(fs.map(compl))
  }

  private def union(fs: List[Union]): Union = {
    unionIntersections(fs.iterator.flatMap(_.fs))
  }

  private def unionIntersections(fs: IterableOnce[Intersection]): Union = {
    val inters = fs.iterator.map(_.split()).toList.groupBy(_._2).map {
      case (uns, grouped) =>
        val cst = grouped.iterator.map(_._1).foldLeft(CofiniteEffSet.empty)(CofiniteEffSet.union)
        if (cst.isEmpty) Intersection(List(Eff(cst)))
        else if (cst.isUniverse && uns.nonEmpty) Intersection(uns)
        else Intersection(Eff(cst) :: uns)
    }.toList
    if (inters.contains(Intersection(List(Eff(CofiniteEffSet.universe)))))
      Univ
    else Union(inters.filter(i => i.fs.nonEmpty))
  }

  private def intersection(fs: List[Union]): Union = {
    fs.foldLeft(Univ) {
      case (acc, u) => union(acc.fs.map(f => intersection(f, u)))
    }
  }

  private def intersection(i: Intersection, u: Union): Union = {
    unionIntersections(u.fs.map(f => intersection(List(f, i))))
  }

  private def intersection(fs: List[Intersection]): Intersection = {
    val (cst, us) = Intersection(fs.iterator.flatMap(_.fs).toList).split()
    Intersection(Eff(cst) :: us)
  }

  private def difference(f1: Union, f2: Union): Union = {
    intersection(List(f1, compl(f2)))
  }

  private def symmetricDifference(f1: Union, f2: Union): Union = {
    val v1 = difference(f1, f2)
    val v2 = difference(f2, f1)
    union(List(v1, v2))
  }

  private def toType(s: CofiniteEffSet, loc: SourceLocation): Type = s match {
    case CofiniteEffSet.empty => Type.Pure
    case CofiniteEffSet.universe => Type.Univ
    case CofiniteEffSet.Set(s) =>
      Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc)
    case CofiniteEffSet.Compl(s) =>
      Type.mkComplement(Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc), loc)
  }

}
