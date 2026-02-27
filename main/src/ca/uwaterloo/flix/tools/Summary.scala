/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, Input, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Kind, Type, TypeConstructor, TypedAst}

object Summary {

  def go(root: Root): Unit = {
    val myDefs = root.defs.filter {
      case (k, _) => k.loc.source.input match {
        case Input.RealFile(_, _) => true
        case _ => false
      }
    }
    val numDefs = myDefs.size
    val allData = myDefs.values.map {
      case defn =>
        val res = visitDef(defn)
        res
    }
    val data = Data.combineAll(allData)
    val header = Data.csvHeader :+ "defs"
    println(header.mkString(","))
    val record = Data.csvRow(data) :+ numDefs
    println(record.mkString(","))
  }

  private def visitDef(defn: TypedAst.Def): Data = defn match {
    case TypedAst.Def(_, spec, _, _) =>
      visitSpec(spec)
  }

  private def isToStar(kind: Kind): Boolean = kind match {
    case Kind.Arrow(_, Kind.Star) => true
    case Kind.Arrow(_, k2) => isToStar(k2)
    case _ => false
  }

  private def isToEff(kind: Kind): Boolean = kind match {
    case Kind.Arrow(_, Kind.Eff) => true
    case Kind.Arrow(_, k2) => isToEff(k2)
    case _ => false
  }

  private def visitSpec(spec: TypedAst.Spec): Data = {
    val tpes = collectSpecTypes(spec)
    // get all the nested types that have proper locations
    val nestedTpes = tpes.flatMap(getNestedTypes).filter(_.loc.isReal)

    // MAGNUS wants:
    // all the star types nested somewhere
    val starTpesNonVar = nestedTpes.filter(_.kind == Kind.Star).filterNot(_.isInstanceOf[Type.Var])
    val starTpesVar = nestedTpes.filter(_.kind == Kind.Star).filter(_.isInstanceOf[Type.Var])

    // all the eff types nested somewhere
    val effTpesNonVar = nestedTpes.filter(_.kind == Kind.Eff).filterNot(_.isInstanceOf[Type.Var])
    val effTpesVar = nestedTpes.filter(_.kind == Kind.Eff).filter(_.isInstanceOf[Type.Var])

    // MATT wants:
    // all the type constructors with a star type
    val starTycons = nestedTpes.collect { case Type.Cst(tc, _) if tc.kind == Kind.Star => tc }

    // all type constructors with a to-star type
    val toStarTycons = nestedTpes.collect { case Type.Cst(tc, _) if isToStar(tc.kind) => tc }

    // all type constructors with an eff type
    val effTycons = nestedTpes.collect { case Type.Cst(tc, _) if tc.kind == Kind.Eff => tc }

    // all type constructors with a to-eff type (type connectives)
    val toEffTycons = nestedTpes.collect { case Type.Cst(tc, _) if isToEff(tc.kind) => tc }

    val starAssocs = nestedTpes.collect { case Type.AssocType(symUse, _, Kind.Star, _) => symUse.sym }

    val effAssocs = nestedTpes.collect { case Type.AssocType(symUse, _, Kind.Eff, _) => symUse.sym }

    // all the type constructors with a star type
    val starVars = nestedTpes.collect { case Type.Var(sym, _) if sym.kind == Kind.Star => sym }

    // all type constructors with a to-star type
    val toStarVars = nestedTpes.collect { case Type.Var(sym, _) if isToStar(sym.kind) => sym }

    // all type constructors with an eff type
    val effVars = nestedTpes.collect { case Type.Var(sym, _) if sym.kind == Kind.Eff => sym }

    // all type constructors with a to-eff type (type connectives)
    val toEffVars = nestedTpes.collect { case Type.Var(sym, _) if isToEff(sym.kind) => sym }

    val effectCat = getEffectCategory(spec.eff)

    val pures = countEq(EffectCategory.Pure, effectCat)
    val ios = countEq(EffectCategory.Io, effectCat)
    val concs = countEq(EffectCategory.Conc, effectCat)
    val vars = countEq(EffectCategory.Var, effectCat)
    val concUnions = countEq(EffectCategory.ConcUnion, effectCat)
    val varUnions = countEq(EffectCategory.VarUnion, effectCat)
    val mixedUnions = countEq(EffectCategory.MixedUnion, effectCat)
    val others = countEq(EffectCategory.Other, effectCat)

    Data(
      starTpesNonVar = toSubData(starTpesNonVar),
      starTpesVar = toSubData(starTpesVar),
      effTpesNonVar = toSubData(effTpesNonVar),
      effTpesVar = toSubData(effTpesVar),
      starTycons = toSubData(starTycons),
      toStarTycons = toSubData(toStarTycons),
      effTycons = toSubData(effTycons),
      toEffTycons = toSubData(toEffTycons),
      starVars = toSubData(starVars),
      toStarVars = toSubData(toStarVars),
      effVars = toSubData(effVars),
      toEffVars = toSubData(toEffVars),
      starAssocs = toSubData(starAssocs),
      effAssocs = toSubData(effAssocs),
      pures = Count(pures),
      ios = Count(ios),
      concs = Count(concs),
      vars = Count(vars),
      concUnions = Count(concUnions),
      varUnions = Count(varUnions),
      mixedUnions = Count(mixedUnions),
      others = Count(others),
    )
  }

  private def countEq[A](x: A, y: A): Int = {
    if (x == y) 1 else 0
  }

  sealed trait EffectCategory {

    def toUnion: EffectCategory.Union = this match {
      case EffectCategory.Pure => EffectCategory.Union.Conc
      case EffectCategory.Io => EffectCategory.Union.Conc
      case EffectCategory.Conc => EffectCategory.Union.Conc
      case EffectCategory.Var => EffectCategory.Union.Var
      case EffectCategory.VarUnion => EffectCategory.Union.Var
      case EffectCategory.ConcUnion => EffectCategory.Union.Conc
      case EffectCategory.MixedUnion => EffectCategory.Union.Mixed
      case EffectCategory.Other => EffectCategory.Union.Other
    }

    def union(that: EffectCategory): EffectCategory = {
      val union1 = this.toUnion
      val union2 = that.toUnion
      val union = union1.union(union2)
      union.toCategory
    }
  }

  object EffectCategory {

    sealed trait Union {
      def toCategory: EffectCategory = this match {
        case Union.Conc => ConcUnion
        case Union.Mixed => MixedUnion
        case Union.Other => Other
        case Union.Var => VarUnion
      }

      def union(that: Union): Union = (this, that) match {
        // Other subsumes all
        case (Union.Other, _) => Union.Other
        case (_, Union.Other) => Union.Other

        // Mixed subsumes all remaining
        case (Union.Mixed, _) => Union.Mixed
        case (_, Union.Mixed) => Union.Mixed

        // Conc + Conc = Conc
        case (Union.Conc, Union.Conc) => Union.Conc

        // Var + Var = Var
        case (Union.Var, Union.Var) => Union.Var

        // Conc + Var = Mixed
        case (Union.Conc, Union.Var) => Union.Conc
        case (Union.Var, Union.Conc) => Union.Conc

        case _ => ??? // should be unreachable
      }
    }

    object Union {
      case object Conc extends Union
      case object Var extends Union
      case object Mixed extends Union
      case object Other extends Union
    }

    case object Pure extends EffectCategory
    case object Io extends EffectCategory
    case object Conc extends EffectCategory
    case object Var extends EffectCategory
    case object ConcUnion extends EffectCategory
    case object VarUnion extends EffectCategory
    case object MixedUnion extends EffectCategory
    case object Other extends EffectCategory
  }

  private def getEffectCategory(eff: Type): EffectCategory = eff match {
    case Type.Cst(TypeConstructor.Pure, _) => EffectCategory.Pure
    case Type.Cst(TypeConstructor.Effect(sym, _), _) if sym.name == "IO" && sym.namespace == Nil => EffectCategory.Io
    case Type.Cst(TypeConstructor.Effect(_, _), _) => EffectCategory.Conc
    case Type.Var(_, _) => EffectCategory.Var
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), eff1, _), eff2, _) =>
      val cat1 = getEffectCategory(eff1)
      val cat2 = getEffectCategory(eff2)
      cat1.union(cat2)
    case _ => EffectCategory.Other
  }

  private def collectSpecTypes(spec: TypedAst.Spec): List[Type] = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe, eff, tconstrs, econstrs) =>
      fparams.flatMap(collectFormalParamTypes) :::
        List(retTpe, eff) :::
        tconstrs.flatMap(collectTraitConstraintTypes) :::
        econstrs.flatMap(collectEqualityConstraintTypes)
  }

  private def collectTraitConstraintTypes(tconstr: TraitConstraint): List[Type] = tconstr match {
    case TraitConstraint(_, tpe, _) => List(tpe)
  }

  private def collectEqualityConstraintTypes(econstr: EqualityConstraint): List[Type] = econstr match {
    case EqualityConstraint(symUse, tpe1, tpe2, loc) => List(Type.AssocType(symUse, tpe1, tpe2.kind, loc), tpe2)
  }

  private def collectFormalParamTypes(fparam: TypedAst.FormalParam): List[Type] = fparam match {
    case TypedAst.FormalParam(_, tpe, _, _) => List(tpe)
  }

  private def toSubData(list: List[?]): Subdata = Subdata(list.length, list.distinct.length)

  private def getNestedTypes(tpe: Type): List[Type] = {
    val tail = tpe match {
      case Type.Var(_, _) => Nil
      case Type.Cst(_, _) => Nil
      case Type.Apply(tpe1, tpe2, _) => getNestedTypes(tpe1) ++ getNestedTypes(tpe2)
      case Type.Alias(_, args, _, _) => args.flatMap(getNestedTypes)
      case Type.AssocType(_, arg, _, _) => getNestedTypes(arg)
      case Type.JvmToType(_, _) => ???
      case Type.JvmToEff(_, _) => ???
      case Type.UnresolvedJvmType(_, _) => ???
    }
    tpe :: tail
  }

  private case class Data(
    starTpesNonVar: Subdata,
    starTpesVar: Subdata,
    effTpesNonVar: Subdata,
    effTpesVar: Subdata,
    starTycons: Subdata,
    toStarTycons: Subdata,
    effTycons: Subdata,
    toEffTycons: Subdata,
    starVars: Subdata,
    toStarVars: Subdata,
    effVars: Subdata,
    toEffVars: Subdata,
    starAssocs: Subdata,
    effAssocs: Subdata,
    pures: Count,
    ios: Count,
    concs: Count,
    vars: Count,
    concUnions: Count,
    varUnions: Count,
    mixedUnions: Count,
    others: Count
  ) {
    def ++(that: Data): Data = Data(
      starTpesNonVar = this.starTpesNonVar ++ that.starTpesNonVar,
      starTpesVar = this.starTpesVar ++ that.starTpesVar,
      effTpesNonVar = this.effTpesNonVar ++ that.effTpesNonVar,
      effTpesVar = this.effTpesVar ++ that.effTpesVar,
      starTycons = this.starTycons ++ that.starTycons,
      toStarTycons = this.toStarTycons ++ that.toStarTycons,
      effTycons = this.effTycons ++ that.effTycons,
      toEffTycons = this.toEffTycons ++ that.toEffTycons,
      starVars = this.starVars ++ that.starVars,
      toStarVars = this.toStarVars ++ that.toStarVars,
      effVars = this.effVars ++ that.effVars,
      toEffVars = this.toEffVars ++ that.toEffVars,
      starAssocs = this.starAssocs ++ that.starAssocs,
      effAssocs = this.effAssocs ++ that.effAssocs,
      pures = this.pures ++ that.pures,
      ios = this.ios ++ that.ios,
      concs = this.concs ++ that.concs,
      vars = this.vars ++ that.vars,
      concUnions = this.concUnions ++ that.concUnions,
      varUnions = this.varUnions ++ that.varUnions,
      mixedUnions = this.mixedUnions ++ that.mixedUnions,
      others = this.others ++ that.others,
    )
  }

  private object Data {
    val empty: Data = Data(
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Subdata.empty,
      Count.empty,
      Count.empty,
      Count.empty,
      Count.empty,
      Count.empty,
      Count.empty,
      Count.empty,
      Count.empty
    )

    def combineAll(data: Iterable[Data]): Data = {
      data.foldLeft(Data.empty)(_ ++ _)
    }

    def csvHeader: List[String] = {
      Data.empty.productElementNames.zip(Data.empty.productIterator).toList.flatMap {
        case (name, Subdata(_, _)) => List(name + "_occs", name + "_uniq")
        case (name, Count(_)) => List(name)
        case _ => ???
      }
    }

    def csvRow(data: Data): List[String] = {
      data.productIterator.toList.flatMap {
        case Subdata(occs, unique) => List(occs.toString, unique.toString)
        case Count(n) => List(n.toString)
        case _ => ??? // impossible
      }
    }
  }

  private case class Subdata(occs: Int, unique: Int) {
    def ++(that: Subdata): Subdata = Subdata(occs + that.occs, unique + that.unique)
  }

  private case object Subdata {
    def empty: Subdata = Subdata(0, 0)
  }

  private case class Count(n: Int) {
    def ++(that: Count): Count = Count(this.n + that.n)
  }

  private case object Count {
    def empty: Count = Count(0)
  }
}
