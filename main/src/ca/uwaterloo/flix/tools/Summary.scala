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
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Kind, Type, TypedAst}

object Summary {

  def go(root: Root): Unit = {
    val numDefs = root.defs.size
    val allData = root.defs.values.map {
      case defn =>
        val res = visitDef(defn)
        if (res.effTpesVar.occs + res.effTpesNonVar.occs > res.starTpesVar.occs + res.starTpesNonVar.occs) {
          println("AHH")
        }
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
    )
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
    )

    def combineAll(data: Iterable[Data]): Data = {
      data.foldLeft(Data.empty)(_ ++ _)
    }

    def csvHeader: List[String] = {
      Data.empty.productElementNames.toList.flatMap {
        case name => List(name + "_occs", name + "_uniq")
      }
    }

    def csvRow(data: Data): List[String] = {
      data.productIterator.toList.flatMap {
        case Subdata(occs, unique) => List(occs.toString, unique.toString)
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
}
