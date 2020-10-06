package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.ToOk
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.annotation.tailrec

// MATT license
// MATT maybe change name to ClassUnification or something like that
object ContextReduction {

  // MATT docs
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entails(instances: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Boolean = {
    // tconstrs0 is unused for now; it will be used when superclasses are implemented
    byInst(instances, tconstr) match {
      case Result.Ok(tconstrs) => tconstrs.forall(entails(instances, tconstrs0, _))
      case Result.Err(_) => false
    }
  }

  // MATT docs
  def simplify(instances: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint])(implicit flix: Flix): List[TypedAst.TypeConstraint] = {

    // MATT docs
    @tailrec
    def loop(acc: List[TypedAst.TypeConstraint], tconstrs0: List[TypedAst.TypeConstraint]): List[TypedAst.TypeConstraint] = tconstrs0 match {
      case Nil => acc
      case head :: tail if entails(instances, acc ++ tail, head) => loop(acc, tail)
      case head :: tail => loop(head :: acc, tail)
    }

    loop(tconstrs0, Nil)
  }

  // MATT docs
  def reduce(instances: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint])(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    for {
      tconstrs <- Result.sequence(tconstrs0.map(toHeadNormalForm(instances, _)))
    } yield simplify(instances, tconstrs.flatten)
  }

  // MATT docs
  private def toHeadNormalForm(instances: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    if (isHeadNormalForm(tconstr.arg)) {
      List(tconstr).toOk
    } else {
      byInst(instances, tconstr)
    }
  }

  // MATT docs
  private def byInst(instances0: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    val matchingInstances = instances0(tconstr.sym).toList

    // MATT docs
    def tryInst(inst: TypedAst.Instance): Result[List[TypedAst.TypeConstraint], UnificationError] = {
      for {
        subst <- Unification.unifyTypes(tconstr.arg, inst.tpe)
      } yield inst.tconstrs.map(subst(_))
    }

    for {
      tconstrs0 <- Result.sequence(matchingInstances.map(tryInst))
    } yield {
      tconstrs0.headOption match {
        case Some(tconstrs) => tconstrs.toOk
        case None => ??? // MATT UnificationError.NoMatchingInstance
        // MATT maybe assert that there is only one
      }
    }
  }

  // MATT docs
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }
}
