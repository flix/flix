package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.annotation.tailrec

// MATT maybe change name to ClassUnification or something like that
object ContextReduction {

  // MATT docs
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entails(instances: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Boolean = {
    // tconstrs0 is unused for now; it will be used when superclasses are implemented
    byInst(instances, tconstr) match {
      case Some(tconstrs) => tconstrs.forall(entails(instances, tconstrs0, _))
      case None => false
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
  def reduce(instances: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint])(implicit flix: Flix): Option[List[TypedAst.TypeConstraint]] = {
    def sequence[T](list: List[Option[T]]): Option[List[T]] = { // MATT change to validations
      Option.unless(list contains None)(list.map(_.get))
    }
    for {
      tconstrs <- sequence(tconstrs0.map(toHeadNormalForm(instances, _)))
    } yield simplify(instances, tconstrs.flatten)
  }

  // MATT docs
  private def toHeadNormalForm(instances: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Option[List[TypedAst.TypeConstraint]] = {
    if (isHeadNormalForm(tconstr.arg)) {
      Some(List(tconstr))
    } else {
      byInst(instances, tconstr)
    }
  }

  // MATT docs
  private def byInst(instances0: MultiMap[Symbol.ClassSym, TypedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Option[List[TypedAst.TypeConstraint]] = {
    val matchingInstances = instances0(tconstr.sym)

    // MATT docs
    def tryInst(inst: TypedAst.Instance): Option[List[TypedAst.TypeConstraint]] = {
      for {
        subst <- toOption(Unification.unifyTypes(tconstr.arg, inst.tpe))
      } yield inst.tconstrs.map(subst(_))
    }

    matchingInstances.flatMap(tryInst).headOption // MATT maybe assert that there is only one
  }


  /**
    * Returns `this` result as an [[Option]].
    */
  def toOption[T](result: Result[T, _]): Option[T] = result match {
    case Result.Ok(t) => Some(t)
    case Result.Err(_) => None
  }


  // MATT docs
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }
}
