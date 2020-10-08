package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{ResolvedAst, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.util.Result.{ToErr, ToOk}
import ca.uwaterloo.flix.util.collection.MultiMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec

// MATT license
// MATT maybe change name to ClassUnification or something like that
object ContextReduction {

  /**
    * Returns `true` iff type constraints `tconstrs0` entail type constraint `tconstr`, under class environment `instances`.
    * That is, `tconstr` is true if all of `tconstrs0` are true.
    */
  // MATT THIH says that toncstrs0 should always be in HNF so checking for byInst is a waste.
  def entail(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Boolean = {
    // tconstrs0 is unused for now; it will be used when superclasses are implemented
    byInst(instances, tconstr) match {
      case Result.Ok(tconstrs) => tconstrs.forall(entail(instances, tconstrs0, _))
      case Result.Err(_) => false
    }
  }

  /**
    * Removes the type constraints which are entailed by the others in the list.
    */
  private def simplify(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint])(implicit flix: Flix): List[TypedAst.TypeConstraint] = {

    @tailrec
    def loop(acc: List[TypedAst.TypeConstraint], tconstrs0: List[TypedAst.TypeConstraint]): List[TypedAst.TypeConstraint] = tconstrs0 match {
      case Nil => acc
      case head :: tail if entail(instances, acc ++ tail, head) => loop(acc, tail)
      case head :: tail => loop(head :: acc, tail)
    }

    loop(tconstrs0, Nil)
  }

  /**
    * Normalizes a list of type constraints, converting to head-normal form and removing semantic duplicates.
    */
  private def reduce(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstrs0: List[TypedAst.TypeConstraint])(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    for {
      tconstrs <- Result.sequence(tconstrs0.map(toHeadNormalForm(instances, _)))
    } yield simplify(instances, tconstrs.flatten)
  }

  /**
    * Converts the type constraint to head-normal form, i.e. `a[X1, Xn]`, where `a` is a variable and `n >= 0`.
    */
  private def toHeadNormalForm(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    if (isHeadNormalForm(tconstr.arg)) {
      List(tconstr).toOk
    } else {
      byInst(instances, tconstr)
    }
  }

  /**
    * Returns the list of constraints that hold if the given constraint `tconstr` hold.
    */
  private def byInst(instances0: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], tconstr: TypedAst.TypeConstraint)(implicit flix: Flix): Result[List[TypedAst.TypeConstraint], UnificationError] = {
    val matchingInstances = instances0(tconstr.sym).toList

    def tryInst(inst: ResolvedAst.Instance): Result[List[TypedAst.TypeConstraint], UnificationError] = {
      for {
        subst <- Unification.unifyTypes(tconstr.arg, inst.tpe)
      } yield inst.tconstrs.map(subst(_))
    }

    // MATT move to result or something
    def isOk(result: Result[_, _]): Boolean = result match {
      case Result.Ok(_) => true
      case Result.Err(_) => false
    }

    matchingInstances.map(tryInst).filter(isOk) match {
      case Result.Ok(tconstrs) :: Nil => tconstrs.toOk
      case Nil => UnificationError.MismatchedTypes(Type.Unit, Type.Unit).toErr // MATT UnificationError.NoMatchingInstance
      case _ => throw InternalCompilerException("Multiple matching instances.")
    }
  }

  /**
    * Returns true iff this type is in head-normal form.
    */
  private def isHeadNormalForm(tpe: Type): Boolean = {
    tpe.typeConstructor.isEmpty
  }

  // MATT docs
  def split(instances: MultiMap[Symbol.ClassSym, ResolvedAst.Instance], fixedVars: List[Type.Var], quantifiedVars: List[Type.Var], tconstrs: List[TypedAst.TypeConstraint])(implicit flix: Flix): Result[(List[TypedAst.TypeConstraint], List[TypedAst.TypeConstraint]), UnificationError] = {
    for {
      tconstrs1 <- reduce(instances, tconstrs)
      (deferred, retained) = tconstrs1.partition(_.arg.typeVars.forall(fixedVars.contains))
      // MATT defaulted predicates here (?)
    } yield (deferred, retained)
  }
}
