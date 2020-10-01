package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}

import scala.annotation.tailrec

object ContextReduction {
  private def toHeadNormalForm(classEnv: Map[Symbol.ClassSym, TypedAst.Class], tconstr: TypedAst.TypeConstraint): Option[List[TypedAst.TypeConstraint]] = {
    if (isHeadNormalForm(tconstr.arg)) {
      Some(List(tconstr))
    } else {
      ???
      /*
      byInst(classEnv, tconstr) match {
        case None => error
        case Some(preds) => preds.map(toHeadNormalForm)
      }
       */
    }
  }

  private def byInst(classEnv: Map[Symbol.ClassSym, TypedAst.Class], tconstr: TypedAst.TypeConstraint): Option[List[TypedAst.TypeConstraint]] = {
    val instances = tconstr match {
      case TypedAst.TypeConstraint(sym, _) => insts(classEnv, sym)
    }

    // for each instance matching this class
    // get substitution from instance.tpe to tconstr.tpe
    // if it works, apply it to the requirement predicates
    // return mapped requirement predicates
    // (if no instance works, error)
    def tryInst(inst: TypedAst.Instance)
  }

  // THIH class(list of superclasses, list of instances)
  // THIH instance(qual of pred) ~= list of preds (like Show[a]) => pred (like Show[List[a]])
  // this can be implemented for flix as:
    // instance(requirements: list of preds, class: the class this implements, type: the type this implements)
    // or we can promote typeconstraint to be used outside typedast (kinda doing that already)

  private def insts(classEnv: Map[Symbol.ClassSym, TypedAst.Class], sym: Symbol.ClassSym): List[TypedAst.TypeConstraint] = {
    ??? // lookup all instances of the class matching this constraint; should use instance env instead
    // can just do simple Map access if we have Symbol.ClassSym -> List[Instance] map
  }

  @tailrec
  private def isHeadNormalForm(tpe: Type): Boolean = tpe match {
    case _: Type.Var => true
    case _: Type.Cst => false
    case Type.Apply(t, _) => isHeadNormalForm(t)
  }
}
