package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.FinalAst.FreeVar
import ca.uwaterloo.flix.language.ast.{Symbol, MonoType}

/**
  * Meta information about a closure.
  */
case class ClosureInfo(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: MonoType) {
  /**
    * Returns the hash code of `this` closure info.
    */
  override def hashCode(): Int = 7 * sym.hashCode + 11 * freeVars.hashCode()

  /**
    * Returns `true` if the given `obj` is the same closure info as `this`.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ClosureInfo => this.sym == that.sym && this.freeVars == that.freeVars
    case _ => false
  }
}
