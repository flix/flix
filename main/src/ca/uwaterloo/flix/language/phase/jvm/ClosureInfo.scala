package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.ExecutableAst.FreeVar
import ca.uwaterloo.flix.language.ast.{Symbol, Type}

/**
  * Meta information about a closure.
  */
case class ClosureInfo(sym: Symbol.DefnSym, freeVars: List[FreeVar], tpe: Type) {
  /**
    * Returns the hash code of `this` closure info.
    */
  override def hashCode(): Int = sym.hashCode

  /**
    * Returns `true` if the given `obj` is the same closure info as `this`.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ClosureInfo => this.sym == that.sym
    case _ => false
  }
}
