package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.ErasedAst.Expression
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}

/**
  * Meta information about a closure.
  */
case class ClosureInfo(sym: Symbol.DefnSym, closureArgs: List[Expression], tpe: MonoType) {
  /**
    * Returns the hash code of `this` closure info.
    */
  override def hashCode(): Int = 7 * sym.hashCode + 11 * closureArgs.hashCode()

  /**
    * Returns `true` if the given `obj` is the same closure info as `this`.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ClosureInfo => this.sym == that.sym && this.closureArgs == that.closureArgs
    case _ => false
  }
}
