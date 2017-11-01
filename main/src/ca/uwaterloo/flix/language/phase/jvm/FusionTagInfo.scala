package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.{Symbol, Type}

case class FusionTagInfo(sym: Symbol.EnumSym, tag: String, elms: List[Type]) {
  /**
    * Returns the hash code of `this` fusion info.
    */
  override def hashCode(): Int = 7 * sym.hashCode + 11 * tag.hashCode

  /**
    * Returns `true` if the given `obj` is the same fusion info.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: FusionTagInfo => this.sym == that.sym && this.tag == that.tag && this.elms == that.elms
    case _ => false
  }
}
