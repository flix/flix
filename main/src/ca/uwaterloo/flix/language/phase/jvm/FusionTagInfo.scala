package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.{Symbol, Type}

/**
  * Meta information about a fused tag and tuple.
  */
case class FusionTagInfo(sym: Symbol.EnumSym, tag: String, enumType: Type, tupleType: Type, elms: List[Type]) {
  /**
    * Returns the hash code of `this` fusion info.
    */
  override def hashCode(): Int = 7 * sym.hashCode + 11 * tag.hashCode + 13 * elms.hashCode()

  /**
    * Returns `true` if the given `obj` is the same fusion info.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: FusionTagInfo => this.sym == that.sym && this.tag == that.tag && this.elms == that.elms
    case _ => false
  }
}
