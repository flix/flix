package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.util.SmartHash

import scala.collection.mutable

object Name {

  /**
   * Represents an identifier.
   *
   * @param sp1 the position of the first character in the literal.
   * @param name the identifier.
   * @param sp2 the position of the last character in the literal.
   */
  case class Ident(sp1: SourcePosition, name: String, sp2: SourcePosition) extends SmartHash {
    /**
     * The source location of `this` unresolved name.
     */
    val loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
     * Returns a human readable string representation of the identifier.
     */
    override val toString: String = name
  }

  /**
   * Represents an unresolved name.
   *
   * @param sp1 the position of the first character in the literal.
   * @param parts the name parts.
   * @param sp2 the position of the last character in the literal.
   */
  case class Unresolved(sp1: SourcePosition, parts: List[String], sp2: SourcePosition) extends SmartHash {
    /**
     * The source location of `this` unresolved name.
     */
    val loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
     * Returns a human readable string representation of the resolved name.
     */
    override val toString: String = "?" + parts.mkString("::")
  }


  /**
   * Companion object for the [[Resolved]] class.
   */
  object Resolved {

    private val cache = mutable.HashMap.empty[List[String], Resolved]

    def mk(parts: List[String]): Resolved = {
      cache.getOrElseUpdate(parts, new Resolved(parts))
    }
  }

  /**
   * Represents a resolved name.
   *
   * @param parts the parts of the name.
   */
  final class Resolved private(val parts: List[String]) {

    /**
     * Returns `true` if this resolved name is equal to `obj` resolved name.
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Resolved => this eq that
      case _ => false
    }

    /**
     * Returns the hash code of this resolved name.
     */
    override val hashCode: Int = parts.hashCode()

    /**
     * Returns a human readable string representation of the resolved name.
     */
    override val toString: String = parts.mkString("::")
  }

}
