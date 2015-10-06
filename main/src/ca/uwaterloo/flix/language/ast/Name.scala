package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.util.CachedHash

object Name {

  /**
   * Represents an identifier.
   *
   * @param sp1 the position of the first character in the literal.
   * @param name the identifier.
   * @param sp2 the position of the last character in the literal.
   */
  case class Ident(sp1: SourcePosition, name: String, sp2: SourcePosition) extends CachedHash {
    /**
     * The source location of `this` unresolved name.
     */
    val loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
     * Returns a human readable string representation of the identifier.
     */
    val format: String = name
  }

  /**
   * Represents an unresolved name.
   *
   * @param sp1 the position of the first character in the literal.
   * @param parts the name parts.
   * @param sp2 the position of the last character in the literal.
   */
  case class Unresolved(sp1: SourcePosition, parts: List[String], sp2: SourcePosition) extends CachedHash {
    /**
     * The source location of `this` unresolved name.
     */
    val loc: SourceLocation = SourceLocation.mk(sp1, sp2)

    /**
     * Returns a human readable string representation of the unresolved name.
     */
    val format: String = parts.mkString("::")
  }

  /**
   * Represents a resolved name.
   *
   * @param parts the parts of the name.
   */
  case class Resolved(parts: List[String]) extends CachedHash {
    /**
     * Returns a human readable string representation of the resolved name.
     */
    val format: String = parts.mkString("::")
  }


}
