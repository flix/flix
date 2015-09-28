package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.util.CachedHash

object Name {

  /**
   * Represents an identifier.
   *
   * @param name the identifier.
   * @param location the source location of the identifier.
   */
  case class Ident(name: String, location: SourceLocation) extends CachedHash {
    /**
     * Returns a human readable string representation of the identifier.
     */
    val format: String = name
  }

  /**
   * Represents an unresolved name.
   *
   * @param parts the name parts.
   *
   * @param location the source location of the first name part.
   */
  case class Unresolved(parts: List[String], location: SourceLocation) extends CachedHash {
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
