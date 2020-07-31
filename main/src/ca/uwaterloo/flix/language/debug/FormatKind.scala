package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Kind

object FormatKind {

  /**
    * Create a string representation of the kind.
    */
  def formatKind(kind: Kind): String = kind match {
    case Kind.Var(id) => s"`$id"
    case Kind.Star => "*"
    case Kind.Bool => "Bool"
    case Kind.Record => "Record"
    case Kind.Schema => "Schema"
    // parenthesize the left because `->` is right-associative
    case Kind.Arrow(k1: Kind.Arrow, k2) => s"(${formatKind(k1)}) -> ${formatKind(k2)}"
    case Kind.Arrow(k1, k2) => s"${formatKind(k1)} -> ${formatKind(k2)}"
  }

}
