package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{SourcePosition, SimplifiedAst, Name}

import scala.util.Random
import ca.uwaterloo.flix.language.ast.Symbol

package object phase {

  // TODO: Cleanup

  class GenSym() {
    def freshDefinition(): Symbol.Resolved = ???

    private var number: Int = 0

    // TODO: Consider allowing a "seed" variable.
    def fresh2(): Name.Ident = fresh2("tmp")

    def fresh2(prefix: String): Name.Ident = {
      number = number + 1
      Name.Ident(SourcePosition.Unknown, prefix + number, SourcePosition.Unknown)
    }

  }

}
