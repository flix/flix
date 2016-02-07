package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{SourcePosition, SimplifiedAst, Name}

import scala.util.Random

package object phase {

  // TODO: Cleanup

  class GenSym() {
    private var number: Int = new Random().nextInt(5000) // TODO

    // TODO: Consider allowing a "seed" variable.
    def fresh2(): Name.Ident = {
      number = number + 1
      Name.Ident(SourcePosition.Unknown, "tmp" + number, SourcePosition.Unknown)
    }

  }

}
