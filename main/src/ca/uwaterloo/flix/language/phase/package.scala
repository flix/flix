package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{SourcePosition, SimplifiedAst, Name}

import scala.util.Random

package object phase {

  // TODO: Cleanup

  class GenSym() {
    private var number: Int = new Random().nextInt() // TODO

    // TODO: Consider allowing a "seed" variable.
    def fresh2(): Name.Ident = fresh2("tmp")

    def fresh2(prefix: String): Name.Ident = {
      number = number + 1
      Name.Ident(SourcePosition.Unknown, prefix + number, SourcePosition.Unknown)
    }

  }

}
