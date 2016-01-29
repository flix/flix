package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{SourcePosition, SimplifiedAst, Name}

package object phase {

  class GenSym() {
    private var number: Int = 0

    // TODO: Consider allowing a "seed" variable.
    def fresh2(): Name.Ident = {
      number = number + 1
      Name.Ident(SourcePosition.Unknown, "tmp" + number, SourcePosition.Unknown)
    }

    //  TODO: return Name.Ident
    def fresh(): SimplifiedAst.Expression.Var = ???



    // TODO: Maintain map from s to Int
    def of(s: Name.Ident): Int = {
      number = number + 1
      number
    }
  }

}
