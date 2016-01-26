package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Name}

package object phase {

  class GenSym() {
    private var number: Int = 0

    //  TODO: return Name.Ident
    def fresh(): SimplifiedAst.Expression.Var = ???

    // TODO: Maintain map from s to Int
    def of(s: Name.Ident): Int = {
      number = number + 1
      number
    }
  }

}
