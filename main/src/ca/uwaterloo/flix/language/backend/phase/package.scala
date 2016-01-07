package ca.uwaterloo.flix.language.backend

import ca.uwaterloo.flix.language.ast.Name

package object phase {

  class GenSym() {
    private var number: Int = 0

    // TODO: Maintain map from s to Int
    def apply(s: Name.Ident): Int = {
      number = number + 1
      number
    }
  }

}
