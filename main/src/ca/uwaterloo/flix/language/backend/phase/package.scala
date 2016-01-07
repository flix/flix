package ca.uwaterloo.flix.language.backend

package object phase {

  class GenSym() {
    private var number: Int = 0

    def apply(): Int = {
      number = number + 1
      number
    }
  }

}
