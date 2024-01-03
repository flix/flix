package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LiftedAst.Root

object Eraser {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Eraser"){
    root
  }

}
