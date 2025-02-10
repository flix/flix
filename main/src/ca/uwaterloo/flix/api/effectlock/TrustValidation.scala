package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst

object TrustValidation {

  sealed trait TrustError

  def run(root: TypedAst.Root)(implicit flix: Flix): List[TrustError] = {
    List.empty
  }

}
