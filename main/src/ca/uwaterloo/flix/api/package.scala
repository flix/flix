package ca.uwaterloo.flix

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.runtime.Value

package object api {

  case class FlixApiError(format: String) extends RuntimeException with FlixError

  // TODO: Add final
  protected abstract class ValueWrapper(value: Value) extends IValue {
    def getType: IType = new TypeWrapper(???)

    // TODO: Override hashcode and equals
  }



}
