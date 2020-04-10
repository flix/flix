package ca.uwaterloo.flix.tools.lsp

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import org.json4s.JsonAST.{JField, JObject, JString}

/**
  * A common super-type for language server replies.
  */
trait Reply {
  /**
    * Returns `this` reply as a JSON object.
    */
  def toJSON: JObject
}

object Reply {

  /**
    * A reply that represents the type and effect of an expression.
    */
  case class EffAndTypeOf(exp: Expression) {
    def toJSON: JObject = {
      val tpe = exp.tpe.toString
      val eff = exp.eff.toString
      val result = s"$tpe & $eff"
      JObject(
        JField("status", JString("success")),
        JField("result", JString(result))
      )
    }
  }

  /**
    * A reply that represents that the specified entity was not found.
    */
  case class NotFound() extends Reply {
    def toJSON: JObject = {
      JObject(
        JField("status", JString("failure")),
        JField("result", JString("Not Found"))
      )
    }
  }

}
