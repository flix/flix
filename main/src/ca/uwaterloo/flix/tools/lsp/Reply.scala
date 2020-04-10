package ca.uwaterloo.flix.tools.lsp

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
