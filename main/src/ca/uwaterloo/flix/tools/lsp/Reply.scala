package ca.uwaterloo.flix.tools.lsp

import org.json4s.JValue
import org.json4s.JsonAST.{JField, JObject, JString}

trait Reply {
  def toJSON: JObject
}

object Reply {

  case class NotFound() extends Reply {
    def toJSON: JObject = {
      JObject(
        JField("status", JString("success")),
        JField("result", JString("unknown"))
      )
    }
  }

}
