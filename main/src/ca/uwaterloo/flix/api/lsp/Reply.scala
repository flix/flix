/*
 * Copyright 2020 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import org.json4s.JsonAST.{JArray, JField, JObject, JString}

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
    * A reply that represents that compilation was successful.
    */
  case class CompilationSuccess(time: Long, version: String) extends Reply {
    def toJSON: JObject = {
      JObject(
        JField("status", JString("success")),
        JField("time", JString(time.toString)),
        JField("version", JString(version))
      )
    }
  }

  /**
    * A reply that represents that compilation was unsuccessful.
    */
  case class CompilationFailure(results: List[PublishDiagnosticsParams]) extends Reply {
    def toJSON: JObject =
      JObject(
        JField("status", JString("failure")),
        JField("results", JArray(results.map(_.toJSON)))
      )
  }

  /**
    * A reply that represents the type and effect of an expression.
    */
  case class EffAndTypeOf(exp: Expression) extends Reply {
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
    * A reply that represents a link to a definition.
    */
  case class GotoDef(locationLink: LocationLink) extends Reply {
    def toJSON: JObject =
      JObject(
        JField("status", JString("success")),
        JField("locationLink", locationLink.toJSON),
      )
  }

  /**
    * A reply that represents a link to a variable.
    */
  case class GotoVar(locationLink: LocationLink) extends Reply {
    def toJSON: JObject =
      JObject(
        JField("status", JString("success")),
        JField("locationLink", locationLink.toJSON),
      )
  }

  /**
    * A reply that represents that the specified entity was not found.
    */
  case class NotFound() extends Reply {
    def toJSON: JObject =
      JObject(
        JField("status", JString("failure")),
        JField("result", JString("Not Found"))
      )
  }

}
