/*
 * Copyright 2023 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.json4s.jvalue2monadic
import org.json4s.JsonAST.*

object CodeActionContext {
  def parse(json: JValue): Result[CodeActionContext, String] = {

    val onlyResult: Result[List[CodeActionKind], String] = json \ "only" match {
      case JNothing => Ok(List())
      case JArray(l) => Result.traverse(l)(CodeActionKind.parse)
      case v => Err(s"Unexpected non-array only-field: '$v'.")
    }

    val triggerKindResult: Result[Option[CodeActionTriggerKind], String] = json \ "triggerKind" match {
      case JNothing => Ok(None)
      case v => CodeActionTriggerKind.parse(v).map(Some(_))
    }

    for {
      only <- onlyResult
      triggerKind <- triggerKindResult
    } yield CodeActionContext(only, triggerKind)
  }
}

case class CodeActionContext(only: List[CodeActionKind], triggerKind: Option[CodeActionTriggerKind])
