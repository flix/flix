package ca.uwaterloo.flix.api.lsp

import org.json4s.{JInt, JValue}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

sealed trait CodeActionTriggerKind

object CodeActionTriggerKind {
  def parse(json: JValue): Result[CodeActionTriggerKind, String] = {
    def err(v: JValue) =
      Err(s"Code action trigger kind not recognized as a valid integer: '$v'.")

    json match {
      case JInt(i) => i.toInt match {
        case 1 => Ok(Invoked)
        case 2 => Ok(Automatic)
        case _ => err(json)
      }
      case _ => err(json)
    }
  }

  case object Invoked extends CodeActionTriggerKind

  case object Automatic extends CodeActionTriggerKind
}
