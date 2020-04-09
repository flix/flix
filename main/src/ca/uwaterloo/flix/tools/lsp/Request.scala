package ca.uwaterloo.flix.tools.lsp

trait Request

object Request {

  case class Compile() extends Request

}

