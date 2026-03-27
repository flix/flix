package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Symbol

sealed trait EffSymOrRigidVar {
  def name: String
}
object EffSymOrRigidVar {

  case class Eff(symbol: Symbol.EffSym) extends EffSymOrRigidVar {
    def name: String = symbol.name
  }
  case class RigidVar(symbol: Symbol.KindedTypeVarSym) extends EffSymOrRigidVar {
    def name: String = symbol.text match {
      case VarText.Absent => "???"
      case VarText.SourceText(s) => s
    }

  }
}
