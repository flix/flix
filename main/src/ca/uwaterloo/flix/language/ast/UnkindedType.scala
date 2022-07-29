package ca.uwaterloo.flix.language.ast

object UnkindedType {
  case class Var(sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation)
}

sealed trait UnkindedType
