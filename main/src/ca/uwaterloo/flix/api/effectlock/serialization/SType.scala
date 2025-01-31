package ca.uwaterloo.flix.api.effectlock.serialization

sealed trait SType

object SType {

  case class Var(sym: SSymbol.VarSym) extends SType

  case class Cst(tc: STC) extends SType

  case class Apply(tpe1: SType, tpe2: SType) extends SType

  case class Alias(symUse: SSymbol.TypeAliasSym, args: List[SType], tpe: SType) extends SType

  case class AssocType(symUse: SSymbol.AssocTypeSym, arg: SType, kind: SKind) extends SType

}
