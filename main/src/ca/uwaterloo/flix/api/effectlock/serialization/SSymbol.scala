package ca.uwaterloo.flix.api.effectlock.serialization

sealed trait SSymbol

object SSymbol {

  case class VarSym(id: Int, text: SVT, kind: SKind) extends SSymbol

  case class TypeAliasSym(namespace: List[String], name: String) extends SSymbol

  case class AssocTypeSym(trt: SSymbol.TraitSym, name: String) extends SSymbol

  case class TraitSym(namespace: List[String], name: String) extends SSymbol

  case class EnumSym(namespace: List[String], text: String) extends SSymbol
}
