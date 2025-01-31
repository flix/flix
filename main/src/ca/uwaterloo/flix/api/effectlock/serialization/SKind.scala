package ca.uwaterloo.flix.api.effectlock.serialization

sealed trait SKind // only have star, eff, arrow

object SKind {

  case object Wild extends SKind

  case object WildCaseSet extends SKind

  case object Star extends SKind

  case object Eff extends SKind

  case object Bool extends SKind

  case object RecordRow extends SKind

  case object SchemaRow extends SKind

  case object Predicate extends SKind

  case object Jvm extends SKind

  // case class CaseSet(sym: SerializableSymbol.RestrictableEnumSym) extends SerializableKind

  case class Arrow(k1: SKind, k2: SKind) extends SKind

}
