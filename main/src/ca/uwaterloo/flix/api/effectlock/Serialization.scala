package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.Type

object Serialization {

  def fromType(tpe: Type): SerializableType = ???

  def toType(serializableType: SerializableType): Type = ???


  case class SerializableLibrary(name: String, defs: List[SerializableFunction]) // TODO: Maybe not String for name field

  case class SerializableFunction(name: String, tpe: SerializableType) // TODO: Maybe not String for name field

  sealed trait SerializableType {
    def kind: SerializableKind
  }

  object SerializableType {
    sealed trait BaseType extends SerializableType
  }

  sealed trait SerializableKind

  object SerializableKind {
    case object Wild extends SerializableKind

    case object WildCaseSet extends SerializableKind

    case object Star extends SerializableKind

    case object Eff extends SerializableKind

    case object Bool extends SerializableKind

    case object RecordRow extends SerializableKind

    case object SchemaRow extends SerializableKind

    case object Predicate extends SerializableKind

    case object Jvm extends SerializableKind

    case class CaseSet(sym: SerializableSymbol.RestrictableEnumSym) extends SerializableKind

    case class Arrow(k1: SerializableKind, k2: SerializableKind) extends SerializableKind

    case object Error extends SerializableKind

  }


  sealed trait SerializableName

  object SerializableName {
    case class Ident(name: String)
  }

  sealed trait SerializableSymbol

  object SerializableSymbol {
    case class RestrictableEnumSym(namespace: List[String], name: String, cases: List[SerializableName.Ident])
  }
}
