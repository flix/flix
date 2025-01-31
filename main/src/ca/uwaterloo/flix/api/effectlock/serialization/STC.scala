package ca.uwaterloo.flix.api.effectlock.serialization

sealed trait STC

object STC {

  case object Void extends STC

  case object AnyType extends STC

  case object Unit extends STC

  case object Null extends STC

  case object Bool extends STC

  case object Char extends STC

  case object Float32 extends STC

  case object Float64 extends STC

  case object BigDecimal extends STC

  case object Int8 extends STC

  case object Int16 extends STC

  case object Int32 extends STC

  case object Int64 extends STC

  case object BigInt extends STC

  case object Str extends STC

  case object Regex extends STC

  case class Arrow(arity: Int) extends STC

  case class ArrowWithoutEffect(arity: Int) extends STC

  case object RecordRowEmpty extends STC

  // case class RecordRowExtend(label: SerializableName.Label) extends SerializableTypeConstructor

  case object Record extends STC

  case object SchemaRowEmpty extends STC

  // case class SchemaRowExtend(pred: SerializableName.Pred) extends SerializableTypeConstructor

  case object Schema extends STC

  case object Sender extends STC

  case object Receiver extends STC

  case object Lazy extends STC

  case class Enum(sym: SSymbol.EnumSym, kind: SKind) extends STC

  // case class Struct(sym: Symbol.StructSym, kind: SerializableKind) extends SerializableTypeConstructor
  // case class RestrictableEnum(sym: Symbol.RestrictableEnumSym, kind: SerializableKind) extends SerializableTypeConstructor
  // case class Native(clazz: Class[?]) extends SerializableTypeConstructor
  // case class JvmConstructor(constructor: Constructor[?]) extends SerializableTypeConstructor
  // case class JvmMethod(method: Method) extends SerializableTypeConstructor
  // case class JvmField(field: Field) extends SerializableTypeConstructor

  case object Array extends STC

  case object Vector extends STC

  case class Tuple(l: Int) extends STC

  case object Relation extends STC

  case object Lattice extends STC

  case object True extends STC

  case object False extends STC

  case object Not extends STC

  case object And extends STC

  case object Or extends STC

  case object Pure extends STC

  case object Univ extends STC

  case object Complement extends STC

  case object Union extends STC

  case object Intersection extends STC

  case object Difference extends STC

  case object SymmetricDiff extends STC

  // case class Effect(sym: Symbol.EffectSym) extends SerializableTypeConstructor
  // case class CaseComplement(sym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor
  // case class CaseUnion(sym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor
  // case class CaseIntersection(sym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor
  // case class CaseSet(syms: SortedSet[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor

  case object RegionToStar extends STC

}
