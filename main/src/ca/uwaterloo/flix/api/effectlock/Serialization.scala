package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.shared.VarText

object Serialization {

  // TODO: Consider making Serializable super / marker trait

  def fromType(tpe: Type): SerializableType = ???

  def toType(serializableType: SerializableType): Type = ???


  case class SerializableLibrary(name: String, defs: List[SerializableFunction]) // TODO: Maybe not String for name field

  case class SerializableFunction(name: String, tpe: SerializableType) // TODO: Maybe not String for name field

  sealed trait SerializableType

  object SerializableType {
    case class Var(sym: SerializableSymbol.KindedTypeVarSym) extends SerializableType

    case class Cst(tc: SerializableTypeConstructor) extends SerializableType

    case class Apply(tpe1: SerializableType, tpe2: SerializableType) extends SerializableType

    case class Alias(symUse: SerializableSymbol.TypeAliasSym, args: List[SerializableType], tpe: SerializableType) extends SerializableType

    case class AssocType(symUse: SerializableSymbol.AssocTypeSym, arg: SerializableType, kind: SerializableKind) extends SerializableType

    case class JvmToType(tpe: SerializableType) extends SerializableType

    case class JvmToEff(tpe: SerializableType) extends SerializableType

    case class UnresolvedJvmType(member: JvmMember) extends SerializableType

    sealed trait JvmMember

    object JvmMember {
      case class JvmConstructor(clazz: String, tpes: List[SerializableType]) extends JvmMember

      case class JvmField(tpe: SerializableType, name: SerializableName.Ident) extends JvmMember

      case class JvmMethod(tpe: SerializableType, name: SerializableName.Ident, tpes: List[SerializableType]) extends JvmMember

      case class JvmStaticMethod(clazz: String, name: SerializableName.Ident, tpes: List[SerializableType]) extends JvmMember
    }

  }

  sealed trait SerializableTypeConstructor

  object SerializableTypeConstructor {
    case object Void extends SerializableTypeConstructor

    case object Unit extends SerializableTypeConstructor

    case object Null extends SerializableTypeConstructor

    case object Bool extends SerializableTypeConstructor

    case object Char extends SerializableTypeConstructor

    case object Float32 extends SerializableTypeConstructor

    case object Float64 extends SerializableTypeConstructor

    case object BigDecimal extends SerializableTypeConstructor

    case object Int8 extends SerializableTypeConstructor

    case object Int16 extends SerializableTypeConstructor

    case object Int32 extends SerializableTypeConstructor

    case object Int64 extends SerializableTypeConstructor

    case object BigInt extends SerializableTypeConstructor

    case object Str extends SerializableTypeConstructor

    case object Regex extends SerializableTypeConstructor

    case class Arrow(arity: Int) extends SerializableTypeConstructor

    case class ArrowWithoutEffect(arity: Int) extends SerializableTypeConstructor

    case object RecordRowEmpty extends SerializableTypeConstructor

    // case class RecordRowExtend(label: SerializableName.Label) extends SerializableTypeConstructor

    case object Record extends SerializableTypeConstructor

    case object SchemaRowEmpty extends SerializableTypeConstructor

    // case class SchemaRowExtend(pred: SerializableName.Pred) extends SerializableTypeConstructor

    case object Schema extends SerializableTypeConstructor

    case object Lazy extends SerializableTypeConstructor

    // case class Enum(sym: Symbol.EnumSym, kind: SerializableKind) extends SerializableTypeConstructor
    // case class Struct(sym: Symbol.StructSym, kind: SerializableKind) extends SerializableTypeConstructor
    // case class RestrictableEnum(sym: Symbol.RestrictableEnumSym, kind: SerializableKind) extends SerializableTypeConstructor
    // case class Native(clazz: Class[?]) extends SerializableTypeConstructor
    // case class JvmConstructor(constructor: Constructor[?]) extends SerializableTypeConstructor
    // case class JvmMethod(method: Method) extends SerializableTypeConstructor
    // case class JvmField(field: Field) extends SerializableTypeConstructor

    case object Array extends SerializableTypeConstructor

    case object Vector extends SerializableTypeConstructor

    case class Tuple(l: Int) extends SerializableTypeConstructor

    case object Relation extends SerializableTypeConstructor

    case object Lattice extends SerializableTypeConstructor

    case object True extends SerializableTypeConstructor

    case object False extends SerializableTypeConstructor

    case object Not extends SerializableTypeConstructor

    case object And extends SerializableTypeConstructor

    case object Or extends SerializableTypeConstructor

    case object Pure extends SerializableTypeConstructor

    case object Univ extends SerializableTypeConstructor

    case object Complement extends SerializableTypeConstructor

    case object Union extends SerializableTypeConstructor

    case object Intersection extends SerializableTypeConstructor

    case object Difference extends SerializableTypeConstructor

    case object SymmetricDiff extends SerializableTypeConstructor

    // case class Effect(sym: Symbol.EffectSym) extends SerializableTypeConstructor
    // case class CaseComplement(sym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor
    // case class CaseUnion(sym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor
    // case class CaseIntersection(sym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor
    // case class CaseSet(syms: SortedSet[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym) extends SerializableTypeConstructor

    case object RegionToStar extends SerializableTypeConstructor

    case class Error(id: Int, kind: SerializableKind) extends SerializableTypeConstructor

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
    case class Ident(name: String) extends SerializableName
  }

  sealed trait SerializableSymbol

  object SerializableSymbol {

    case class KindedTypeVarSym(id: Int, text: VarText, kind: SerializableKind, isRegion: Boolean, isSlack: Boolean, scope: Scope) extends SerializableSymbol

    case class RestrictableEnumSym(namespace: List[String], name: String, cases: List[SerializableName.Ident]) extends SerializableSymbol


    case class TypeAliasSym(namespace: List[String], name: String) extends SerializableSymbol

    case class AssocTypeSym(trt: SerializableSymbol.TraitSym, name: String) extends SerializableSymbol

    case class TraitSym(namespace: List[String], name: String) extends SerializableSymbol

    case class Scope(syms: List[SerializableSymbol.KindedTypeVarSym])
  }
}
