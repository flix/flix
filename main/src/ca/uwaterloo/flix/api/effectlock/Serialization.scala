package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.{Kind, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.{Scope, SymUse, VarText}

object Serialization {

  // TODO: Consider making Serializable super / marker trait

  def fromType(tpe: Type): SerializableType = tpe match {
    case Type.Var(sym, _) =>
      // TODO: Consider not using qualified case classes but just classes so we can use simple recursion
      // TODO: If there is a 1:1 mapping, then foregoing some type safety is ok
      val serSym = SerializableSymbol.KindedTypeVarSym(sym.id, sym.text, fromKind(sym.kind), sym.isRegion, sym.isSlack, fromScope(sym.scope))
      SerializableType.Var(serSym)

    case Type.Cst(tc, _) =>
      val serTC = fromTypeConstructor(tc)
      SerializableType.Cst(serTC)

    case Type.Apply(tpe1, tpe2, _) =>
      val serT1 = fromType(tpe1)
      val serT2 = fromType(tpe2)
      SerializableType.Apply(serT1, serT2)

    case Type.Alias(SymUse.TypeAliasSymUse(sym, _), args, tpe, _) =>
      val serSym = SerializableSymbol.TypeAliasSym(sym.namespace, sym.name)
      val serTs = args.map(fromType)
      val serT = fromType(tpe)
      SerializableType.Alias(serSym, serTs, serT)

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, _), arg, kind, _) =>
      val serTrtSym = SerializableSymbol.TraitSym(sym.trt.namespace, sym.trt.name)
      val serSym = SerializableSymbol.AssocTypeSym(serTrtSym, sym.name)
      val serT = fromType(arg)
      val serKind = fromKind(kind)

      SerializableType.AssocType(serSym, serT, serKind)
    case Type.JvmToType(tpe, _) =>
      val serT = fromType(tpe)
      SerializableType.JvmToType(serT)

    case Type.JvmToEff(tpe, _) =>
      val serT = fromType(tpe)
      SerializableType.JvmToEff(serT)

    case Type.UnresolvedJvmType(member, _) =>
      val serMem = fromMember(member)
      SerializableType.UnresolvedJvmType(serMem)

  }

  def fromTypeConstructor(tc0: TypeConstructor): SerializableTypeConstructor = tc0 match {
    case TypeConstructor.Void => ???
    case TypeConstructor.AnyType => ???
    case TypeConstructor.Unit => ???
    case TypeConstructor.Null => ???
    case TypeConstructor.Bool => ???
    case TypeConstructor.Char => ???
    case TypeConstructor.Float32 => ???
    case TypeConstructor.Float64 => ???
    case TypeConstructor.BigDecimal => ???
    case TypeConstructor.Int8 => ???
    case TypeConstructor.Int16 => ???
    case TypeConstructor.Int32 => ???
    case TypeConstructor.Int64 => ???
    case TypeConstructor.BigInt => ???
    case TypeConstructor.Str => ???
    case TypeConstructor.Regex => ???
    case TypeConstructor.Arrow(arity) => ???
    case TypeConstructor.ArrowWithoutEffect(arity) => ???
    case TypeConstructor.RecordRowEmpty => ???
    case TypeConstructor.RecordRowExtend(label) => ???
    case TypeConstructor.Record => ???
    case TypeConstructor.SchemaRowEmpty => ???
    case TypeConstructor.SchemaRowExtend(pred) => ???
    case TypeConstructor.Schema => ???
    case TypeConstructor.Sender => ???
    case TypeConstructor.Receiver => ???
    case TypeConstructor.Lazy => ???
    case TypeConstructor.Enum(sym, kind) => ???
    case TypeConstructor.Struct(sym, kind) => ???
    case TypeConstructor.RestrictableEnum(sym, kind) => ???
    case TypeConstructor.Native(clazz) => ???
    case TypeConstructor.JvmConstructor(constructor) => ???
    case TypeConstructor.JvmMethod(method) => ???
    case TypeConstructor.JvmField(field) => ???
    case TypeConstructor.Array => ???
    case TypeConstructor.ArrayWithoutRegion => ???
    case TypeConstructor.Vector => ???
    case TypeConstructor.Tuple(l) => ???
    case TypeConstructor.Relation => ???
    case TypeConstructor.Lattice => ???
    case TypeConstructor.True => ???
    case TypeConstructor.False => ???
    case TypeConstructor.Not => ???
    case TypeConstructor.And => ???
    case TypeConstructor.Or => ???
    case TypeConstructor.Pure => ???
    case TypeConstructor.Univ => ???
    case TypeConstructor.Complement => ???
    case TypeConstructor.Union => ???
    case TypeConstructor.Intersection => ???
    case TypeConstructor.Difference => ???
    case TypeConstructor.SymmetricDiff => ???
    case TypeConstructor.Effect(sym) => ???
    case TypeConstructor.CaseComplement(sym) => ???
    case TypeConstructor.CaseUnion(sym) => ???
    case TypeConstructor.CaseIntersection(sym) => ???
    case TypeConstructor.CaseSet(syms, enumSym) => ???
    case TypeConstructor.RegionToStar => ???
    case TypeConstructor.RegionWithoutRegion => ???
    case TypeConstructor.Error(id, kind) => ???
  }

  def fromMember(member0: Type.JvmMember): SerializableType.JvmMember = member0 match {
    case JvmMember.JvmConstructor(clazz, tpes) => ???
    case JvmMember.JvmField(base, tpe, name) => ???
    case JvmMember.JvmMethod(tpe, name, tpes) => ???
    case JvmMember.JvmStaticMethod(clazz, name, tpes) => ???
  }

  def fromSymbol(sym0: Symbol): SerializableSymbol = sym0 match {
    case sym: Symbol.VarSym => ???
    case sym: Symbol.KindedTypeVarSym =>
      SerializableSymbol.KindedTypeVarSym(sym.id, sym.text, fromKind(sym.kind), sym.isRegion, sym.isSlack, fromScope(sym.scope))
    case sym: Symbol.UnkindedTypeVarSym => ???
    case sym: Symbol.DefnSym => ???
    case sym: Symbol.EnumSym => ???
    case sym: Symbol.StructSym => ???
    case sym: Symbol.RestrictableEnumSym => ???
    case sym: Symbol.CaseSym => ???
    case sym: Symbol.StructFieldSym => ???
    case sym: Symbol.RestrictableCaseSym => ???
    case sym: Symbol.TraitSym => ???
    case sym: Symbol.SigSym => ???
    case sym: Symbol.LabelSym => ???
    case sym: Symbol.HoleSym => ???
    case sym: Symbol.TypeAliasSym => ???
    case sym: Symbol.AssocTypeSym => ???
    case sym: Symbol.EffectSym => ???
    case sym: Symbol.OpSym => ???
    case sym: Symbol.ModuleSym => ???
  }

  def fromKind(kind0: Kind): SerializableKind = kind0 match {
    case Kind.Wild => ???
    case Kind.WildCaseSet => ???
    case Kind.Star => ???
    case Kind.Eff => ???
    case Kind.Bool => ???
    case Kind.RecordRow => ???
    case Kind.SchemaRow => ???
    case Kind.Predicate => ???
    case Kind.Jvm => ???
    case Kind.CaseSet(sym) => ???
    case Kind.Arrow(k1, k2) => ???
    case Kind.Error => ???
  }

  def fromScope(scope0: Scope): SerializableScope = ???

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
      // TODO: Use class.getName()
      case class JvmConstructor(clazz: String, tpes: List[SerializableType]) extends JvmMember

      case class JvmField(tpe: SerializableType, name: SerializableName.Ident) extends JvmMember

      case class JvmMethod(tpe: SerializableType, name: SerializableName.Ident, tpes: List[SerializableType]) extends JvmMember

      // TODO: Use class.getName()
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

    case class KindedTypeVarSym(id: Int, text: VarText, kind: SerializableKind, isRegion: Boolean, isSlack: Boolean, scope: SerializableScope) extends SerializableSymbol

    case class RestrictableEnumSym(namespace: List[String], name: String, cases: List[SerializableName.Ident]) extends SerializableSymbol

    case class TypeAliasSym(namespace: List[String], name: String) extends SerializableSymbol

    case class AssocTypeSym(trt: SerializableSymbol.TraitSym, name: String) extends SerializableSymbol

    case class TraitSym(namespace: List[String], name: String) extends SerializableSymbol

  }

  case class SerializableScope(syms: List[SerializableSymbol.KindedTypeVarSym])

}
