package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.{Scope, SymUse, VarText}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.{Formats, ShortTypeHints}
import org.json4s.native.Serialization.{read, write}

object Serialization {

  private implicit val formats: Formats = org.json4s.native.Serialization.formats(ShortTypeHints(List(
    // Types
    classOf[SerializableType.Var],
    classOf[SerializableType.Cst],
    classOf[SerializableType.Apply],
    classOf[SerializableType.Alias],
    classOf[SerializableType.AssocType],

    // TypeConstructors
    SerializableTypeConstructor.Void.getClass,
    SerializableTypeConstructor.AnyType.getClass,
    SerializableTypeConstructor.Unit.getClass,
    SerializableTypeConstructor.Null.getClass,
    SerializableTypeConstructor.Bool.getClass,
    SerializableTypeConstructor.Char.getClass,
    SerializableTypeConstructor.Float32.getClass,
    SerializableTypeConstructor.Float64.getClass,
    SerializableTypeConstructor.BigDecimal.getClass,
    SerializableTypeConstructor.Int8.getClass,
    SerializableTypeConstructor.Int16.getClass,
    SerializableTypeConstructor.Int32.getClass,
    SerializableTypeConstructor.Int64.getClass,
    SerializableTypeConstructor.BigInt.getClass,
    SerializableTypeConstructor.Str.getClass,
    SerializableTypeConstructor.Regex.getClass,
    classOf[SerializableTypeConstructor.Arrow],
    classOf[SerializableTypeConstructor.ArrowWithoutEffect],
    SerializableTypeConstructor.RecordRowEmpty.getClass,
    // classOf[SerializableTypeConstructor.RecordRowExtend],
    SerializableTypeConstructor.Record.getClass,
    SerializableTypeConstructor.SchemaRowEmpty.getClass,
    // classOf[SerializableTypeConstructor.SchemaRowExtend],
    SerializableTypeConstructor.Schema.getClass,
    SerializableTypeConstructor.Sender.getClass,
    SerializableTypeConstructor.Receiver.getClass,
    SerializableTypeConstructor.Lazy.getClass,
    // classOf[SerializableTypeConstructor.Enum],
    // classOf[SerializableTypeConstructor.Struct],
    // classOf[SerializableTypeConstructor.RestrictableEnum],
    // classOf[SerializableTypeConstructor.Native],
    // classOf[SerializableTypeConstructor.JvmConstructor],
    // classOf[SerializableTypeConstructor.JvmMethod],
    // classOf[SerializableTypeConstructor.JvmField],
    SerializableTypeConstructor.Array.getClass,
    SerializableTypeConstructor.Vector.getClass,
    classOf[SerializableTypeConstructor.Tuple],
    SerializableTypeConstructor.Relation.getClass,
    SerializableTypeConstructor.Lattice.getClass,
    SerializableTypeConstructor.True.getClass,
    SerializableTypeConstructor.False.getClass,
    SerializableTypeConstructor.Not.getClass,
    SerializableTypeConstructor.And.getClass,
    SerializableTypeConstructor.Or.getClass,
    SerializableTypeConstructor.Pure.getClass,
    SerializableTypeConstructor.Univ.getClass,
    SerializableTypeConstructor.Complement.getClass,
    SerializableTypeConstructor.Union.getClass,
    SerializableTypeConstructor.Intersection.getClass,
    SerializableTypeConstructor.Difference.getClass,
    SerializableTypeConstructor.SymmetricDiff.getClass,
    // classOf[SerializableTypeConstructor.Effect],
    // classOf[SerializableTypeConstructor.CaseComplement],
    // classOf[SerializableTypeConstructor.CaseUnion],
    // classOf[SerializableTypeConstructor.CaseIntersection],
    // classOf[SerializableTypeConstructor.CaseSet],
    SerializableTypeConstructor.RegionToStar.getClass,

    // Kinds
    SerializableKind.Wild.getClass,
    SerializableKind.WildCaseSet.getClass,
    SerializableKind.Star.getClass,
    SerializableKind.Eff.getClass,
    SerializableKind.Bool.getClass,
    SerializableKind.RecordRow.getClass,
    SerializableKind.SchemaRow.getClass,
    SerializableKind.Predicate.getClass,
    SerializableKind.Jvm.getClass,
    // classOf[SerializableKind.CaseSet],
    classOf[SerializableKind.Arrow],

    // Symbols
    classOf[SerializableSymbol.VarSym],
    classOf[SerializableSymbol.TypeAliasSym],
    classOf[SerializableSymbol.AssocTypeSym],
    classOf[SerializableSymbol.TraitSym],
    classOf[SerializableSymbol.DefnSyn],

    // VarText
    SerializableVarText.Absent.getClass,
    classOf[SerializableVarText.SourceText],
  )))

  def serialize(tpe: Type): String = {
    write(fromType(tpe))
  }

  def deserialize(tpe: String): Option[Type] = {
    // Toggle error handling
    if (true) {
      val deser = read[SerializableType](tpe)
      Some(toType(deser))
    } else {
      try {
        val deser = read[SerializableType](tpe)
        Some(toType(deser))
      }
      catch {
        case _: Exception => None
      }
    }
  }

  // TODO: Scheme: only use base and list of typevar

  def fromType(tpe: Type): SerializableType = tpe match {
    case Type.Var(sym, _) =>
      val serSym = SerializableSymbol.VarSym(sym.id, fromVarText(sym.text), fromKind(sym.kind))
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

    case Type.JvmToType(_, _) | Type.JvmToEff(_, _) | Type.UnresolvedJvmType(_, _) => throw InternalCompilerException("unexpected jvm type", tpe.loc)

  }

  def fromTypeConstructor(tc0: TypeConstructor): SerializableTypeConstructor = tc0 match {
    case TypeConstructor.Void => SerializableTypeConstructor.Void
    case TypeConstructor.AnyType => SerializableTypeConstructor.AnyType
    case TypeConstructor.Unit => SerializableTypeConstructor.Unit
    case TypeConstructor.Null => SerializableTypeConstructor.Null
    case TypeConstructor.Bool => SerializableTypeConstructor.Bool
    case TypeConstructor.Char => SerializableTypeConstructor.Char
    case TypeConstructor.Float32 => SerializableTypeConstructor.Float32
    case TypeConstructor.Float64 => SerializableTypeConstructor.Float64
    case TypeConstructor.BigDecimal => SerializableTypeConstructor.BigDecimal
    case TypeConstructor.Int8 => SerializableTypeConstructor.Int8
    case TypeConstructor.Int16 => SerializableTypeConstructor.Int16
    case TypeConstructor.Int32 => SerializableTypeConstructor.Int32
    case TypeConstructor.Int64 => SerializableTypeConstructor.Int64
    case TypeConstructor.BigInt => SerializableTypeConstructor.BigInt
    case TypeConstructor.Str => SerializableTypeConstructor.Str
    case TypeConstructor.Regex => SerializableTypeConstructor.Regex
    case TypeConstructor.Arrow(arity) => SerializableTypeConstructor.Arrow(arity)
    case TypeConstructor.ArrowWithoutEffect(arity) => SerializableTypeConstructor.ArrowWithoutEffect(arity)
    case TypeConstructor.RecordRowEmpty => SerializableTypeConstructor.RecordRowEmpty
    case TypeConstructor.RecordRowExtend(label) => ??? // SerializableTypeConstructor.RecordRowExtend(label)
    case TypeConstructor.Record => SerializableTypeConstructor.Record
    case TypeConstructor.SchemaRowEmpty => SerializableTypeConstructor.SchemaRowEmpty
    case TypeConstructor.SchemaRowExtend(pred) => ??? // SerializableTypeConstructor.SchemaRowExtend(pred)
    case TypeConstructor.Schema => SerializableTypeConstructor.Schema
    case TypeConstructor.Sender => SerializableTypeConstructor.Sender
    case TypeConstructor.Receiver => SerializableTypeConstructor.Receiver
    case TypeConstructor.Lazy => SerializableTypeConstructor.Lazy
    case TypeConstructor.Enum(sym, kind) => ??? // SerializableTypeConstructor.Enum(sym, kind)
    case TypeConstructor.Struct(sym, kind) => ??? // SerializableTypeConstructor.Struct(sym, kind)
    case TypeConstructor.RestrictableEnum(sym, kind) => ??? // SerializableTypeConstructor.RestrictableEnum(sym, kind)
    case TypeConstructor.Native(clazz) => ??? // SerializableTypeConstructor.Native(clazz)
    case TypeConstructor.JvmConstructor(constructor) => ??? // SerializableTypeConstructor.JvmConstructor(constructor)
    case TypeConstructor.JvmMethod(method) => ??? // SerializableTypeConstructor.JvmMethod(method)
    case TypeConstructor.JvmField(field) => ??? // SerializableTypeConstructor.JvmField(field)
    case TypeConstructor.Array => SerializableTypeConstructor.Array
    case TypeConstructor.ArrayWithoutRegion => ??? // SerializableTypeConstructor.ArrayWithoutRegion
    case TypeConstructor.Vector => SerializableTypeConstructor.Vector
    case TypeConstructor.Tuple(l) => SerializableTypeConstructor.Tuple(l)
    case TypeConstructor.Relation => SerializableTypeConstructor.Relation
    case TypeConstructor.Lattice => SerializableTypeConstructor.Lattice
    case TypeConstructor.True => SerializableTypeConstructor.True
    case TypeConstructor.False => SerializableTypeConstructor.False
    case TypeConstructor.Not => SerializableTypeConstructor.Not
    case TypeConstructor.And => SerializableTypeConstructor.And
    case TypeConstructor.Or => SerializableTypeConstructor.Or
    case TypeConstructor.Pure => SerializableTypeConstructor.Pure
    case TypeConstructor.Univ => SerializableTypeConstructor.Univ
    case TypeConstructor.Complement => SerializableTypeConstructor.Complement
    case TypeConstructor.Union => SerializableTypeConstructor.Union
    case TypeConstructor.Intersection => SerializableTypeConstructor.Intersection
    case TypeConstructor.Difference => SerializableTypeConstructor.Difference
    case TypeConstructor.SymmetricDiff => SerializableTypeConstructor.SymmetricDiff
    case TypeConstructor.Effect(sym) => ??? // SerializableTypeConstructor.Effect(sym)
    case TypeConstructor.CaseComplement(sym) => ??? // SerializableTypeConstructor.CaseComplement(sym)
    case TypeConstructor.CaseUnion(sym) => ??? // SerializableTypeConstructor.CaseUnion(sym)
    case TypeConstructor.CaseIntersection(sym) => ??? // SerializableTypeConstructor.CaseIntersection(sym)
    case TypeConstructor.CaseSet(syms, enumSym) => ??? // SerializableTypeConstructor.CaseSet(syms, enumSym)
    case TypeConstructor.RegionToStar => SerializableTypeConstructor.RegionToStar
    case TypeConstructor.RegionWithoutRegion => ??? // SerializableTypeConstructor.RegionWithoutRegion
    case TypeConstructor.Error(id, kind) => ??? // SerializableTypeConstructor.Error(id, kind)
  }

  private def fromVarText(text: VarText): SerializableVarText = text match {
    case VarText.Absent => SerializableVarText.Absent
    case VarText.SourceText(s) => SerializableVarText.SourceText(s)
  }

  private def toVarText(text: SerializableVarText): VarText = text match {
    case SerializableVarText.Absent => VarText.Absent
    case SerializableVarText.SourceText(s) => VarText.SourceText(s)
  }

  private def fromKind(kind0: Kind): SerializableKind = kind0 match {
    case Kind.Wild => SerializableKind.Wild
    case Kind.WildCaseSet => SerializableKind.WildCaseSet
    case Kind.Star => SerializableKind.Star
    case Kind.Eff => SerializableKind.Eff
    case Kind.Bool => SerializableKind.Bool
    case Kind.RecordRow => SerializableKind.RecordRow
    case Kind.SchemaRow => SerializableKind.SchemaRow
    case Kind.Predicate => SerializableKind.Predicate
    case Kind.Jvm => SerializableKind.Jvm
    case Kind.CaseSet(sym) => ???
    case Kind.Arrow(k1, k2) => SerializableKind.Arrow(fromKind(k1), fromKind(k2))
    case Kind.Error => ???
  }

  private def toKind(kind0: SerializableKind): Kind = kind0 match {
    case SerializableKind.Wild => Kind.Wild
    case SerializableKind.WildCaseSet => Kind.WildCaseSet
    case SerializableKind.Star => Kind.Star
    case SerializableKind.Eff => Kind.Eff
    case SerializableKind.Bool => Kind.Bool
    case SerializableKind.RecordRow => Kind.RecordRow
    case SerializableKind.SchemaRow => Kind.SchemaRow
    case SerializableKind.Predicate => Kind.Predicate
    case SerializableKind.Jvm => Kind.Jvm
    case SerializableKind.Arrow(k1, k2) => Kind.Arrow(toKind(k1), toKind(k2))
  }

  def toType(tpe: SerializableType): Type = tpe match {
    case SerializableType.Var(sym) =>
      val sym1 = new Symbol.KindedTypeVarSym(sym.id, toVarText(sym.text), toKind(sym.kind), isRegion = false, isSlack = false, scope = Scope.Top, loc = SourceLocation.Unknown)
      Type.Var(sym1, SourceLocation.Unknown)
    case SerializableType.Cst(tc) => Type.Cst(toTypeConstructor(tc), SourceLocation.Unknown)
    case SerializableType.Apply(tpe1, tpe2) => ???
    case SerializableType.Alias(symUse, args, tpe) => ???
    case SerializableType.AssocType(symUse, arg, kind) => ???
  }

  def toTypeConstructor(tc0: SerializableTypeConstructor): TypeConstructor = tc0 match {
    case SerializableTypeConstructor.Void => ???
    case SerializableTypeConstructor.AnyType => ???
    case SerializableTypeConstructor.Unit => TypeConstructor.Unit
    case SerializableTypeConstructor.Null => ???
    case SerializableTypeConstructor.Bool => ???
    case SerializableTypeConstructor.Char => ???
    case SerializableTypeConstructor.Float32 => ???
    case SerializableTypeConstructor.Float64 => ???
    case SerializableTypeConstructor.BigDecimal => ???
    case SerializableTypeConstructor.Int8 => ???
    case SerializableTypeConstructor.Int16 => ???
    case SerializableTypeConstructor.Int32 => ???
    case SerializableTypeConstructor.Int64 => ???
    case SerializableTypeConstructor.BigInt => ???
    case SerializableTypeConstructor.Str => ???
    case SerializableTypeConstructor.Regex => ???
    case SerializableTypeConstructor.Arrow(arity) => ???
    case SerializableTypeConstructor.ArrowWithoutEffect(arity) => ???
    case SerializableTypeConstructor.RecordRowEmpty => ???
    case SerializableTypeConstructor.Record => ???
    case SerializableTypeConstructor.SchemaRowEmpty => ???
    case SerializableTypeConstructor.Schema => ???
    case SerializableTypeConstructor.Sender => ???
    case SerializableTypeConstructor.Receiver => ???
    case SerializableTypeConstructor.Lazy => ???
    case SerializableTypeConstructor.Array => ???
    case SerializableTypeConstructor.Vector => ???
    case SerializableTypeConstructor.Tuple(l) => ???
    case SerializableTypeConstructor.Relation => ???
    case SerializableTypeConstructor.Lattice => ???
    case SerializableTypeConstructor.True => ???
    case SerializableTypeConstructor.False => ???
    case SerializableTypeConstructor.Not => ???
    case SerializableTypeConstructor.And => ???
    case SerializableTypeConstructor.Or => ???
    case SerializableTypeConstructor.Pure => ???
    case SerializableTypeConstructor.Univ => ???
    case SerializableTypeConstructor.Complement => ???
    case SerializableTypeConstructor.Union => ???
    case SerializableTypeConstructor.Intersection => ???
    case SerializableTypeConstructor.Difference => ???
    case SerializableTypeConstructor.SymmetricDiff => ???
    case SerializableTypeConstructor.RegionToStar => ???
  }

  case class SerializableLibrary(name: String, defs: List[SerializableFunction]) // TODO: Maybe not String for name field

  case class SerializableFunction(name: SerializableSymbol.DefnSyn, tpe: SerializableType) // TODO: Use Spec instead of type

  sealed trait SerializableType

  object SerializableType {
    case class Var(sym: SerializableSymbol.VarSym) extends SerializableType

    case class Cst(tc: SerializableTypeConstructor) extends SerializableType

    case class Apply(tpe1: SerializableType, tpe2: SerializableType) extends SerializableType

    case class Alias(symUse: SerializableSymbol.TypeAliasSym, args: List[SerializableType], tpe: SerializableType) extends SerializableType

    case class AssocType(symUse: SerializableSymbol.AssocTypeSym, arg: SerializableType, kind: SerializableKind) extends SerializableType

  }

  sealed trait SerializableTypeConstructor

  object SerializableTypeConstructor {
    case object Void extends SerializableTypeConstructor

    case object AnyType extends SerializableTypeConstructor

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

    case object Sender extends SerializableTypeConstructor

    case object Receiver extends SerializableTypeConstructor

    case object Lazy extends SerializableTypeConstructor

    //case class Enum(sym: Symbol.EnumSym, kind: SerializableKind) extends SerializableTypeConstructor
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
  }

  sealed trait SerializableKind // only have star, eff, arrow

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

    // case class CaseSet(sym: SerializableSymbol.RestrictableEnumSym) extends SerializableKind

    case class Arrow(k1: SerializableKind, k2: SerializableKind) extends SerializableKind

  }


  sealed trait SerializableName

  object SerializableName {
    case class Ident(name: String) extends SerializableName // jvm types do not exist after typing
  }

  sealed trait SerializableSymbol

  object SerializableSymbol {

    case class VarSym(id: Int, text: SerializableVarText, kind: SerializableKind) extends SerializableSymbol

    case class TypeAliasSym(namespace: List[String], name: String) extends SerializableSymbol

    case class AssocTypeSym(trt: SerializableSymbol.TraitSym, name: String) extends SerializableSymbol

    case class TraitSym(namespace: List[String], name: String) extends SerializableSymbol

    case class DefnSyn(id: Option[Int], namespace: List[String], text: String) extends SerializableSymbol
  }

  sealed trait SerializableVarText

  object SerializableVarText {

    case object Absent extends SerializableVarText

    case class SourceText(s: String) extends SerializableVarText

  }

}
