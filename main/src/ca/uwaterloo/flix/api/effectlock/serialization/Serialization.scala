package ca.uwaterloo.flix.api.effectlock.serialization

import ca.uwaterloo.flix.language.ast.shared.{Scope, SymUse, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.native.Serialization.{read, write}
import org.json4s.{Formats, ShortTypeHints}

object Serialization {

  type Library = String

  type NamedTypeScheme = (Symbol.DefnSym, Scheme)

  /**
    * Type hints for JSON library to (de)serialize ADTs.
    * If an error occurs, it is probably because the [[TypeHints.formats]]
    * list has not been updated.
    */
  private implicit val formats: Formats = TypeHints.formats

  def serialize(libraryDefs: Map[Library, List[TypedAst.Def]]): String = {
    write(fromLibs(libraryDefs))
  }

  def serialize(tpe: Type): String = {
    write(fromType(tpe))
  }

  // TODO: Use Map[Symbol.DefnSym, Scheme] instead of List[NamedTypeScheme]
  def deserialize(json: String): Option[Map[Library, List[NamedTypeScheme]]] = {
    // Toggle error handling
    if (true) {
      val deser = read[Map[Library, List[SDef]]](json)
      Some(toLibs(deser))
    } else {
      try {
        val deser = read[Map[Library, List[SDef]]](json)
        Some(toLibs(deser))
      }
      catch {
        case _: Exception => None
      }
    }
  }

  def deserializeTpe(tpe: String): Option[Type] = {
    // Toggle error handling
    if (true) {
      val deser = read[SType](tpe)
      Some(toType(deser))
    } else {
      try {
        val deser = read[SType](tpe)
        Some(toType(deser))
      }
      catch {
        case _: Exception => None
      }
    }
  }


  private def fromLibs(libs: Map[Library, List[TypedAst.Def]]): Map[Library, List[SDef]] = {
    libs.map {
      case (l, defs) => l -> defs.map(fromDef)
    }
  }

  private def toLibs(libs: Map[Library, List[SDef]]): Map[Library, List[NamedTypeScheme]] = {
    libs.map {
      case (l, defs) => l -> defs.map(toNamedTypeScheme)
    }
  }

  private def fromDef(defn0: TypedAst.Def): SDef = defn0 match {
    case TypedAst.Def(sym, spec, _, _) =>
      val ns = sym.namespace
      val text = sym.text
      val sc = fromScheme(spec.declaredScheme)
      SDef(ns, text, sc)
  }

  private def toNamedTypeScheme(defn0: SDef): NamedTypeScheme = defn0 match {
    case SDef(namespace, text, scheme) =>
      val sym = Symbol.mkDefnSym(namespace.mkString("", ".", ".").appendedAll(text))
      (sym, toScheme(scheme))
  }

  private def fromScheme(scheme: Scheme): SScheme = scheme match {
    case Scheme(quantifiers, _, _, base) =>
      val qs = quantifiers.map(fromKindedTypeVarSym)
      val b = fromType(base)
      SScheme(qs, b)
  }

  private def toScheme(scheme: SScheme): Scheme = scheme match {
    case SScheme(quantifiers, base) =>
      val qs = quantifiers.map(toKindedTypeVarSym)
      val b = toType(base)
      Scheme(qs, List.empty, List.empty, b)
  }

  private def fromKindedTypeVarSym(sym: Symbol.KindedTypeVarSym): SSymbol.VarSym = {
    SSymbol.VarSym(sym.id, fromVarText(sym.text), fromKind(sym.kind))
  }

  private def toKindedTypeVarSym(sym: SSymbol.VarSym): Symbol.KindedTypeVarSym = {
    new Symbol.KindedTypeVarSym(sym.id, toVarText(sym.text), toKind(sym.kind), isRegion = false, isSlack = false, scope = Scope.Top, loc = SourceLocation.Unknown)
  }

  private def fromType(tpe: Type): SType = tpe match {
    case Type.Var(sym, _) =>
      val serSym = SSymbol.VarSym(sym.id, fromVarText(sym.text), fromKind(sym.kind))
      SType.Var(serSym)

    case Type.Cst(tc, _) =>
      val serTC = fromTypeConstructor(tc)
      SType.Cst(serTC)

    case Type.Apply(tpe1, tpe2, _) =>
      val serT1 = fromType(tpe1)
      val serT2 = fromType(tpe2)
      SType.Apply(serT1, serT2)

    case Type.Alias(SymUse.TypeAliasSymUse(sym, _), args, tpe, _) =>
      val serSym = SSymbol.TypeAliasSym(sym.namespace, sym.name)
      val serTs = args.map(fromType)
      val serT = fromType(tpe)
      SType.Alias(serSym, serTs, serT)

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, _), arg, kind, _) =>
      val serTrtSym = SSymbol.TraitSym(sym.trt.namespace, sym.trt.name)
      val serSym = SSymbol.AssocTypeSym(serTrtSym, sym.name)
      val serT = fromType(arg)
      val serKind = fromKind(kind)
      SType.AssocType(serSym, serT, serKind)

    case Type.JvmToType(_, _) | Type.JvmToEff(_, _) | Type.UnresolvedJvmType(_, _) => throw InternalCompilerException("unexpected jvm type", tpe.loc)

  }

  private def fromTypeConstructor(tc0: TypeConstructor): STC = tc0 match {
    case TypeConstructor.Void => STC.Void
    case TypeConstructor.AnyType => STC.AnyType
    case TypeConstructor.Unit => STC.Unit
    case TypeConstructor.Null => STC.Null
    case TypeConstructor.Bool => STC.Bool
    case TypeConstructor.Char => STC.Char
    case TypeConstructor.Float32 => STC.Float32
    case TypeConstructor.Float64 => STC.Float64
    case TypeConstructor.BigDecimal => STC.BigDecimal
    case TypeConstructor.Int8 => STC.Int8
    case TypeConstructor.Int16 => STC.Int16
    case TypeConstructor.Int32 => STC.Int32
    case TypeConstructor.Int64 => STC.Int64
    case TypeConstructor.BigInt => STC.BigInt
    case TypeConstructor.Str => STC.Str
    case TypeConstructor.Regex => STC.Regex
    case TypeConstructor.Arrow(arity) => STC.Arrow(arity)
    case TypeConstructor.ArrowWithoutEffect(arity) => STC.ArrowWithoutEffect(arity)
    case TypeConstructor.RecordRowEmpty => STC.RecordRowEmpty
    case TypeConstructor.RecordRowExtend(label) => ??? // SerializableTypeConstructor.RecordRowExtend(label)
    case TypeConstructor.Record => STC.Record
    case TypeConstructor.SchemaRowEmpty => STC.SchemaRowEmpty
    case TypeConstructor.SchemaRowExtend(pred) => ??? // SerializableTypeConstructor.SchemaRowExtend(pred)
    case TypeConstructor.Schema => STC.Schema
    case TypeConstructor.Sender => STC.Sender
    case TypeConstructor.Receiver => STC.Receiver
    case TypeConstructor.Lazy => STC.Lazy
    case TypeConstructor.Enum(sym, kind) =>
      val serSym = SSymbol.EnumSym(sym.namespace, sym.text)
      val serKind = fromKind(kind)
      STC.Enum(serSym, serKind)
    case TypeConstructor.Struct(sym, kind) => ??? // SerializableTypeConstructor.Struct(sym, kind)
    case TypeConstructor.RestrictableEnum(sym, kind) => ??? // SerializableTypeConstructor.RestrictableEnum(sym, kind)
    case TypeConstructor.Native(clazz) => ??? // SerializableTypeConstructor.Native(clazz)
    case TypeConstructor.JvmConstructor(constructor) => ??? // SerializableTypeConstructor.JvmConstructor(constructor)
    case TypeConstructor.JvmMethod(method) => ??? // SerializableTypeConstructor.JvmMethod(method)
    case TypeConstructor.JvmField(field) => ??? // SerializableTypeConstructor.JvmField(field)
    case TypeConstructor.Array => STC.Array
    case TypeConstructor.ArrayWithoutRegion => ??? // SerializableTypeConstructor.ArrayWithoutRegion
    case TypeConstructor.Vector => STC.Vector
    case TypeConstructor.Tuple(l) => STC.Tuple(l)
    case TypeConstructor.Relation => STC.Relation
    case TypeConstructor.Lattice => STC.Lattice
    case TypeConstructor.True => STC.True
    case TypeConstructor.False => STC.False
    case TypeConstructor.Not => STC.Not
    case TypeConstructor.And => STC.And
    case TypeConstructor.Or => STC.Or
    case TypeConstructor.Pure => STC.Pure
    case TypeConstructor.Univ => STC.Univ
    case TypeConstructor.Complement => STC.Complement
    case TypeConstructor.Union => STC.Union
    case TypeConstructor.Intersection => STC.Intersection
    case TypeConstructor.Difference => STC.Difference
    case TypeConstructor.SymmetricDiff => STC.SymmetricDiff
    case TypeConstructor.Effect(sym) => ??? // SerializableTypeConstructor.Effect(sym)
    case TypeConstructor.CaseComplement(sym) => ??? // SerializableTypeConstructor.CaseComplement(sym)
    case TypeConstructor.CaseUnion(sym) => ??? // SerializableTypeConstructor.CaseUnion(sym)
    case TypeConstructor.CaseIntersection(sym) => ??? // SerializableTypeConstructor.CaseIntersection(sym)
    case TypeConstructor.CaseSet(syms, enumSym) => ??? // SerializableTypeConstructor.CaseSet(syms, enumSym)
    case TypeConstructor.RegionToStar => STC.RegionToStar
    case TypeConstructor.RegionWithoutRegion => ??? // SerializableTypeConstructor.RegionWithoutRegion
    case TypeConstructor.Error(id, kind) => ??? // SerializableTypeConstructor.Error(id, kind)
  }

  private def fromVarText(text: VarText): SVT = text match {
    case VarText.Absent => SVT.Absent
    case VarText.SourceText(s) => SVT.SourceText(s)
  }

  private def toVarText(text: SVT): VarText = text match {
    case SVT.Absent => VarText.Absent
    case SVT.SourceText(s) => VarText.SourceText(s)
  }

  private def fromKind(kind0: Kind): SKind = kind0 match {
    case Kind.Wild => SKind.Wild
    case Kind.WildCaseSet => SKind.WildCaseSet
    case Kind.Star => SKind.Star
    case Kind.Eff => SKind.Eff
    case Kind.Bool => SKind.Bool
    case Kind.RecordRow => SKind.RecordRow
    case Kind.SchemaRow => SKind.SchemaRow
    case Kind.Predicate => SKind.Predicate
    case Kind.Jvm => SKind.Jvm
    case Kind.CaseSet(sym) => ???
    case Kind.Arrow(k1, k2) => SKind.Arrow(fromKind(k1), fromKind(k2))
    case Kind.Error => ???
  }

  private def toKind(kind0: SKind): Kind = kind0 match {
    case SKind.Wild => Kind.Wild
    case SKind.WildCaseSet => Kind.WildCaseSet
    case SKind.Star => Kind.Star
    case SKind.Eff => Kind.Eff
    case SKind.Bool => Kind.Bool
    case SKind.RecordRow => Kind.RecordRow
    case SKind.SchemaRow => Kind.SchemaRow
    case SKind.Predicate => Kind.Predicate
    case SKind.Jvm => Kind.Jvm
    case SKind.Arrow(k1, k2) => Kind.Arrow(toKind(k1), toKind(k2))
  }

  private def toType(tpe: SType): Type = tpe match {
    case SType.Var(sym) =>
      Type.Var(toKindedTypeVarSym(sym), SourceLocation.Unknown)

    case SType.Cst(tc) =>
      Type.Cst(toTypeConstructor(tc), SourceLocation.Unknown)

    case SType.Apply(tpe1, tpe2) =>
      Type.Apply(toType(tpe1), toType(tpe2), SourceLocation.Unknown)

    case SType.Alias(symUse, args, tpe) =>
      val sym = new Symbol.TypeAliasSym(symUse.namespace, symUse.name, SourceLocation.Unknown)
      val su = SymUse.TypeAliasSymUse(sym, SourceLocation.Unknown)
      val as = args.map(toType)
      val t = toType(tpe)
      Type.Alias(su, as, t, SourceLocation.Unknown)

    case SType.AssocType(symUse, arg, kind) =>
      val tsym = new Symbol.TraitSym(symUse.trt.namespace, symUse.trt.name, SourceLocation.Unknown)
      val sym = new Symbol.AssocTypeSym(tsym, symUse.name, SourceLocation.Unknown)
      val su = SymUse.AssocTypeSymUse(sym, SourceLocation.Unknown)
      val a = toType(arg)
      val k = toKind(kind)
      Type.AssocType(su, a, k, SourceLocation.Unknown)
  }

  private def toTypeConstructor(tc0: STC): TypeConstructor = tc0 match {
    case STC.Void => TypeConstructor.Void
    case STC.AnyType => TypeConstructor.AnyType
    case STC.Unit => TypeConstructor.Unit
    case STC.Null => TypeConstructor.Null
    case STC.Bool => TypeConstructor.Bool
    case STC.Char => TypeConstructor.Char
    case STC.Float32 => TypeConstructor.Float32
    case STC.Float64 => TypeConstructor.Float64
    case STC.BigDecimal => TypeConstructor.BigDecimal
    case STC.Int8 => TypeConstructor.Int8
    case STC.Int16 => TypeConstructor.Int16
    case STC.Int32 => TypeConstructor.Int32
    case STC.Int64 => TypeConstructor.Int64
    case STC.BigInt => TypeConstructor.BigInt
    case STC.Str => TypeConstructor.Str
    case STC.Regex => TypeConstructor.Regex
    case STC.Arrow(arity) => TypeConstructor.Arrow(arity)
    case STC.ArrowWithoutEffect(arity) => TypeConstructor.ArrowWithoutEffect(arity)
    case STC.RecordRowEmpty => TypeConstructor.RecordRowEmpty
    case STC.Record => TypeConstructor.Record
    case STC.SchemaRowEmpty => TypeConstructor.SchemaRowEmpty
    case STC.Schema => TypeConstructor.Schema
    case STC.Sender => TypeConstructor.Sender
    case STC.Receiver => TypeConstructor.Receiver
    case STC.Lazy => TypeConstructor.Lazy
    case STC.Enum(sym, kind) =>
      val s = new Symbol.EnumSym(sym.namespace, sym.text, SourceLocation.Unknown)
      val k = toKind(kind)
      TypeConstructor.Enum(s, k)
    case STC.Array => TypeConstructor.Array
    case STC.Vector => TypeConstructor.Vector
    case STC.Tuple(l) => TypeConstructor.Tuple(l)
    case STC.Relation => TypeConstructor.Relation
    case STC.Lattice => TypeConstructor.Lattice
    case STC.True => TypeConstructor.True
    case STC.False => TypeConstructor.False
    case STC.Not => TypeConstructor.Not
    case STC.And => TypeConstructor.And
    case STC.Or => TypeConstructor.Or
    case STC.Pure => TypeConstructor.Pure
    case STC.Univ => TypeConstructor.Univ
    case STC.Complement => TypeConstructor.Complement
    case STC.Union => TypeConstructor.Union
    case STC.Intersection => TypeConstructor.Intersection
    case STC.Difference => TypeConstructor.Difference
    case STC.SymmetricDiff => TypeConstructor.SymmetricDiff
    case STC.RegionToStar => TypeConstructor.RegionToStar
  }

  private case class SDef(namespace: List[String], text: String, scheme: SScheme)

  private case class SScheme(quantifiers: List[SSymbol.VarSym], base: SType)

  private sealed trait SType

  private object SType {

    case class Var(sym: SSymbol.VarSym) extends SType

    case class Cst(tc: STC) extends SType

    case class Apply(tpe1: SType, tpe2: SType) extends SType

    case class Alias(symUse: SSymbol.TypeAliasSym, args: List[SType], tpe: SType) extends SType

    case class AssocType(symUse: SSymbol.AssocTypeSym, arg: SType, kind: SKind) extends SType

  }

  private sealed trait STC

  private object STC {

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


  private sealed trait SSymbol

  private object SSymbol {

    case class VarSym(id: Int, text: SVT, kind: SKind) extends SSymbol

    case class TypeAliasSym(namespace: List[String], name: String) extends SSymbol

    case class AssocTypeSym(trt: SSymbol.TraitSym, name: String) extends SSymbol

    case class TraitSym(namespace: List[String], name: String) extends SSymbol

    case class EnumSym(namespace: List[String], text: String) extends SSymbol
  }

  private sealed trait SVT

  private object SVT {

    case object Absent extends SVT

    case class SourceText(s: String) extends SVT

  }

  private object TypeHints {

    /**
      * List of type hints for serialization library.
      * Each constructor is registered here, so the library can serialize the name
      * of the constructor and its fields via reflection.
      */
    val formats: Formats = org.json4s.native.Serialization.formats(ShortTypeHints(List(
      // Types
      classOf[SType.Var],
      classOf[SType.Cst],
      classOf[SType.Apply],
      classOf[SType.Alias],
      classOf[SType.AssocType],

      // TypeConstructors
      STC.Void.getClass,
      STC.AnyType.getClass,
      STC.Unit.getClass,
      STC.Null.getClass,
      STC.Bool.getClass,
      STC.Char.getClass,
      STC.Float32.getClass,
      STC.Float64.getClass,
      STC.BigDecimal.getClass,
      STC.Int8.getClass,
      STC.Int16.getClass,
      STC.Int32.getClass,
      STC.Int64.getClass,
      STC.BigInt.getClass,
      STC.Str.getClass,
      STC.Regex.getClass,
      classOf[STC.Arrow],
      classOf[STC.ArrowWithoutEffect],
      STC.RecordRowEmpty.getClass,
      // classOf[SerializableTypeConstructor.RecordRowExtend],
      STC.Record.getClass,
      STC.SchemaRowEmpty.getClass,
      // classOf[SerializableTypeConstructor.SchemaRowExtend],
      STC.Schema.getClass,
      STC.Sender.getClass,
      STC.Receiver.getClass,
      STC.Lazy.getClass,
      classOf[STC.Enum],
      // classOf[SerializableTypeConstructor.Struct],
      // classOf[SerializableTypeConstructor.RestrictableEnum],
      // classOf[SerializableTypeConstructor.Native],
      // classOf[SerializableTypeConstructor.JvmConstructor],
      // classOf[SerializableTypeConstructor.JvmMethod],
      // classOf[SerializableTypeConstructor.JvmField],
      STC.Array.getClass,
      STC.Vector.getClass,
      classOf[STC.Tuple],
      STC.Relation.getClass,
      STC.Lattice.getClass,
      STC.True.getClass,
      STC.False.getClass,
      STC.Not.getClass,
      STC.And.getClass,
      STC.Or.getClass,
      STC.Pure.getClass,
      STC.Univ.getClass,
      STC.Complement.getClass,
      STC.Union.getClass,
      STC.Intersection.getClass,
      STC.Difference.getClass,
      STC.SymmetricDiff.getClass,
      // classOf[SerializableTypeConstructor.Effect],
      // classOf[SerializableTypeConstructor.CaseComplement],
      // classOf[SerializableTypeConstructor.CaseUnion],
      // classOf[SerializableTypeConstructor.CaseIntersection],
      // classOf[SerializableTypeConstructor.CaseSet],
      STC.RegionToStar.getClass,

      // Kinds
      SKind.Wild.getClass,
      SKind.WildCaseSet.getClass,
      SKind.Star.getClass,
      SKind.Eff.getClass,
      SKind.Bool.getClass,
      SKind.RecordRow.getClass,
      SKind.SchemaRow.getClass,
      SKind.Predicate.getClass,
      SKind.Jvm.getClass,
      // classOf[SerializableKind.CaseSet],
      classOf[SKind.Arrow],

      // Symbols
      classOf[SSymbol.VarSym],
      classOf[SSymbol.TypeAliasSym],
      classOf[SSymbol.AssocTypeSym],
      classOf[SSymbol.TraitSym],

      // VarText
      SVT.Absent.getClass,
      classOf[SVT.SourceText],
    )))
  }

}
