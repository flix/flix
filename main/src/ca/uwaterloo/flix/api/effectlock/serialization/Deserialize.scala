package ca.uwaterloo.flix.api.effectlock.serialization

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.{Scope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Name, Scheme, SourceLocation, Symbol}

object Deserialize {

  def deserializeDef(defn0: SDef)(implicit flix: Flix): (Symbol.DefnSym, Scheme) = defn0 match {
    case SDef(namespace, text, scheme, source) => // source unused?
      val sym = Symbol.mkDefnSym(namespace.mkString("", ".", s".$text"))
      val sc = deserializeScheme(scheme)
      sym -> sc
  }

  private def deserializeScheme(sc0: SScheme)(implicit flix: Flix): Scheme = sc0 match {
    case SScheme(quantifiers, tconstrs, econstrs, base) =>
      val qs = quantifiers.map(deserializeKindedTypeVarSym)
      Scheme(qs, ???, ???, ???)
  }

  private def deserializeKind(kind0: SKind): Kind = kind0 match {
    case WildKind => Kind.Wild
    case WildCaseSetKind => Kind.WildCaseSet
    case StarKind => Kind.Star
    case EffKind => Kind.Eff
    case BoolKind => Kind.Bool
    case RecordRowKind => Kind.RecordRow
    case SchemaRowKind => Kind.SchemaRow
    case PredicateKind => Kind.Predicate
    case JvmKind => Kind.Jvm
    case CaseSetKind(sym) => Kind.CaseSet(deserializeRestrictableEnumSym(sym))
    case ArrowKind(k1, k2) => Kind.Arrow(deserializeKind(k1), deserializeKind(k2))
  }

  private def deserializeKindedTypeVarSym(sym0: VarSym)(implicit flix: Flix): Symbol.KindedTypeVarSym = sym0 match {
    case VarSym(text, kind) =>
      val t = deserializeVarText(text)
      val k = deserializeKind(kind)
      Symbol.freshKindedTypeVarSym(t, k, isSlack = false, SourceLocation.Unknown)(Scope.Top, flix)
  }

  private def deserializeRestrictableEnumSym(sym0: RestrictableEnumSym): Symbol.RestrictableEnumSym = sym0 match {
    case RestrictableEnumSym(ns, name, cases) =>
      new Symbol.RestrictableEnumSym(ns, name, cases.map(deserializeIdent), SourceLocation.Unknown)
  }

  private def deserializeVarText(text0: SVarText): VarText = text0 match {
    case Absent => VarText.Absent
    case Text(s) => VarText.SourceText(s)
  }

  private def deserializeIdent(ident0: String): Name.Ident = {
    Name.Ident(ident0, SourceLocation.Unknown)
  }

}
