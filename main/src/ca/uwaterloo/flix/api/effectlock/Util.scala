package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{Kind, Scheme, Symbol, Type}

import scala.collection.mutable

object Util {

  /**
    * Performs alpha-renaming on `sc0`.
    *
    * To account for change in signatures, symbols with different kinds are renamed differently.
    */
  def alpha(sc0: Scheme): Scheme = {
    val seen = mutable.Map.empty[Kind, mutable.Map[Symbol.KindedTypeVarSym, Symbol.KindedTypeVarSym]]

    def visit(tpe0: Type): Type = tpe0 match {
      case Type.Var(sym, loc) =>
        if (!seen.contains(sym.kind)) {
          seen.put(sym.kind, mutable.Map.empty[Symbol.KindedTypeVarSym, Symbol.KindedTypeVarSym])
        }
        val innerMap = seen(sym.kind)
        innerMap.get(sym) match {
          case Some(subst) => Type.Var(subst, loc)
          case None =>
            val subst = new Symbol.KindedTypeVarSym(innerMap.size, sym.text, sym.kind, sym.isSlack, sym.scope, sym.loc)
            innerMap += sym -> subst
            Type.Var(subst, loc)
        }

      case Type.Cst(_, _) =>
        tpe0

      case Type.Apply(tpe1, tpe2, loc) =>
        Type.Apply(visit(tpe1), visit(tpe2), loc)

      case Type.Alias(symUse, args, tpe, loc) =>
        Type.Alias(symUse, args.map(visit), visit(tpe), loc)

      case Type.AssocType(symUse, arg, kind, loc) =>
        Type.AssocType(symUse, visit(arg), kind, loc)

      case Type.JvmToType(tpe, loc) =>
        Type.JvmToType(visit(tpe), loc)

      case Type.JvmToEff(tpe, loc) =>
        Type.JvmToEff(visit(tpe), loc)

      case Type.UnresolvedJvmType(_, _) =>
        tpe0
    }

    val base = visit(sc0.base)
    val tconstrs = sc0.tconstrs.map(tc => tc.copy(arg = visit(tc.arg)))
    val econstrs = sc0.econstrs.map(ec => ec.copy(tpe1 = visit(ec.tpe1), tpe2 = visit(ec.tpe2)))
    val qs = sc0.quantifiers.map {
      q =>
        seen.get(q.kind) match {
          case Some(inner) => inner.getOrElse(q, q)
          case None => q
        }
    }
    Scheme(qs, tconstrs, econstrs, base)
  }

}
