package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Kind, Type}
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.util.InternalCompilerException


/** This type simplification is purely focused on user readability. */
object TypeSimplifier {

  /**
    * Simplifies types and effects intended for user readability.
    *
    * This function will never crash.
    */
  def simplify(tpe: Type): Type = {
    try {
      simplifyInternal(tpe)
    } catch {
      case _: InternalCompilerException => tpe
    }
  }

  /**
    * Simplifies all effects occuring in `tpe0`.
    *
    * Might throw [[InternalCompilerException]] for non-well-kinded effects.
    */
  private def simplifyInternal(tpe0: Type): Type = tpe0 match {
    case t if t.kind == Kind.Eff => EffUnification3.simplify(t)
    case t: Type.Var => t
    case t: Type.Cst => t
    case t@Type.Apply(tpe1, tpe2, loc) =>
      val t1 = simplifyInternal(tpe1)
      val t2 = simplifyInternal(tpe2)
      t.renew(t1, t2, loc)
    case Type.Alias(symUse, args, tpe, loc) =>
      val as = args.map(simplifyInternal)
      val t = simplifyInternal(tpe)
      Type.Alias(symUse, as, t, loc)
    case Type.AssocType(symUse, arg, kind, loc) =>
      val a = simplifyInternal(arg)
      Type.AssocType(symUse, a, kind, loc)
    case Type.JvmToType(tpe, loc) =>
      val t = simplifyInternal(tpe)
      Type.JvmToType(t, loc)
    case Type.JvmToEff(tpe, loc) =>
      val t = simplifyInternal(tpe)
      Type.JvmToEff(t, loc)
    case Type.UnresolvedJvmType(member, loc) =>
      val m = member.map(simplifyInternal)
      Type.UnresolvedJvmType(m, loc)
  }

}
