package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{Scheme, Type}

object EffectLock {

  def isSafe(sc1: Scheme, sc2: Scheme): Boolean = {
    isSafe(sc1.base, sc2.base)
  }

  private def isSafe(tpe1: Type, tpe2: Type): Boolean = (tpe1, tpe2) match {
    case (Type.Var(sym1, loc1), Type.Var(sym2, loc2)) => ???
    case (Type.Cst(tc1, loc1), Type.Cst(tc2, loc2)) => tc1 == tc2
    case (Type.Apply(tpe11, tpe12, loc1), Type.Apply(tpe21, tpe22, loc2)) => isSafe(tpe11, tpe21) && isSafe(tpe12, tpe22)
    case (Type.Alias(symUse1, args1, tpe1, loc1), Type.Alias(symUse2, args2, tpe2, loc2)) => ???
    case (Type.AssocType(symUse1, arg1, kind1, loc1), Type.AssocType(symUse2, arg2, kind2, loc2)) => ???
    case _ => false
  }

}
