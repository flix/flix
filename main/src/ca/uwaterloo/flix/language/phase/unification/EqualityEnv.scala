package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.shared.AssocTypeDef
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypeHead}

/**
  * Maintains information about associated type definitions.
  */
object EqualityEnv {
  val empty: EqualityEnv = EqualityEnv(Map.empty)
}

case class EqualityEnv(private val m: Map[(Symbol.AssocTypeSym, TypeHead), AssocTypeDef]) {

  /**
    * Returns the value of the associated type, if it is defined.
    */
  def getAssocDef(sym: Symbol.AssocTypeSym, tpe: Type): Option[AssocTypeDef] = {
    for {
      head <- TypeHead.fromType(tpe)
      assoc <- m.get((sym, head))
    } yield assoc
  }

  def addAssocTypeDef(sym: Symbol.AssocTypeSym, arg: Type, ret: Type): EqualityEnv = {
    TypeHead.fromType(arg) match {
      // Resiliency: Ignore this instance if it's not well-formed
      case None => this

      case Some(head) =>
        // tparams are Nil because we are adding instances directly, but not schemas of instances
        val tparams = Nil
        val defn = AssocTypeDef(tparams, arg, ret)

        EqualityEnv(m + (head -> defn))
    }
  }
}
