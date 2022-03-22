package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}

object FormatEff {

  def formatEff(eff: Type)(implicit audience: Audience): String = eff match {
    case Type.Cst(TypeConstructor.True, _) => "Pure"
    case Type.Cst(TypeConstructor.False, _) => "Impure"
    case _ => FormatType.formatWellKindedType(eff)
  }

}
