package ca.uwaterloo.flix.language.ast
// MATT license
// MATT docs
sealed trait UnkindedType

object UnkindedType {
  case class Cst(cst: TypeConstructor, loc: SourceLocation)
  case class Apply(t1: UnkindedType, t2: UnkindedType)
  case class Lambda(t1: UnkindedType.Var, t2: UnkindedType)
  case class Var(id: Int, text: Option[String] = None)
}
