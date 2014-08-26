package impl.verifier

/**
 * An SMT-LIB declaration.
 */
sealed trait SmtDecl {
  def fmt: String = ???
}

object SmtDecl {

  case class Assert(e: SmtExp) extends SmtDecl

  case class DeclareFun() extends SmtDecl

  case object CheckSat extends SmtDecl

  case object GetModel extends SmtDecl

}
