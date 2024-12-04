package ca.uwaterloo.flix.language

sealed trait CompilationMessageKind

object CompilationMessageKind {

  case object ParseError extends CompilationMessageKind {
    override def toString: String = "Parse Error"
  }

  case object WeederError extends CompilationMessageKind {
    override def toString: String = "Syntax Error"
  }

  case object NameError extends CompilationMessageKind {
    override def toString: String = "Name Error"
  }

  case object ResolutionError extends CompilationMessageKind {
    override def toString: String = "Resolution Error"
  }

  case object KindError extends CompilationMessageKind {
    override def toString: String = "Kind Error"
  }

  case object DerivationError extends CompilationMessageKind {
    override def toString: String = "Derivation Error"
  }

  case object TypeError extends CompilationMessageKind {
    override def toString: String = "Type Error"
  }

  case object EntryPointError extends CompilationMessageKind {
    override def toString: String = "Entry Point Error"
  }

  case object InstanceError extends CompilationMessageKind {
    override def toString: String = "Instance Error"
  }

  case object StratificationError extends CompilationMessageKind {
    override def toString: String = "Stratification Error"
  }

  case object PatternMatchError extends CompilationMessageKind {
    override def toString: String = "Pattern Match"
  }

  case object RedundancyError extends CompilationMessageKind {
    override def toString: String = "Redundancy Error"
  }

  case object SafetyError extends CompilationMessageKind {
    override def toString: String = "Safety Error"
  }

}
