package ca.uwaterloo.flix.language

sealed trait CompilationMessageKind

object CompilationMessageKind {

  case object ParseError extends CompilationMessageKind

  case object WeederError extends CompilationMessageKind

  case object NameError extends CompilationMessageKind

  case object ResolutionError extends CompilationMessageKind

  case object KindError extends CompilationMessageKind

  case object DerivationError extends CompilationMessageKind

  case object TypeError extends CompilationMessageKind

  case object RegionError extends CompilationMessageKind

  case object EntryPointError extends CompilationMessageKind

  case object InstanceError extends CompilationMessageKind

  case object PredDepError extends CompilationMessageKind

  case object StratificationError extends CompilationMessageKind

  case object PatternMatchError extends CompilationMessageKind

  case object RedundancyError extends CompilationMessageKind

  case object SafetyError extends CompilationMessageKind

}
