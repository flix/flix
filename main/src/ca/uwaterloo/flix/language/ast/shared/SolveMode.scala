package ca.uwaterloo.flix.language.ast.shared

sealed trait SolveMode // in ast.shared package
object SolveMode{
  case object Default extends SolveMode
  case object WithProvenance extends SolveMode
}
