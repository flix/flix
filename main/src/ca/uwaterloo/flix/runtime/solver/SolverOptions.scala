package ca.uwaterloo.flix.runtime.solver

import java.time.Duration

case class SolverOptions(monitor: Boolean, threads: Int, timeout: Option[Duration], verbose: Boolean)
