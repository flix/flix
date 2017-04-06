/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime.verifier

import com.microsoft.z3.{BoolExpr, Context, Solver, Status}

object SmtSolver {

  /**
    * The solver timeout in miliseconds.
    */
  val Timeout = 1000

  /**
    * A reference to the shared, non-thread safe Z3 context.
    *
    * Initialized by the first call to `withContext`.
    */
  var ctx: Context = null

  /**
    * Checks the satisfiability of the given boolean formula `f`.
    */
  def checkSat(f: BoolExpr, ctx: Context): SmtResult = {
    // Use try-finally block to manage resources.
    var solver: Solver = null
    try {
      val p = ctx.mkParams()
      p.add("timeout", Timeout)
      solver = ctx.mkSolver()
      solver.setParameters(p)
      solver.add(f)
      solver.check() match {
        case Status.SATISFIABLE => SmtResult.Satisfiable(solver.getModel)
        case Status.UNSATISFIABLE => SmtResult.Unsatisfiable
        case Status.UNKNOWN => SmtResult.Unknown
      }
    } finally {
      // Release resources.
      if (solver != null) solver.dispose()
    }
  }

  /**
    * Applies the given function `f` to an SMT context that is automatically closed.
    */
  def withContext[A](f: Context => A): A = {
    // Ensure that the context has been initialized.
    if (ctx == null) {
      // Check that the path property is set.
      val prop = System.getProperty("java.library.path")
      if (prop == null) {
        Console.println(errorMessage)
        Console.println()
        Console.println("> java.library.path not set.")
        System.exit(1)
      }

      // Try to load the Z3 library.
      loadLibrary()

      // Initialize the context.
      ctx = new Context()
    }

    f(ctx)
  }

  /**
    * Attempts to load the Z3 native library.
    */
  private def loadLibrary(): Unit = try {
    if (isWindows)
      System.loadLibrary("libz3")
    else
      System.loadLibrary("z3")
  } catch {
    case e: UnsatisfiedLinkError =>
      Console.println(errorMessage)
      Console.println()
      Console.println("> Unable to load the library. Stack Trace reproduced below: ")
      throw e
  }


  /**
    * Returns an error message explaining how to configure Microsoft Z3.
    */
  private def errorMessage: String =
    """###############################################################################
      |###                                                                         ###
      |### You are running Flix with verification enabled (--verify).              ###
      |### Flix uses the Microsoft Z3 SMT solver to verify correctness.            ###
      |### For this to work, you must have the correct Z3 libraries installed.     ###
      |###                                                                         ###
      |### On Windows:                                                             ###
      |###   1. Unpack the z3 bundle.                                              ###
      |###   2. Ensure that java.library.path points to that path, i.e. run        ###
      |###      java -Djava.library.path=... -jar flix.jar                         ###
      |###   3. Ensure that you have the                                           ###
      |###      'Microsoft Visual Studio Redistributable 2012 Package' installed.  ###
      |###                                                                         ###
      |###  NB: You must have the 64 bit version of Java, Z3 and the VS package.   ###
      |###                                                                         ###
      |### On Linux:                                                               ###
      |###    1. Ensure that java.library.path is set correctly.                   ###
      |###    2. Ensure that LD_LIBRARY_PATH is set correctly.                     ###
      |###                                                                         ###
      |###  On Mac OS X:                                                           ###
      |###    1. Ensure that java.library.path is set correctly.                   ###
      |###    2. Ensure that DYLD_LIBRARY_PATH is set correctly.                   ###
      |###                                                                         ###
      |###############################################################################
    """.stripMargin

  /**
    * Returns `true` if running on the Windows platform.
    */
  def isWindows: Boolean = System.getProperty("os.name").toLowerCase.contains("windows")
}
