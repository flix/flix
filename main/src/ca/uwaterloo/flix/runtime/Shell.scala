package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.util.{PrettyPrint, Version}

class Shell(solver: Solver) extends Thread {

  /**
   * A common super-type for input commands.
   */
  sealed trait Input

  object Input {

    /**
     * Does literally nothing.
     */
    case object Nop extends Input

    /**
     * Prints information about the available commands.
     */
    case object Help extends Input

    /**
     * Prints some basic status information about the fixpoint computation.
     */
    case object Status extends Input

    /**
      * Prints the given constant, relation or lattice.
      */
    case class Print(name: String) extends Input

    /**
     * Pauses the fixpoint computation.
     */
    case object Pause extends Input

    /**
     * Resumes the fixpoint computation.
     */
    case object Unpause extends Input

    /**
     * Gracefully terminates Flix.
     */
    case object Exit extends Input

    /**
     * Immediately and brutally terminates Flix.
     */
    case object Abort extends Input

    /**
     * A command that was not unknown. Possibly a typo.
     */
    case class Unknown(line: String) extends Input

  }

  /**
   * Prints the welcome banner and starts the interactive shell.
   */
  override def run(): Unit = {
    Thread.sleep(100)
    printWelcomeBanner()
    loop()
  }

  /**
   * Continuously reads a line of input from the input stream, parses and executes it.
   */
  def loop(): Unit = {
    while (!Thread.currentThread().isInterrupted) {
      Console.print(prompt)
      Console.flush()
      val line = scala.io.StdIn.readLine()
      val cmd = parse(line)
      try {
        execute(cmd)
      } catch {
        case e: Exception =>
          Console.println(e.getMessage)
          e.printStackTrace()
      }
    }
  }

  /**
   * Parses the string `line` into a command.
   */
  private def parse(line: String): Input = line match {
    case null => Input.Abort
    case "" => Input.Nop
    case "help" => Input.Help
    case "status" => Input.Status
    case "pause" => Input.Pause
    case "resume" | "unpause" | "continue" => Input.Unpause
    case "exit" | "quit" => Input.Exit
    case "abort" => Input.Abort
    case s if s.startsWith("print") => Input.Print(s.substring("print ".length))
    case _ => Input.Unknown(line)
  }

  /**
   * Executes the given command `cmd`
   */
  private def execute(cmd: Input): Unit = cmd match {
    case Input.Nop => // nop

    case Input.Status =>
      Console.println(s"Queue Size: ${solver.getQueueSize}, Total Facts: ${solver.getNumberOfFacts}.")

    case Input.Pause =>
      Console.println("Fixpoint computation paused.")
      solver.pause()

    case Input.Unpause =>
      Console.println("Fixpoint computation resumed.")
      solver.resume()

    case Input.Exit =>
      Thread.currentThread().interrupt()

    case Input.Abort =>
      System.exit(1)

    case Input.Print(name) =>
      val m = solver.getModel
      if (m == null)
        Console.println("Model not yet computed.")
      else
        PrettyPrint.print(name, m)

    case Input.Help =>
      Console.println("Available commands:")
      Console.println("  status     -- prints basic info about the fixpoint computation.")
      Console.println("  pause      -- pauses the fixpoint computation.")
      Console.println("  resume     -- resumes the fixpoint computation.")
      Console.println("  exit       -- graceful shutdown.")
      Console.println("  abort      -- immediate shutdown.")

    case Input.Unknown(s) => Console.println(s"Unknown command '$s'. Try `help'.")
  }

  /**
   * Prints the welcome banner to the console.
   */
  private def printWelcomeBanner(): Unit = {
    Console.println(s"Welcome to Flix ${Version.currentVersion}!  Type 'help' for more information.")
    Console.println(s"Enter a command and hit return. Type 'exit' or press ctrl+d to quit.")
  }

  /**
   * Prints the prompt.
   */
  private def prompt: String = "flix> "
}

