package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.util.Version

class Shell(solver: Solver) extends Thread {

  /**
   * A common super-type for input commands.
   */
  sealed trait Input

  object Input {

    /**
     * The no-operation command.
     */
    case object Nop extends Input

    /**
     * The help command.
     */
    case object Help extends Input

    /**
     * The pause command.
     */
    case object Pause extends Input

    /**
     * The un-pause command.
     */
    case object Unpause extends Input

    /**
     * The exit command.
     */
    case object Exit extends Input

    /**
     * The abort command.
     */
    case object Abort extends Input

    /**
     * An unknown command.
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
    case "exit" | "quit" => Input.Exit
    case "pause" => Input.Pause
    case "resume" | "unpause" | "continue" => Input.Unpause
    case "abort" => Input.Abort
    case "help" => Input.Help
    case _ => Input.Unknown(line)
  }

  /**
   * Executes the given command `cmd`
   */
  private def execute(cmd: Input): Unit = cmd match {
    case Input.Nop => // nop

    case Input.Pause =>
      Console.println("Fixpoint computation paused.")
      solver.pause()

    case Input.Unpause =>
      Console.println("Fixpoint computation resumed.")
      solver.resume()

    case Input.Exit =>

    case Input.Abort =>
      System.exit(1)

    case Input.Help =>
      Console.println("Available commands:")
      Console.println("  pause      -- pauses the fixpoint computation.")
      Console.println("  resume     -- resumes the fixpoint computation.")
      Console.println("  exit       -- graceful shutdown.")
      Console.println("  abort      -- immediate shutdown.")

    case Input.Unknown(s) => Console.println(s"Illegal command '$s'. Try `help'.")
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

