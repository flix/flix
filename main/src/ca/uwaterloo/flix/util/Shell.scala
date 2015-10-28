package ca.uwaterloo.flix.util

class Shell(implicit options: Options) {

  sealed trait Command

  object Command {

    case object Nop extends Command

    case object Help extends Command

    case object Exit extends Command

    case class Unknown(line: String) extends Command

  }

  /**
   * Prints the welcome banner and starts the interactive shell.
   */
  def startAndAwait(): Unit = {
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
          Console.println("Exception: " + e.getMessage)
          e.printStackTrace()
      }
    }
  }

  /**
   * Parses the string `line` into a command.
   */
  private def parse(line: String): Command = line match {
    case null => Command.Exit
    case "" => Command.Nop
    case "exit" | "quit" | "abort" | "stop" => Command.Exit
    case "help" => Command.Help
    case _ => Command.Unknown(line)
  }

  /**
   * Executes the given command `cmd`
   */
  private def execute(cmd: Command): Unit = cmd match {
    case Command.Exit => System.exit(1)
    case Command.Help =>
      Console.println("Available commands:")
      Console.println("  exit")
    case Command.Nop => // nop
    case Command.Unknown(s) => Console.println(s"Illegal command '$s'. Try `help'.")
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
