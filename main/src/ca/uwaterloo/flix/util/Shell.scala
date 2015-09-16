package ca.uwaterloo.flix.util

class Shell(implicit options: Options) {

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
//      val cmd = parse(line)
//      try {
//        execute(cmd)
//      } catch {
//        case e: Exception =>
//          Console.println("Exception: " + e.getMessage)
//          e.printStackTrace()
//      }
    }
  }

  // TODO. Ideas for commands:
  // :show
  // :reload
  // :stop (if fixpoint engine is running)


  private def printWelcomeBanner(): Unit = {
    println(s"Welcome to Flix ${Version.currentVersion}!  Type 'help' for more information.")
    println(s"Enter a command and hit return. Type 'exit' or press ctrl+d to quit.")
    print("> ")
  }

  private def prompt: String = "> "
}
