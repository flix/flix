package ca.uwaterloo.flix

import impl.ast2.Parser

object Shell {

  // TODO: Experiment with other shells to discver what works

  // TODO: Define grammar.

  def main(args: Array[String]): Unit = {

    val version = "v1.0.0"
    val codename = "Cold Winter"

    //    Runtime.getRuntime.addShutdownHook(new Thread {
    //      override def run(): Unit = {
    //        println("Oki!")
    //      }
    //    })

    println(s"Welcome to Flix $version ($codename)!  Type 'help' for more information.")
    println(s"Enter a command and hit return. Type 'exit' or press ctrl+d to quit.")
    print("> ")

    var line = scala.io.StdIn.readLine()
    while (line != null) {

      line match {
        case "" => // nop
        case "help" => println("Haha, just kidding. There is no help.")
        case "exit" | "quit" => System.exit(0)
        case _ =>
          val ast = Parser.parse(line)
          println(ast)
        //case _ => println(s"Unknown command '$line'. Did you mean ???")
      }

      System.out.flush()
      print("> ")
      System.out.flush()
      line = scala.io.StdIn.readLine()
    }
  }

  println("Goodbye.")

}
