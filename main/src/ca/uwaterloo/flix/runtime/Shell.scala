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

package ca.uwaterloo.flix.runtime

import java.io.File
import java.nio.file._
import java.util.concurrent.Executors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Def, Root}
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualTerminal}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.vt.VirtualString.{Blue, Cyan, NewLine}

import scala.collection.JavaConverters._

class Shell(files: List[File], main: Option[String], options: Options) {

  /**
    * The minimum amount of time between runs of the compiler.
    */
  private val Delay: Long = 1000 * 1000 * 1000

  /**
    * The default color context.
    */
  private implicit val _ = TerminalContext.AnsiTerminal

  /**
    * The executor service.
    */
  private val executorService = Executors.newSingleThreadExecutor()

  /**
    * The current model (if any).
    */
  private var model: Model = _

  /**
    * The current watcher (if any).
    */
  private var watcher: WatcherThread = _

  /**
    * A common super-type for input commands.
    */
  sealed trait Command

  object Command {

    /**
      * No input.
      */
    case object Nop extends Command

    /**
      * End of input.
      */
    case object Eof extends Command

    /**
      * Browse the definitions in the given namespace.
      */
    case class Browse(ns: String) extends Command

    /**
      * Runs the current program.
      */
    case object Run extends Command

    /**
      * Prints information about the available commands.
      */
    case object Help extends Command

    /**
      * Prints the given constant, relation or lattice.
      */
    case class Print(name: String) extends Command

    /**
      * Gracefully terminates Flix.
      */
    case object Quit extends Command

    /**
      * Watch loaded paths for changes.
      */
    case object Watch extends Command

    /**
      * Unwatch loaded paths for changes.
      */
    case object Unwatch extends Command

    /**
      * A command that was not unknown. Possibly a typo.
      */
    case class Unknown(line: String) extends Command

  }

  /**
    * Continuously reads a line of input from the input stream, parses and executes it.
    */
  def loop(): Unit = {
    // Print welcome banner.
    printWelcomeBanner()

    // Loop forever.
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
  private def parse(line: String): Command = {
    if (line == null)
      return Command.Eof

    if (line == "")
      return Command.Nop

    if (line.startsWith(":browse")) {
      return Command.Browse(line.substring(":browse".length).trim)
    }

    line match {
      case "run" => Command.Run
      case ":help" | ":h" | ":?" => Command.Help
      case ":quit" | ":q" => Command.Quit
      case ":watch" | ":w" => Command.Watch
      case ":unwatch" => Command.Unwatch
      case s if s.startsWith("print") => Command.Print(s.substring("print ".length))
      case _ => Command.Unknown(line)
    }
  }

  /**
    * Executes the given command `cmd`
    */
  private def execute(cmd: Command): Unit = cmd match {
    case Command.Nop => // nop

    case Command.Eof | Command.Quit =>
      Console.println("Thanks, and goodbye.")
      Thread.currentThread().interrupt()

    case Command.Run =>
      val future = executorService.submit(new CompilerThread())
      future.get()

    case Command.Browse(ns) =>
      val vt = new VirtualTerminal
      val matchedDefs = getDefinitionsByNamespace(ns, model.getRoot)
      for (defn <- matchedDefs.sortBy(_.sym.name)) {
        vt << "def " << Blue(defn.sym.toString) << ": " << Cyan(defn.tpe.toString) << NewLine
      }
      Console.print(vt.fmt)

    case Command.Print(name) =>
      if (model == null)
        Console.println("Model not yet computed.")
      else
        PrettyPrint.print(name, model)

    case Command.Help =>
      Console.println("  Command    Alias    Arguments        Description")
      Console.println()
      Console.println("  run                                  compile and run.")
      Console.println("  print                                print a relation/lattice.")
      Console.println("  :browse             <ns>             shows the definitions in the given namespace.")
      Console.println("  :quit      :q                        shutdown.")
      Console.println("  :watch     :w                        watch loaded paths for changes.")
      Console.println("  :unwatch   :w                        unwatch loaded paths for changes.")

    case Command.Watch =>
      // Check if the watcher is already initialized.
      if (watcher != null)
        return

      // Compute the set of directories to watch.
      val directories = files.map(_.toPath.toAbsolutePath.getParent)

      // Print debugging information.
      Console.println("Watching Directories:")
      for (directory <- directories) {
        Console.println(s"  $directory")
      }

      watcher = new WatcherThread(directories)
      watcher.start()

    case Command.Unwatch =>
      watcher.interrupt()
      Console.println("Unwatched loaded paths.")

    case Command.Unknown(s) => Console.println(s"Unknown command '$s'. Try `help'.")
  }

  /**
    * Prints the welcome banner to the console.
    */
  private def printWelcomeBanner(): Unit = {
    Console.println(s"Welcome to Flix ${Version.CurrentVersion}!  Type 'help' for more information.")
    Console.println(s"Enter a command and hit return. Type 'exit' or press ctrl+d to quit.")
  }

  /**
    * Prints the prompt.
    */
  private def prompt: String = "flix> "

  /**
    * Returns the definitions in the given namespace.
    */
  private def getDefinitionsByNamespace(ns: String, root: Root): List[Def] = {
    val namespace: List[String] = if (ns == "" || ns == ".")
      Nil
    else if (!ns.contains(".")) {
      List(ns)
    } else {
      val index = ns.indexOf('.')
      ns.substring(0, index).split('/').toList
    }

    root.defs.foldLeft(Nil: List[Def]) {
      case (xs, (sym, defn)) if sym.namespace == namespace && !defn.mod.isSynthetic =>
        defn :: xs
      case (xs, _) => xs
    }
  }

  /**
    * A thread to run the Flix compiler in.
    */
  class CompilerThread() extends Runnable {
    override def run(): Unit = {
      // configure Flix and add the paths.
      val flix = new Flix()
      flix.setOptions(options)
      for (file <- files) {
        flix.addPath(file.toPath)
      }

      // check if a main function was given.
      if (main.nonEmpty) {
        val name = main.get
        flix.addReachableRoot(name)
      }

      // compute the least model.
      val timer = new Timer(flix.solve())
      timer.getResult match {
        case Validation.Success(m, errors) =>
          model = m
          if (main.nonEmpty) {
            val name = main.get
            val evalTimer = new Timer(m.evalToString(name))
            Console.println(s"$name returned `${evalTimer.getResult}' (compile: ${timer.fmt}, execute: ${evalTimer.fmt})")
          }
        case Validation.Failure(errors) =>
          errors.foreach(e => println(e.message.fmt))
      }
    }
  }

  /**
    * A thread to watch over changes in a collection of directories.
    */
  class WatcherThread(paths: List[Path]) extends Thread {

    // Initialize a new watcher service.
    val watchService: WatchService = FileSystems.getDefault.newWatchService

    // Register each directory.
    for (path <- paths) {
      if (Files.isDirectory(path)) {
        path.register(watchService, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_MODIFY)
      }
    }

    override def run(): Unit = try {
      // Record the last timestamp of a change.
      var lastChanged = System.nanoTime()

      // Loop until interrupted.
      while (!Thread.currentThread().isInterrupted) {
        // Wait for a set of events.
        val watchKey = watchService.take()
        // Iterate through each event.
        for (event <- watchKey.pollEvents().asScala) {
          // Check if a file with ".flix" extension changed.
          val changedPath = event.context().asInstanceOf[Path]
          if (changedPath.toString.endsWith(".flix")) {
            println(s"File: '$changedPath' changed.")
          }
        }

        // Check if sufficient time has passed since the last compilation.
        val currentTime = System.nanoTime()
        if ((currentTime - lastChanged) >= Delay) {
          lastChanged = currentTime
          // Allow a short delay before running the compiler.
          Thread.sleep(50)
          executorService.submit(new CompilerThread())
        }

        // Reset the watch key.
        watchKey.reset()
      }
    } catch {
      case ex: InterruptedException => // nop, shutdown.
    }

  }

}

