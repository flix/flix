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

import java.nio.file._
import java.util.concurrent.Executors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Def, Root, Table}
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualTerminal}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.vt.VirtualString._

import scala.collection.mutable
import scala.collection.JavaConverters._

class Shell(initialPaths: List[Path], main: Option[String], options: Options) {

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
    * The mutable set of paths to load.
    */
  private val currentPaths = mutable.Set.empty[Path] ++ initialPaths

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
    case class Browse(ns: Option[String]) extends Command

    /**
      * Evaluate the given expression `s`.
      */
    case class Eval(s: String) extends Command

    /**
      * Loads the given path `s`.
      */
    case class Load(s: String) extends Command

    /**
      * Unloads the given path `s`.
      */
    case class Unload(s: String) extends Command

    /**
      * Runs the current program.
      */
    // TODO: Rename to reload.
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
      if (line == ":browse") {
        return Command.Browse(None)
      }
      val ns = line.substring(":browse".length).trim
      return Command.Browse(Some(ns))
    }

    if (line.startsWith(":load")) {
      val path = line.substring(":load".length).trim
      return Command.Load(path)
    }

    if (line.startsWith(":unload")) {
      val path = line.substring(":unload".length).trim
      return Command.Unload(path)
    }

    line match {
      case ":r" | ":run" => Command.Run
      case ":help" | ":h" | ":?" => Command.Help
      case ":quit" | ":q" => Command.Quit
      case ":watch" | ":w" => Command.Watch
      case ":unwatch" => Command.Unwatch
      case s if s.startsWith(":print") => Command.Print(s.substring(":print ".length))
      case s if s.startsWith(":") => Command.Unknown(line)
      case s => Command.Eval(s)
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

    case Command.Eval(s) =>
      Console.println(s"Eval not implemented yet: ${s}")

    case Command.Load(s) =>
      execLoad(s)

    case Command.Unload(s) =>
      execUnload(s)

    case Command.Run =>
      val future = executorService.submit(new CompilerThread())
      future.get()

    case Command.Browse(nsOpt) => execBrowse(nsOpt)

    case Command.Print(name) =>
      if (model == null)
        Console.println("Model not yet computed.")
      else
        PrettyPrint.print(name, model)

    case Command.Help => execHelp()

    case Command.Watch =>
      // Check if the watcher is already initialized.
      if (watcher != null)
        return

      // Compute the set of directories to watch.
      val directories = currentPaths.map(_.toAbsolutePath.getParent).toList

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
    * Executes the browse command.
    */
  private def execBrowse(nsOpt: Option[String]): Unit = nsOpt match {
    case None =>
      // Case 1: Browse available namespaces.

      // Construct a new virtual terminal.
      val vt = new VirtualTerminal

      // Find the available namespaces.
      val namespaces = namespacesOf(model.getRoot)

      vt << Bold("Namespaces:") << Indent << NewLine << NewLine
      for (namespace <- namespaces.toList.sorted) {
        vt << namespace << NewLine
      }
      vt << Dedent << NewLine

      // Print the virtual terminal to the console.
      Console.print(vt.fmt)

    case Some(ns) =>
      // Case 2: Browse a specific namespace.

      // Construct a new virtual terminal.
      val vt = new VirtualTerminal

      // Print the matched definitions.
      val matchedDefs = getDefinitionsByNamespace(ns, model.getRoot)
      if (matchedDefs.nonEmpty) {
        vt << Bold("Definitions:") << Indent << NewLine << NewLine
        for (defn <- matchedDefs.sortBy(_.sym.name)) {
          vt << Bold("def ") << Blue(defn.sym.name) << "("
          if (defn.formals.nonEmpty) {
            vt << defn.formals.head.sym.text << ": " << Cyan(defn.formals.head.tpe.toString)
            for (fparam <- defn.formals.tail) {
              vt << ", " << fparam.sym.text << ": " << Cyan(fparam.tpe.toString)
            }
          }
          vt << "): " << Cyan(defn.tpe.typeArguments.last.toString) << NewLine
        }
        vt << Dedent << NewLine
      }

      // Print the matched relations.
      val matchedRels = getRelationsByNamespace(ns, model.getRoot)
      if (matchedRels.nonEmpty) {
        vt << Bold("Relations:") << Indent << NewLine << NewLine
        for (rel <- matchedRels.sortBy(_.sym.name)) {
          vt << Bold("rel ") << Blue(rel.sym.toString) << "("
          vt << rel.attributes.head.name << ": " << Cyan(rel.attributes.head.tpe.toString)
          for (attr <- rel.attributes.tail) {
            vt << ", " << attr.name << ": " << Cyan(attr.tpe.toString)
          }
          vt << ")" << NewLine
        }
        vt << Dedent << NewLine
      }

      // Print the matched lattices.
      val matchedLats = getLatticesByNamespace(ns, model.getRoot)
      if (matchedLats.nonEmpty) {
        vt << Bold("Lattices:") << Indent << NewLine << NewLine
        for (lat <- matchedLats.sortBy(_.sym.name)) {
          vt << Bold("lat ") << Blue(lat.sym.toString) << "("
          vt << lat.attributes.head.name << ": " << Cyan(lat.attributes.head.tpe.toString)
          for (attr <- lat.attributes.tail) {
            vt << ", " << attr.name << ": " << Cyan(attr.tpe.toString)
          }
          vt << ")" << NewLine
        }
        vt << Dedent << NewLine
      }

      // Print the virtual terminal to the console.
      Console.print(vt.fmt)
  }

  /**
    * Executes the help command.
    */
  private def execHelp(): Unit = {
    Console.println("  Command    Alias    Arguments        Description")
    Console.println()
    Console.println("  :run       :r                        compile and run.")
    Console.println("  :print                               print a relation/lattice.")
    Console.println("  :browse             <ns>             shows the definitions in the given namespace.")
    Console.println("  :load               <path>           loads the given path.")
    Console.println("  :unload             <path>           unloads the given path.")
    Console.println("  :quit      :q                        shutdown.")
    Console.println("  :watch     :w                        watch loaded paths for changes.")
    Console.println("  :unwatch   :w                        unwatch loaded paths for changes.")
  }

  /**
    * Executes the load command.
    */
  private def execLoad(s: String): Unit = {
    val path = Paths.get(s)
    if (!Files.exists(path) || !Files.isRegularFile(path)) {
      Console.println(s"Path '$path' does not exist or is not a regular file.")
      return
    }
    currentPaths += path
    Console.println(s"Path '$path' was loaded.")
  }

  /**
    * Executes the unload command.
    */
  private def execUnload(s: String): Unit = {
    val path = Paths.get(s)
    if (!(currentPaths contains path)) {
      Console.println(s"Path '$path' was not loaded.")
      return
    }
    currentPaths -= path
    Console.println(s"Path '$path' was unloaded.")
  }

  /**
    * Returns the namespaces in the given AST `root`.
    */
  private def namespacesOf(root: Root): Set[String] = {
    val ns1 = root.defs.keySet.map(_.namespace.mkString("/"))
    val ns2 = root.tables.keySet.map(_.namespace.mkString("/"))
    (ns1 ++ ns2) - ""
  }

  /**
    * Returns the definitions in the given namespace.
    */
  private def getDefinitionsByNamespace(ns: String, root: Root): List[Def] = {
    val namespace: List[String] = getNameSpace(ns)
    root.defs.foldLeft(Nil: List[Def]) {
      case (xs, (sym, defn)) if sym.namespace == namespace && !defn.mod.isSynthetic =>
        defn :: xs
      case (xs, _) => xs
    }
  }

  /**
    * Returns the relations in the given namespace.
    */
  private def getRelationsByNamespace(ns: String, root: Root): List[Table.Relation] = {
    val namespace: List[String] = getNameSpace(ns)
    root.tables.foldLeft(Nil: List[Table.Relation]) {
      case (xs, (sym, t: Table.Relation)) if sym.namespace == namespace =>
        t :: xs
      case (xs, _) => xs
    }
  }

  /**
    * Returns the lattices in the given namespace.
    */
  private def getLatticesByNamespace(ns: String, root: Root): List[Table.Lattice] = {
    val namespace: List[String] = getNameSpace(ns)
    root.tables.foldLeft(Nil: List[Table.Lattice]) {
      case (xs, (sym, t: Table.Lattice)) if sym.namespace == namespace =>
        t :: xs
      case (xs, _) => xs
    }
  }

  /**
    * Interprets the given string `ns` as a namespace.
    */
  private def getNameSpace(ns: String): List[String] = {
    if (ns == "" || ns == ".")
    // Case 1: The empty namespace.
      Nil
    else if (!ns.contains(".")) {
      // Case 2: A simple namespace.
      List(ns)
    } else {
      // Case 3: A complex namespace.
      val index = ns.indexOf('.')
      ns.substring(0, index).split('/').toList
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
      for (path <- currentPaths) {
        flix.addPath(path)
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

