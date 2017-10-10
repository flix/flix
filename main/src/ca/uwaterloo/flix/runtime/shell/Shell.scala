/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.shell

import java.nio.file._
import java.util.concurrent.Executors
import java.util.logging.{Level, Logger}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.runtime.Model
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.tc.Show
import ca.uwaterloo.flix.util.tc.Show._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualTerminal}
import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.{Terminal, TerminalBuilder}

import scala.collection.JavaConverters._
import scala.collection.mutable

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
  private val sourcePaths = mutable.Set.empty[Path] ++ initialPaths

  /**
    * The current flix instance (initialized on startup).
    */
  private var flix: Flix = _

  /**
    * The current typed ast root (initialized on startup).
    */
  private var root: Root = _

  /**
    * The current model (if any).
    */
  private var model: Model = _

  /**
    * The current watcher (if any).
    */
  private var watcher: WatcherThread = _

  /**
    * The definition symbol to use for the expression.
    */
  private val symbol: Symbol.DefnSym = Symbol.mkDefnSym("$1")

  /**
    * The current expression (if any).
    */
  private var expression: String = _

  /**
    * Continuously reads a line of input from the input stream, parses and executes it.
    */
  def loop(): Unit = {
    // Silence JLine warnings about terminal type.
    Logger.getLogger("org.jline").setLevel(Level.OFF)

    // Initialize the terminal.
    implicit val terminal: Terminal = TerminalBuilder.builder().system(true).build()

    // Initialize the terminal line reader.
    val reader = LineReaderBuilder.builder().appName("flix").terminal(terminal).build()

    // Print the welcome banner.
    printWelcomeBanner()

    // Trigger a compilation of the source input files.
    execReload()

    try {
      // Repeatedly try to read an input from the line reader.
      while (!Thread.currentThread().isInterrupted) {
        // Try to read a command.
        val line = reader.readLine(prompt)

        // Parse the command.
        val cmd = Command.parse(line)
        try {
          // Try to execute the command. Catch any exception.
          execute(cmd)
        } catch {
          case e: Exception =>
            Console.println(e.getMessage)
            e.printStackTrace()
        }
      }
    } catch {
      case ex: UserInterruptException => // nop, exit gracefully.
      case ex: EndOfFileException => // nop, exit gracefully.
    }

    // Print goodbye message.
    terminal.writer().println("Thanks, and goodbye.")
  }

  /**
    * Prints the welcome banner to the terminal.
    */
  private def printWelcomeBanner()(implicit terminal: Terminal): Unit = {
    val banner =
      """     __   _   _
        |    / _| | | (_)             Welcome to Flix __VERSION__
        |   | |_  | |  _  __  __
        |   |  _| | | | | \ \/ /      Enter an expression or command, and hit return.
        |   | |   | | | |  >  <       Type ':help' for more information.
        |   |_|   |_| |_| /_/\_\      Type ':quit' or press 'ctrl + d' to exit.
      """.stripMargin

    terminal.writer().println(banner.replaceAll("__VERSION__", Version.CurrentVersion.toString))
    terminal.flush()
  }

  /**
    * Returns the Flix prompt.
    */
  private def prompt: String = "flix> "

  /**
    * Executes the given command `cmd`.
    */
  private def execute(cmd: Command)(implicit terminal: Terminal): Unit = cmd match {
    case Command.Nop => // nop
    case Command.Eval(s) => execEval(s)
    case Command.TypeOf(exp) => execTypeOf(exp)
    case Command.KindOf(exp) => execKindOf(exp)
    case Command.Browse(ns) => execBrowse(ns)
    case Command.Doc(fqn) => execDoc(fqn)
    case Command.Search(s) => execSearch(s)
    case Command.Load(s) => execLoad(s)
    case Command.Unload(s) => execUnload(s)
    case Command.Reload => execReload()
    case Command.Solve => execSolve()
    case Command.Rel(fqn, needle) => execRel(fqn, needle)
    case Command.Lat(fqn, needle) => execLat(fqn, needle)
    case Command.Watch => execWatch()
    case Command.Unwatch => execUnwatch()
    case Command.Quit => execQuit()
    case Command.Help => execHelp()
    case Command.Unknown(s) => execUnknown(s)
  }

  /**
    * Executes the eval command.
    */
  private def execEval(s: String)(implicit terminal: Terminal): Unit = {
    // Set the expression.
    expression = s.trim

    // Recompile the program.
    execReload()

    // Solve the program (to retrieve the model).
    execSolve()

    // Evaluate the function and get the result.
    val result = model.evalToString(symbol.toString)

    // Write the result to the terminal.
    terminal.writer().println(result)
  }

  /**
    * Shows the type of the given expression `exp`.
    */
  private def execTypeOf(exp: String)(implicit terminal: Terminal, s: Show[Type]): Unit = {
    // Set the expression.
    expression = exp

    // Recompile the program.
    execReload()

    // Retrieve the definition.
    val defn = root.defs(symbol)

    // Compute the result type, i.e. the last type argument.
    val tpe = defn.tpe.typeArguments.last

    // Print the type to the console.
    val vt = new VirtualTerminal
    vt << Cyan(tpe.show) << NewLine
    terminal.writer().print(vt.fmt)
  }

  /**
    * Shows the kind of the given expression `exp`.
    */
  private def execKindOf(exp: String)(implicit terminal: Terminal, s: Show[Type]): Unit = {
    // Set the expression.
    expression = exp

    // Recompile the program.
    execReload()

    // Retrieve the definition.
    val defn = root.defs(symbol)

    // Retrieve the kind.
    val kind = defn.tpe.kind

    // Print the kind to the console.
    val vt = new VirtualTerminal
    vt << Magenta(kind.toString) << NewLine
    terminal.writer().print(vt.fmt)
  }

  /**
    * Executes the browse command.
    */
  private def execBrowse(nsOpt: Option[String])(implicit terminal: Terminal): Unit = nsOpt match {
    case None =>
      // Case 1: Browse available namespaces.

      // Construct a new virtual terminal.
      val vt = new VirtualTerminal

      // Find the available namespaces.
      val namespaces = namespacesOf(root)

      vt << Bold("Namespaces:") << Indent << NewLine << NewLine
      for (namespace <- namespaces.toList.sorted) {
        vt << namespace << NewLine
      }
      vt << Dedent << NewLine

      // Print the virtual terminal to the console.
      terminal.writer().print(vt.fmt)

    case Some(ns) =>
      // Case 2: Browse a specific namespace.

      // Construct a new virtual terminal.
      val vt = new VirtualTerminal

      // Print the matched definitions.
      val matchedDefs = getDefinitionsByNamespace(ns, root)
      if (matchedDefs.nonEmpty) {
        vt << Bold("Definitions:") << Indent << NewLine << NewLine
        for (defn <- matchedDefs.sortBy(_.sym.name)) {
          vt << Bold("def ") << Blue(defn.sym.name) << "("
          if (defn.fparams.nonEmpty) {
            vt << defn.fparams.head.sym.text << ": " << Cyan(defn.fparams.head.tpe.show)
            for (fparam <- defn.fparams.tail) {
              vt << ", " << fparam.sym.text << ": " << Cyan(fparam.tpe.show)
            }
          }
          vt << "): " << Cyan(defn.tpe.typeArguments.last.show) << NewLine
        }
        vt << Dedent << NewLine
      }

      // Print the matched relations.
      val matchedRels = getRelationsByNamespace(ns, root)
      if (matchedRels.nonEmpty) {
        vt << Bold("Relations:") << Indent << NewLine << NewLine
        for (rel <- matchedRels.sortBy(_.sym.name)) {
          vt << Bold("rel ") << Blue(rel.sym.toString) << "("
          vt << rel.attributes.head.name << ": " << Cyan(rel.attributes.head.tpe.show)
          for (attr <- rel.attributes.tail) {
            vt << ", " << attr.name << ": " << Cyan(attr.tpe.show)
          }
          vt << ")" << NewLine
        }
        vt << Dedent << NewLine
      }

      // Print the matched lattices.
      val matchedLats = getLatticesByNamespace(ns, root)
      if (matchedLats.nonEmpty) {
        vt << Bold("Lattices:") << Indent << NewLine << NewLine
        for (lat <- matchedLats.sortBy(_.sym.name)) {
          vt << Bold("lat ") << Blue(lat.sym.toString) << "("
          vt << lat.attributes.head.name << ": " << Cyan(lat.attributes.head.tpe.show)
          for (attr <- lat.attributes.tail) {
            vt << ", " << attr.name << ": " << Cyan(attr.tpe.show)
          }
          vt << ")" << NewLine
        }
        vt << Dedent << NewLine
      }

      // Print the virtual terminal to the console.
      terminal.writer().print(vt.fmt)
  }

  /**
    * Executes the doc command.
    */
  private def execDoc(fqn: String): Unit = {
    val sym = Symbol.mkDefnSym(fqn)
    root.defs.get(sym) match {
      case None =>
        Console.println(s"Undefined symbol: '$sym'.")
      case Some(defn) => defn.doc match {
        case None =>
          Console.println(s"No documentation for: '$sym'.")
        case Some(doc) =>
          Console.println(doc.text)
      }

    }
  }

  /**
    * Executes the help command.
    */
  private def execHelp(): Unit = {
    Console.println("  Command       Arguments         Purpose")
    Console.println()
    Console.println("  <expr>                          Evaluates the expression <expr>.")
    Console.println("  :type :t      <expr>            Shows the type of <expr>.")
    Console.println("  :kind :k      <expr>            Shows the kind of <expr>.")
    Console.println("  :browse       <ns>              Shows all entities in <ns>.")
    Console.println("  :doc          <fqn>             Shows documentation for <fqn>.")
    Console.println("  :search       <needle>          Shows all entities that match <needle>.")
    Console.println("  :load         <path>            Adds <path> as a source file.")
    Console.println("  :unload       <path>            Removes <path> as a source file.")
    Console.println("  :reload :r                      Recompiles every source file.")
    Console.println("  :solve                          Computes the least fixed point.")
    Console.println("  :rel          <fqn> [needle]    Shows all rows in the relation <fqn> that match <needle>.")
    Console.println("  :lat          <fqn> [needle]    Shows all rows in the lattice <fqn> that match <needle>.")
    Console.println("  :watch :w                       Watches all source files for changes.")
    Console.println("  :unwatch                        Unwatches all source files for changes.")
    Console.println("  :quit :q                        Terminates the Flix shell.")
    Console.println("  :help :h :?                     Shows this helpful information.")
    Console.println()
  }

  private def execUnknown(s: String): Unit = {
    Console.println(s"Unknown command '$s'. Try `:help'.")
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
    sourcePaths += path
    Console.println(s"Path '$path' was loaded.")
  }

  /**
    * Executes the unload command.
    */
  private def execUnload(s: String): Unit = {
    val path = Paths.get(s)
    if (!(sourcePaths contains path)) {
      Console.println(s"Path '$path' was not loaded.")
      return
    }
    sourcePaths -= path
    Console.println(s"Path '$path' was unloaded.")
  }

  /**
    * Reloads every loaded path.
    */
  private def execReload(): Unit = {
    // Instantiate a fresh flix instance.
    flix = new Flix()
    flix.setOptions(options)
    for (path <- sourcePaths) {
      flix.addPath(path)
    }
    if (expression != null) {
      // TODO: Refactor
      flix.addNamedExp(Symbol.mkDefnSym("$1"), expression)
    }

    // Check if a main function was given.
    // TODO: Deprecated.
    if (main.nonEmpty) {
      val name = main.get
      flix.addReachableRoot(name)
    }

    // Print the error messages (if any).
    flix.check() match {
      case Validation.Success(ast, _) =>
        this.root = ast
      case Validation.Failure(errors) =>
        errors.foreach(e => println(e.message.fmt))
    }

  }

  /**
    * Searches for the given `needle`.
    */
  private def execSearch(needle: String): Unit = {
    // Construct a new virtual terminal.
    val vt = new VirtualTerminal
    vt << Bold("Definitions:") << Indent << NewLine << NewLine
    // TODO: Group by namespace.
    for ((sym, defn) <- root.defs) {
      if (sym.name.toLowerCase().contains(needle)) {
        vt << Bold("def ") << Blue(defn.sym.name) << "("
        if (defn.fparams.nonEmpty) {
          vt << defn.fparams.head.sym.text << ": " << Cyan(defn.fparams.head.tpe.show)
          for (fparam <- defn.fparams.tail) {
            vt << ", " << fparam.sym.text << ": " << Cyan(fparam.tpe.show)
          }
        }
        vt << "): " << Cyan(defn.tpe.typeArguments.last.show) << NewLine
      }
    }

    vt << Dedent << NewLine
    // Print the virtual terminal to the console.
    Console.print(vt.fmt)
  }

  /**
    * Computes the least fixed point.
    */
  private def execSolve(): Unit = {
    val future = executorService.submit(new SolverThread())
    future.get()
  }

  /**
    * Shows the rows in the given relation `fqn` that match the optional `needle`.
    */
  private def execRel(fqn: String, needle: Option[String]): Unit = {
    model.getRelations.get(fqn) match {
      case None =>
        Console.println(s"Undefined relation: '$fqn'.")
      case Some((attributes, rows)) =>
        val ascii = new AsciiTable().withCols(attributes: _*).withFilter(needle)
        for (row <- rows) {
          ascii.mkRow(row)
        }
        ascii.write(System.out)
    }
  }

  /**
    * Shows the rows in the given lattice `fqn` that match the optional `needle`.
    */
  private def execLat(fqn: String, needle: Option[String]): Unit = {
    model.getLattices.get(fqn) match {
      case None =>
        Console.println(s"Undefined lattice: '$fqn'.")
      case Some((attributes, rows)) =>
        val ascii = new AsciiTable().withCols(attributes: _*).withFilter(needle)
        for (row <- rows) {
          ascii.mkRow(row)
        }
        ascii.write(System.out)
    }
  }

  private def execWatch(): Unit = {
    // Check if the watcher is already initialized.
    if (watcher != null)
      return

    // Compute the set of directories to watch.
    val directories = sourcePaths.map(_.toAbsolutePath.getParent).toList

    // Print debugging information.
    Console.println("Watching Directories:")
    for (directory <- directories) {
      Console.println(s"  $directory")
    }

    watcher = new WatcherThread(directories)
    watcher.start()
  }

  private def execUnwatch(): Unit = {
    watcher.interrupt()
    watcher = null
    Console.println("Unwatched loaded paths.")
  }

  private def execQuit(): Unit = {
    Thread.currentThread().interrupt()
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
    * A thread to run the fixed point solver in.
    */
  class SolverThread() extends Runnable {
    override def run(): Unit = {
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
          executorService.submit(new Runnable {
            def run(): Unit = execReload()
          })
        }

        // Reset the watch key.
        watchKey.reset()
      }
    } catch {
      case ex: InterruptedException => // nop, shutdown.
    }

  }

}

