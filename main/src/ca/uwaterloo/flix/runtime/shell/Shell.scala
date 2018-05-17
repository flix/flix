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
import ca.uwaterloo.flix.language.ast.Ast.HoleContext
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.runtime.{Benchmarker, Model, Tester}
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
    * The definition symbol to use for the expression.
    */
  private val sym: Symbol.DefnSym = Symbol.mkDefnSym("$1")

  /**
    * The current expression (if any).
    */
  private var exp: String = _

  /**
    * The current watcher (if any).
    */
  private var watcher: WatcherThread = _

  /**
    * Continuously reads a line of input from the terminal, parses and executes it.
    */
  def loop(): Unit = {
    // Silence JLine warnings about terminal type.
    Logger.getLogger("org.jline").setLevel(Level.OFF)

    // Initialize the terminal.
    implicit val terminal: Terminal = TerminalBuilder
      .builder()
      .system(true)
      .build()

    // Initialize the terminal line reader.
    val reader = LineReaderBuilder
      .builder()
      .appName("flix")
      .terminal(terminal)
      .build()

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
            terminal.writer().print(e.getMessage)
            e.printStackTrace(terminal.writer())
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
        |    / _| | | (_)            Welcome to Flix __VERSION__
        |   | |_  | |  _  __  __
        |   |  _| | | | | \ \/ /     Enter an expression or command, and hit return.
        |   | |   | | | |  >  <      Type ':help' for more information.
        |   |_|   |_| |_| /_/\_\     Type ':quit' or press 'ctrl + d' to exit.
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
    case Command.TypeOf(e) => execTypeOf(e)
    case Command.KindOf(e) => execKindOf(e)
    case Command.EffectOf(e) => execEffectOf(e)
    case Command.Hole(fqn) => execHole(fqn)
    case Command.Browse(ns) => execBrowse(ns)
    case Command.Doc(fqn) => execDoc(fqn)
    case Command.Search(s) => execSearch(s)
    case Command.Load(s) => execLoad(s)
    case Command.Unload(s) => execUnload(s)
    case Command.Reload => execReload()
    case Command.Solve => execSolve()
    case Command.Rel(fqn, needle) => execRel(fqn, needle)
    case Command.Lat(fqn, needle) => execLat(fqn, needle)
    case Command.Benchmark => execBenchmark()
    case Command.Test => execTest()
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
    this.exp = s.trim

    // Recompile the program.
    execReload()

    // Solve the program (to retrieve the model).
    execSolve()

    // Evaluate the function and get the result.
    val result = this.model.evalToString(this.sym.toString)

    // Write the result to the terminal.
    terminal.writer().println(result)
  }

  /**
    * Shows the type of the given expression `exp`.
    */
  private def execTypeOf(exp: String)(implicit terminal: Terminal, s: Show[Type]): Unit = {
    // Set the expression.
    this.exp = exp

    // Recompile the program.
    execReload()

    // Retrieve the definition.
    val defn = this.root.defs(this.sym)

    // Compute the result type, i.e. the last type argument.
    val tpe = defn.tpe.typeArguments.last

    // Print the type to the terminal.
    val vt = new VirtualTerminal
    vt << Cyan(tpe.show) << NewLine
    terminal.writer().print(vt.fmt)
  }

  /**
    * Shows the kind of the given expression `exp`.
    */
  private def execKindOf(exp: String)(implicit terminal: Terminal, s: Show[Type]): Unit = {
    // Set the expression.
    this.exp = exp

    // Recompile the program.
    execReload()

    // Retrieve the definition.
    val defn = this.root.defs(this.sym)

    // Retrieve the kind.
    val kind = defn.tpe.kind

    // Print the kind to the terminal.
    val vt = new VirtualTerminal
    vt << Green(kind.show) << NewLine
    terminal.writer().print(vt.fmt)
  }

  /**
    * Shows the effect of the given expression `exp`.
    */
  private def execEffectOf(exp: String)(implicit terminal: Terminal, s: Show[Type]): Unit = {
    // Set the expression.
    this.exp = exp

    // Recompile the program.
    execReload()

    // Retrieve the definition.
    val defn = this.root.defs(this.sym)

    // Retrieve the effect.
    val effect = defn.eff

    // Print the effect to the terminal.
    val vt = new VirtualTerminal
    vt << Magenta(effect.show) << NewLine
    terminal.writer().print(vt.fmt)
  }

  /**
    * Shows the hole context of the given `fqn`.
    */
  private def execHole(fqn: String)(implicit terminal: Terminal, s: Show[Type]): Unit = {
    // Compute the hole symbol.
    val sym = Symbol.mkHoleSym(fqn)

    // Retrieve all the holes in the program.
    val holes = TypedAstOps.holesOf(root)

    // Lookup the hole symbol.
    holes.get(sym) match {
      case None =>
        // Case 1: Hole not found.
        terminal.writer().println(s"Undefined hole: '$fqn'.")
      case Some(HoleContext(_, holeType, env)) =>
        // Case 2: Hole found.
        val vt = new VirtualTerminal

        // Indent
        vt << "  "

        // Iterate through the premises, i.e. the variable symbols in scope.
        for ((varSym, varType) <- env) {
          vt << Blue(varSym.text) << ": " << Cyan(varType.show) << " " * 6
        }

        // Print the divider.
        vt << NewLine << "-" * 80 << NewLine

        // Print the goal.
        vt << Blue(sym.toString) << ": " << Cyan(holeType.show) << NewLine

        // Print the result to the terminal.
        terminal.writer().print(vt.fmt)
    }
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
      val namespaces = namespacesOf(this.root)

      vt << Bold("Namespaces:") << Indent << NewLine << NewLine
      for (namespace <- namespaces.toList.sorted) {
        vt << namespace << NewLine
      }
      vt << Dedent << NewLine

      // Print the result to the terminal.
      terminal.writer().print(vt.fmt)

    case Some(ns) =>
      // Case 2: Browse a specific namespace.

      // Construct a new virtual terminal.
      val vt = new VirtualTerminal

      // Print the matched definitions.
      val matchedDefs = getDefinitionsByNamespace(ns, this.root)
      if (matchedDefs.nonEmpty) {
        vt << Bold("Definitions:") << Indent << NewLine << NewLine
        for (defn <- matchedDefs.sortBy(_.sym.name)) {
          prettyPrintDef(defn, vt)
        }
        vt << Dedent << NewLine
      }

      // Print the matched relations.
      val matchedRels = getRelationsByNamespace(ns, this.root)
      if (matchedRels.nonEmpty) {
        vt << Bold("Relations:") << Indent << NewLine << NewLine
        for (rel <- matchedRels.sortBy(_.sym.name)) {
          prettyPrintRel(rel, vt)
        }
        vt << Dedent << NewLine
      }

      // Print the matched lattices.
      val matchedLats = getLatticesByNamespace(ns, this.root)
      if (matchedLats.nonEmpty) {
        vt << Bold("Lattices:") << Indent << NewLine << NewLine
        for (lat <- matchedLats.sortBy(_.sym.name)) {
          prettyPrintLat(lat, vt)
        }
        vt << Dedent << NewLine
      }

      // Print the result to the terminal.
      terminal.writer().print(vt.fmt)
  }

  /**
    * Executes the doc command.
    */
  private def execDoc(fqn: String)(implicit terminal: Terminal): Unit = {
    val sym = Symbol.mkDefnSym(fqn)
    this.root.defs.get(sym) match {
      case None =>
        // Case 1: Symbol not found.
        terminal.writer().println(s"Undefined symbol: '$sym'.")
      case Some(defn) =>
        // Case 2: Symbol found.

        // Construct a new virtual terminal.
        val vt = new VirtualTerminal
        prettyPrintDef(defn, vt)
        vt << defn.doc.text
        vt << NewLine

        // Print the result to the terminal.
        terminal.writer().print(vt.fmt)
    }
  }

  /**
    * Searches for the given `needle`.
    */
  private def execSearch(needle: String)(implicit terminal: Terminal): Unit = {
    /**
      * Returns `true` if the definition `d` is matched by the `needle`.
      */
    def isMatched(d: Def): Boolean = d.sym.name.toLowerCase.contains(needle.toLowerCase)

    // Construct a new virtual terminal.
    val vt = new VirtualTerminal
    vt << Bold("Definitions:") << Indent << NewLine << NewLine

    // Group definitions by namespace.
    val defsByNamespace = this.root.defs.values.groupBy(_.sym.namespace).toList

    // Loop through each namespace.
    for ((ns, defns) <- defsByNamespace) {
      // Compute the matched definitions.
      val matchedDefs = defns.filter(isMatched)

      // Print the namespace.
      if (matchedDefs.nonEmpty) {
        vt << Bold(ns.mkString("/")) << Indent << NewLine
        for (defn <- matchedDefs) {
          prettyPrintDef(defn, vt)
        }
        vt << Dedent << NewLine
      }
    }
    vt << Dedent << NewLine

    // Print the result to the terminal.
    terminal.writer().print(vt.fmt)
  }

  /**
    * Executes the load command.
    */
  private def execLoad(s: String)(implicit terminal: Terminal): Unit = {
    val path = Paths.get(s)
    if (!Files.exists(path) || !Files.isRegularFile(path)) {
      terminal.writer().println(s"Path '$path' does not exist or is not a regular file.")
      return
    }
    this.sourcePaths += path
    terminal.writer().println(s"Path '$path' was loaded.")
  }

  /**
    * Executes the unload command.
    */
  private def execUnload(s: String)(implicit terminal: Terminal): Unit = {
    val path = Paths.get(s)
    if (!(this.sourcePaths contains path)) {
      terminal.writer().println(s"Path '$path' was not loaded.")
      return
    }
    this.sourcePaths -= path
    terminal.writer().println(s"Path '$path' was unloaded.")
  }

  /**
    * Reloads every source path.
    */
  private def execReload()(implicit terminal: Terminal): Unit = {
    // Instantiate a fresh flix instance.
    this.flix = new Flix()
    this.flix.setOptions(options)

    // Add each path to Flix.
    for (path <- this.sourcePaths) {
      this.flix.addPath(path)
    }

    // Add the named expression (if any).
    if (this.exp != null) {
      this.flix.addNamedExp(this.sym, this.exp)
    }

    // Type check and print the error messages (if any).
    this.flix.check() match {
      case Validation.Success(ast) =>
        this.root = ast
        // Pretty print the holes (if any).
        prettyPrintHoles()
      case Validation.Failure(errors) =>
        for (error <- errors) {
          terminal.writer().print(error.message.fmt)
        }
    }
  }

  /**
    * Computes the least fixed point.
    */
  private def execSolve()(implicit terminal: Terminal): Unit = {
    val future = this.executorService.submit(new SolverThread())
    future.get()
  }

  /**
    * Shows the rows in the given relation `fqn` that match the optional `needle`.
    */
  private def execRel(fqn: String, needle: Option[String])(implicit terminal: Terminal): Unit = {
    // Check that the model has been computed.
    if (this.model == null) {
      terminal.writer().println(s"The model has not been computed. Run :solve.")
      return
    }

    // Lookup the relation.
    this.model.getRelations.get(fqn) match {
      case None =>
        // Case 1: The relation was not found.
        terminal.writer().println(s"Undefined relation: '$fqn'.")
      case Some((attributes, rows)) =>
        // Case 2: The relation was found.
        val ascii = new AsciiTable().withCols(attributes: _*).withFilter(needle)
        for (row <- rows) {
          ascii.mkRow(row)
        }
        ascii.write(terminal.writer())
    }
  }

  /**
    * Shows the rows in the given lattice `fqn` that match the optional `needle`.
    */
  private def execLat(fqn: String, needle: Option[String])(implicit terminal: Terminal): Unit = {
    // Check that the model has been computed.
    if (this.model == null) {
      terminal.writer().println(s"The model has not been computed. Run :solve.")
      return
    }

    // Lookup the lattice.
    this.model.getLattices.get(fqn) match {
      case None =>
        // Case 1: The lattice was not found.
        terminal.writer().println(s"Undefined lattice: '$fqn'.")
      case Some((attributes, rows)) =>
        // Case 1: The lattice was found.
        val ascii = new AsciiTable().withCols(attributes: _*).withFilter(needle)
        for (row <- rows) {
          ascii.mkRow(row)
        }
        ascii.write(terminal.writer())
    }
  }

  /**
    * Run all benchmarks in the program.
    */
  private def execBenchmark()(implicit terminal: Terminal): Unit = {
    // Check that the model has been computed.
    if (this.model == null) {
      terminal.writer().println(s"The model has not been computed. Run :solve.")
      return
    }

    // Run all benchmarks.
    Benchmarker.benchmark(this.model, terminal.writer())
  }

  /**
    * Run all unit tests in the program.
    */
  private def execTest()(implicit terminal: Terminal): Unit = {
    // Check that the model has been computed.
    if (this.model == null) {
      terminal.writer().println(s"The model has not been computed. Run :solve.")
      return
    }

    // Run all unit tests.
    val vt = Tester.test(this.model)

    // Print the result to the terminal.
    terminal.writer().print(vt.output.fmt)
  }

  /**
    * Watches source paths for changes.
    */
  private def execWatch()(implicit terminal: Terminal): Unit = {
    // Check if the watcher is already initialized.
    if (this.watcher != null) {
      terminal.writer().println("Already watching for changes.")
      return
    }

    // Compute the set of directories to watch.
    val directories = sourcePaths.map(_.toAbsolutePath.getParent).toList

    // Print debugging information.
    terminal.writer().println("Watching Directories:")
    for (directory <- directories) {
      terminal.writer().println(s"  $directory")
    }

    this.watcher = new WatcherThread(directories)
    this.watcher.start()
  }

  /**
    * Unwatches source paths for changes.
    */
  private def execUnwatch()(implicit terminal: Terminal): Unit = {
    this.watcher.interrupt()
    this.watcher = null
    terminal.writer().println("No longer watching for changes.")
  }

  /**
    * Exits the shell.
    */
  private def execQuit()(implicit terminal: Terminal): Unit = {
    Thread.currentThread().interrupt()
  }

  /**
    * Executes the help command.
    */
  private def execHelp()(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()

    w.println("  Command       Arguments         Purpose")
    w.println()
    w.println("  <expr>                          Evaluates the expression <expr>.")
    w.println("  :type :t      <expr>            Shows the type of <expr>.")
    w.println("  :kind :k      <expr>            Shows the kind of <expr>.")
    w.println("  :effect :e    <expr>            Shows the effect of <expr>.")
    w.println("  :hole         <fqn>             Shows the hole context of <fqn>.")
    w.println("  :browse       <ns>              Shows all entities in <ns>.")
    w.println("  :doc          <fqn>             Shows documentation for <fqn>.")
    w.println("  :search       <needle>          Shows all entities that match <needle>.")
    w.println("  :load         <path>            Adds <path> as a source file.")
    w.println("  :unload       <path>            Removes <path> as a source file.")
    w.println("  :reload :r                      Recompiles every source file.")
    w.println("  :solve                          Computes the least fixed point.")
    w.println("  :rel          <fqn> [needle]    Shows all rows in the relation <fqn> that match <needle>.")
    w.println("  :lat          <fqn> [needle]    Shows all rows in the lattice <fqn> that match <needle>.")
    w.println("  :benchmark                      Run all benchmarks in the program and show the results.")
    w.println("  :test                           Run all unit tests in the program and show the results.")
    w.println("  :watch :w                       Watches all source files for changes.")
    w.println("  :unwatch                        Unwatches all source files for changes.")
    w.println("  :quit :q                        Terminates the Flix shell.")
    w.println("  :help :h :?                     Shows this helpful information.")
    w.println()
  }

  /**
    * Reports unknown command.
    */
  private def execUnknown(s: String)(implicit terminal: Terminal): Unit = {
    terminal.writer().println(s"Unknown command '$s'. Try `:help'.")
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
      case (xs, (s, defn)) if s.namespace == namespace && defn.mod.isPublic && !defn.mod.isSynthetic =>
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
      case (xs, (s, t: Table.Relation)) if s.namespace == namespace =>
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
      case (xs, (s, t: Table.Lattice)) if s.namespace == namespace =>
        t :: xs
      case (xs, _) => xs
    }
  }

  /**
    * Interprets the given string `ns` as a namespace.
    */
  private def getNameSpace(ns: String): List[String] = {
    if (ns == "" || ns == ".") {
      // Case 1: The empty namespace.
      Nil
    } else {
      // Case 2: A (possibly) qualified namespace.
      ns.split("/").toList
    }
  }

  /**
    * Pretty prints the given definition `defn` to the given virtual terminal `vt`.
    */
  private def prettyPrintDef(defn: Def, vt: VirtualTerminal): Unit = {
    vt << Bold("def ") << Blue(defn.sym.name) << "("
    if (defn.fparams.nonEmpty) {
      vt << defn.fparams.head.sym.text << ": " << Cyan(defn.fparams.head.tpe.show)
      for (fparam <- defn.fparams.tail) {
        vt << ", " << fparam.sym.text << ": " << Cyan(fparam.tpe.show)
      }
    }
    vt << "): " << Cyan(defn.tpe.typeArguments.last.show) << NewLine
  }

  /**
    * Pretty prints the given relation `rel` to the given virtual terminal `vt`.
    */
  private def prettyPrintRel(rel: Table.Relation, vt: VirtualTerminal): Unit = {
    vt << Bold("rel ") << Blue(rel.sym.toString) << "("
    vt << rel.attributes.head.name << ": " << Cyan(rel.attributes.head.tpe.show)
    for (attr <- rel.attributes.tail) {
      vt << ", " << attr.name << ": " << Cyan(attr.tpe.show)
    }
    vt << ")" << NewLine
  }

  /**
    * Pretty prints the given lattice `lat` to the given virtual terminal `vt`.
    */
  private def prettyPrintLat(lat: Table.Lattice, vt: VirtualTerminal): Unit = {
    vt << Bold("lat ") << Blue(lat.sym.toString) << "("
    vt << lat.attributes.head.name << ": " << Cyan(lat.attributes.head.tpe.show)
    for (attr <- lat.attributes.tail) {
      vt << ", " << attr.name << ": " << Cyan(attr.tpe.show)
    }
    vt << ")" << NewLine
  }

  /**
    * Pretty prints the holes in the program.
    */
  private def prettyPrintHoles()(implicit terminal: Terminal): Unit = {
    // Print holes, if any.
    val holes = TypedAstOps.holesOf(root)
    val vt = new VirtualTerminal

    // Check if any holes are present.
    if (holes.nonEmpty) {
      vt << Bold("Holes:") << Indent
      // Print each hole and its type.
      for ((sym, ctx) <- holes) {
        vt << NewLine << Blue(sym.toString) << ": " << Cyan(ctx.tpe.show)
      }
      vt << Dedent << NewLine
    }

    // Print the result to the terminal.
    terminal.writer().print(vt.fmt)
  }

  /**
    * A thread to run the fixed point solver in.
    */
  class SolverThread()(implicit terminal: Terminal) extends Runnable {
    override def run(): Unit = {
      // compute the least model.
      val executableRoot = flix.codeGen(root).get
      val timer = new Timer(flix.solve(executableRoot))
      timer.getResult match {
        case Validation.Success(m) =>
          model = m
          if (main.nonEmpty) {
            val name = main.get
            val evalTimer = new Timer(m.evalToString(name))
            terminal.writer().println(s"$name returned `${evalTimer.getResult}' (compile: ${timer.getFormatter.fmt}, execute: ${evalTimer.getFormatter.fmt})")
          }
        case Validation.Failure(errors) =>
          for (error <- errors) {
            terminal.writer().print(error.message.fmt)
          }
      }
    }
  }

  /**
    * A thread to watch over changes in a collection of directories.
    */
  class WatcherThread(paths: List[Path])(implicit terminal: Terminal) extends Thread {
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
            terminal.writer().println()
            terminal.writer().println(s"File: '$changedPath' changed.")
            terminal.flush()
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

