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

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.Ast.HoleContext
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.runtime.verifier.Verifier
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.{Benchmarker, Tester}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.tc.Show
import ca.uwaterloo.flix.util.tc.Show._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualTerminal}
import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.{Terminal, TerminalBuilder}

import scala.jdk.CollectionConverters._
import scala.collection.mutable

class Shell(initialPaths: List[Path], options: Options) {

  private implicit val audience: Audience = Audience.External

  /**
    * The number of warmup iterations.
    */
  private val WarmupIterations = 80

  /**
    * The default color context.
    */
  private implicit val terminalContext: TerminalContext = TerminalContext.AnsiTerminal

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
    * The current compilation result (initialized on startup).
    */
  private var compilationResult: CompilationResult = _

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
      case _: UserInterruptException => // nop, exit gracefully.
      case _: EndOfFileException => // nop, exit gracefully.
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
        |   |  _| | | | | \ \/ /     Enter a command and hit return.
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
    case Command.Run => execRun()
    case Command.Hole(fqnOpt) => execHole(fqnOpt)
    case Command.Browse(ns) => execBrowse(ns)
    case Command.Doc(fqn) => execDoc(fqn)
    case Command.Search(s) => execSearch(s)
    case Command.Reload => execReload()
    case Command.Benchmark => execBenchmark()
    case Command.Test => execTest()
    case Command.Warmup => execWarmup()
    case Command.Watch => execWatch()
    case Command.Unwatch => execUnwatch()
    case Command.Quit => execQuit()
    case Command.Help => execHelp()
    case Command.Praise => execPraise()
    case Command.Unknown(s) => execUnknown(s)
  }

  /**
    * Executes the eval command.
    */
  private def execRun()(implicit terminal: Terminal): Unit = {
    // Recompile the program.
    execReload()

    // Evaluate the main function and get the result.
    val main = Symbol.mkDefnSym("main")
    val result = this.compilationResult.evalToString(main.toString)

    // Write the result to the terminal.
    terminal.writer().println(result)
  }

  /**
    * Shows the hole context of the given `fqn`.
    */
  private def execHole(fqnOpt: Option[String])(implicit terminal: Terminal): Unit = fqnOpt match {
    case None =>
      // Case 1: Print all available holes.
      prettyPrintHoles()
    case Some(fqn) =>
      // Case 2: Print the given hole.

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
            vt << Blue(varSym.text) << ": " << Cyan(FormatType.formatType(varType)) << " " * 6
          }

          // Print the divider.
          vt << NewLine << "-" * 80 << NewLine

          // Print the goal.
          vt << Blue(sym.toString) << ": " << Cyan(FormatType.formatType(holeType)) << NewLine

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

    // Compute the TypedAst and store it.
    this.flix.check() match {
      case Validation.Success(ast) =>
        this.root = ast
        // Pretty print the holes (if any).
        prettyPrintHoles()

        // Generate code.
        flix.codeGen(root) match {
          case Validation.Success(m) =>
            compilationResult = m
          case Validation.Failure(errors) =>
            for (error <- errors) {
              terminal.writer().print(error.message.fmt)
            }
        }
      case Validation.Failure(errors) =>
        terminal.writer().println()
        for (error <- errors) {
          terminal.writer().print(error.message.fmt)
        }
        terminal.writer().println()
        terminal.writer().print(prompt)
        terminal.writer().flush()
    }

  }

  /**
    * Run all benchmarks in the program.
    */
  private def execBenchmark()(implicit terminal: Terminal): Unit = {
    // Run all benchmarks.
    Benchmarker.benchmark(this.compilationResult, terminal.writer())
  }

  /**
    * Run all unit tests in the program.
    */
  private def execTest()(implicit terminal: Terminal): Unit = {
    // Run all unit tests.
    val vt = Tester.test(this.compilationResult)

    // Print the result to the terminal.
    terminal.writer().print(vt.output.fmt)
  }

  /**
    * Warms up the compiler by running it multiple times.
    */
  private def execWarmup()(implicit terminal: Terminal): Unit = {
    val elapsed = mutable.ListBuffer.empty[Duration]
    for (i <- 0 until WarmupIterations) {
      val t = System.nanoTime()
      execReload()
      terminal.writer().print(".")
      terminal.writer().flush()
      val e = System.nanoTime()
      elapsed += new Duration(e - t)
    }
    terminal.writer().println()
    terminal.writer().println(s"Minimum = ${Duration.min(elapsed).fmt}, Maximum = ${Duration.max(elapsed).fmt}, Average = ${Duration.avg(elapsed).fmt})")
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
    w.println("  :run                            Runs the main function.")
    w.println("  :hole         <fqn>             Shows the hole context of <fqn>.")
    w.println("  :browse       <ns>              Shows all entities in <ns>.")
    w.println("  :doc          <fqn>             Shows documentation for <fqn>.")
    w.println("  :search       <needle>          Shows all entities that match <needle>.")
    w.println("  :reload :r                      Recompiles every source file.")
    w.println("  :benchmark                      Run all benchmarks in the program and show the results.")
    w.println("  :test                           Run all unit tests in the program and show the results.")
    w.println("  :warmup                         Warms up the compiler by running it multiple times.")
    w.println("  :watch :w                       Watches all source files for changes.")
    w.println("  :unwatch                        Unwatches all source files for changes.")
    w.println("  :quit :q                        Terminates the Flix shell.")
    w.println("  :help :h :?                     Shows this helpful information.")
    w.println()
  }

  /**
    * Executes the praise command.
    */
  private def execPraise()(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()
    w.print(Toucan.leToucan())
  }

  /**
    * Reports unknown command.
    */
  private def execUnknown(s: String)(implicit terminal: Terminal): Unit = {
    terminal.writer().println(s"Unknown command '$s'. Try `:run` or `:help'.")
  }

  /**
    * Returns the namespaces in the given AST `root`.
    */
  private def namespacesOf(root: Root): Set[String] = root.defs.keySet.map(_.namespace.mkString("/"))

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
      vt << defn.fparams.head.sym.text << ": " << Cyan(FormatType.formatType(defn.fparams.head.tpe))
      for (fparam <- defn.fparams.tail) {
        vt << ", " << fparam.sym.text << ": " << Cyan(FormatType.formatType(fparam.tpe))
      }
    }
    vt << "): " << Cyan(FormatType.formatType(defn.inferredScheme.base.typeArguments.last)) << NewLine
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
        vt << NewLine << Blue(sym.toString) << ": " << Cyan(FormatType.formatType(ctx.tpe))
      }
      vt << Dedent << NewLine
    }

    // Print the result to the terminal.
    terminal.writer().print(vt.fmt)
  }

  /**
    * A thread to watch over changes in a collection of directories.
    */
  class WatcherThread(paths: List[Path])(implicit terminal: Terminal) extends Thread {

    /**
      * The minimum amount of time between runs of the compiler.
      */
    private val Delay: Long = 1000 * 1000 * 1000

    // Initialize a new watcher service.
    val watchService: WatchService = FileSystems.getDefault.newWatchService

    // Register each directory.
    for (path <- paths) {
      if (Files.isDirectory(path)) {
        path.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY)
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
        val changed = mutable.ListBuffer.empty[Path]
        for (event <- watchKey.pollEvents().asScala) {
          // Check if a file with ".flix" extension changed.
          val changedPath = event.context().asInstanceOf[Path]
          if (changedPath.toString.endsWith(".flix")) {
            changed += changedPath
          }
        }

        if (changed.nonEmpty) {
          // Print information to the user.
          terminal.writer().println()
          terminal.writer().println(s"Recompiling. File(s) changed: ${changed.mkString(", ")}")
          terminal.writer().print(prompt)
          terminal.writer().flush()

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

        }

        // Reset the watch key.
        watchKey.reset()
      }
    } catch {
      case _: InterruptedException => // nop, shutdown.
    }

  }

}

