/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{SecurityContext, SourceName}
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.{Options, Result}

import java.io.PrintStream
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

/**
  * The Flix project a language server serves.
  *
  * The project is loaded with [[Bootstrap]], so that a language server sees what the command line
  * sees: the source files of the project directory, and the packages and JARs its `flix.toml`
  * declares. The packages and JARs are fixed for the lifetime of a Flix instance, so a change to
  * them is applied by loading the project again and replacing the instance.
  *
  * The client owns the documents it has open: a document added with [[addSource]] shadows the file
  * of the same name on disk, so that unsaved changes are what is compiled.
  */
class LspProject(o: Options) {

  /**
    * The stream the progress and errors of the project loading are written to.
    *
    * Never stdout: a language server may speak its protocol over stdout.
    */
  private val out: PrintStream = System.err

  /**
    * The workspace root the client has added, if it has added one.
    */
  private var root: Option[Path] = None

  /**
    * The contents of the documents the client has open, by source name.
    */
  private val buffers: mutable.Map[SourceName, String] = mutable.Map.empty

  /**
    * The project, if it was loaded successfully.
    */
  private var bootstrap: Option[Bootstrap] = None

  /**
    * Whether the project must be loaded again before the next check.
    */
  private var stale: Boolean = true

  /**
    * The Flix instance (the same instance is used for incremental compilation).
    *
    * Replaced whenever the project is loaded successfully, since the packages and JARs are fixed
    * for the lifetime of an instance. Until then, an instance without any dependencies, so that the
    * documents the client has open can be compiled before the project is loaded.
    */
  private var flix: Flix = new Flix().setFormatter(NoFormatter).setOptions(o)

  /**
    * Returns the Flix instance of the project.
    */
  def compiler: Flix = flix

  /**
    * Returns the path of the project: the workspace root the client has added, or the working
    * directory of the server if it has added none.
    */
  def projectPath: Path = root.getOrElse(LspProject.WorkingDirectory)

  /**
    * Adds `path` as the workspace root.
    *
    * Only the first root is kept: [[Bootstrap]] loads a single project.
    */
  def addWorkspace(path: Path): Unit = {
    val newRoot = path.toAbsolutePath.normalize()
    root match {
      case Some(currentRoot) =>
        if (currentRoot != newRoot) {
          out.println(s"Ignoring the workspace root '$newRoot'. The project is '$currentRoot'.")
        }
      case None =>
        val previous = projectPath
        root = Some(newRoot)
        if (newRoot != previous) {
          // The project moved: load it again before the next check.
          stale = true
        }
    }
  }

  /**
    * Records that the packages or JARs of the project changed, so that it is loaded again before
    * the next check.
    */
  def markDependenciesChanged(): Unit = {
    stale = true
  }

  /**
    * Loads the project again and replaces the Flix instance with a fresh one.
    *
    * Returns the error if the project could not be loaded.
    */
  def restart(): Option[BootstrapError] = reload()

  /**
    * Adds the document `src` under `name`, shadowing the file of the same name on disk.
    */
  def addSource(name: SourceName, src: String): Unit = {
    buffers += (name -> src)
    ClientUri.addSource(flix, name, src)
  }

  /**
    * Returns `true` if the client has added the document named `name`.
    */
  def isOpen(name: SourceName): Boolean = buffers.contains(name)

  /**
    * Removes the document named `name`.
    *
    * A document that is a source file of the project goes back to its contents on disk: the client
    * no longer owns it, but it is still part of the project.
    */
  def remSource(name: SourceName): Unit = {
    buffers -= name
    name match {
      case SourceName.PathName(path) if isProjectSource(path) && Files.isRegularFile(path) =>
        flix.addFile(path, SecurityContext.Unrestricted)
      case _ =>
        ClientUri.remSource(flix, name)
    }
  }

  /**
    * Type checks the project, loading it again first if its packages or JARs changed.
    */
  def check(): (Option[Root], List[CompilationMessage]) = {
    if (stale) {
      // The project is loaded at most once per check: a project that cannot be loaded is reported
      // once, and the previous one keeps being compiled until the client asks for a restart.
      reload().foreach(err => out.println(err.message(NoFormatter)))
    }
    flix.check()
  }

  /**
    * Returns the names of the sources of the project: its source files on disk together with the
    * documents the client has open.
    */
  def sourceNames: Set[SourceName] = projectSources.map(SourceName.PathName.apply).toSet ++ buffers.keySet

  /**
    * Releases the resources held by the Flix instance.
    */
  def close(): Unit = flix.close()

  /**
    * Loads the project and replaces the Flix instance with one for it.
    *
    * Requires network access: the dependencies the manifest declares are resolved and installed.
    *
    * Returns the error if the project could not be loaded. The project then keeps the Flix instance
    * it was last loaded with, so that an unreadable manifest does not empty an open editor, and the
    * documents the client has open are compiled as they were before.
    */
  private def reload(): Option[BootstrapError] = {
    stale = false
    out.println(s"Loading the project '$projectPath'.")
    Bootstrap.bootstrap(projectPath, o.githubToken)(NoFormatter, out) match {
      case Result.Ok(b) =>
        bootstrap = Some(b)
        replaceFlix(b.mkFlix(o, NoFormatter))
        None
      case Result.Err(err) =>
        Some(err)
    }
  }

  /**
    * Replaces the Flix instance with `newFlix` and closes the previous instance.
    */
  private def replaceFlix(newFlix: Flix): Unit = {
    val oldFlix = flix
    flix = newFlix
    // The documents the client has open shadow the files that were read from disk.
    for ((name, src) <- buffers) {
      ClientUri.addSource(flix, name, src)
    }
    oldFlix.close()
  }

  /**
    * Returns the source files of the project, or `Nil` if it has not been loaded.
    */
  private def projectSources: List[Path] = bootstrap match {
    case Some(b) => b.projectFiles.sources.map(_.normalize())
    case None => Nil
  }

  /**
    * Returns `true` if `path` is a source file of the project.
    */
  private def isProjectSource(path: Path): Boolean = projectSources.contains(path.normalize())

}

object LspProject {

  /**
    * The working directory of the server, used as the project when the client adds no workspace root.
    */
  private val WorkingDirectory: Path = Paths.get(".").toAbsolutePath.normalize()

}
