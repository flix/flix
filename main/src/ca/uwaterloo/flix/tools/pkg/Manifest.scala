/*
 * Copyright 2023 Magnus Madsen
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, Repository, SecurityContext}
import ca.uwaterloo.flix.tools.pkg.Dependency.{FlixDependency, JarDependency, MavenDependency}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import org.tomlj.Toml

case class Manifest(version: SemVer,
                    repository: Option[GitHub.Project],
                    flix: SemVer,
                    dependencies: List[Dependency]) {
  def flixDependencies: List[Dependency.FlixDependency] = dependencies.collect { case dep: Dependency.FlixDependency => dep }

  /**
    * Returns how this package is named in a message.
    *
    * A package is named by the repository it is published as, and by nothing else: that is what
    * a dependent writes to declare it, and what its release assets are found under. A package
    * that declares no repository cannot be addressed, and so has no name to give.
    */
  def displayName: String = repository.map(_.toString).getOrElse(Manifest.Unnamed)

  /**
    * Returns the package this manifest declares itself to be, if it declares a repository.
    */
  def packageId: Option[PackageId] = repository.map(p => PackageId(Repository.GitHub, p.owner, p.repo))

  /**
    * Returns the mount table of this manifest: the name of each mount to the identifier of the
    * dependency it names.
    */
  def mounts: Map[Mountpoint, PackageId] =
    flixDependencies.map(dep => dep.mount -> dep.id).toMap

  def mavenDependencies: List[Dependency.MavenDependency] = dependencies.collect { case dep: Dependency.MavenDependency => dep }

  def jarDependencies: List[Dependency.JarDependency] = dependencies.collect { case dep: Dependency.JarDependency => dep }
}

object Manifest {

  /** How a package that declares no repository is named in a message. */
  val Unnamed: String = "<unnamed>"

  /** The keys that TOML reads as they are written, i.e., without quotes. */
  private val BareKey = "[A-Za-z0-9_-]+".r

  /**
    * Returns `manifest` as the text of a `flix.toml` file.
    *
    * The text depends only on `manifest`, and neither on the platform nor on the text that
    * `manifest` was parsed from:
    *
    *   - The tables come in a fixed order, and a table that declares nothing is left out.
    *   - The dependencies of a table are sorted by key.
    *   - The `=` of the entries of a table are aligned.
    *   - A Flix dependency is written in the [[DependencyStyle]] it was declared in.
    *   - Every line ends in `\n`.
    *
    * Parsing the text gives back `manifest`, up to the order of its dependencies.
    */
  def format(manifest: Manifest): String = {
    val tables = List(
      packageTable(manifest),
      dependencyTable("dependencies", manifest.flixDependencies.map(flixDependencyEntry)),
      dependencyTable("mvn-dependencies", manifest.mavenDependencies.map(mavenDependencyEntry)),
      dependencyTable("jar-dependencies", manifest.jarDependencies.map(jarDependencyEntry))
    )

    val sb = new StringBuilder
    // A table that declares nothing is left out rather than written empty.
    for (table <- tables if table.entries.nonEmpty) {
      // The tables are separated by a blank line.
      if (sb.nonEmpty) sb.append('\n')
      appendTable(sb, table)
    }
    sb.toString
  }

  /** Returns the `[package]` table of `manifest`. */
  private def packageTable(manifest: Manifest): Table = {
    val version = Entry("version", Value.Str(manifest.version.toString))
    val repository = manifest.repository.map(proj => Entry("repository", Value.Str(s"github:$proj")))
    val flix = Entry("flix", Value.Str(manifest.flix.toString))
    Table("package", version :: repository.toList ::: List(flix))
  }

  /**
    * Returns the table `name` of the dependencies `entries`, sorted by key.
    *
    * Where a dependency is written depends only on what it is, and not on when it was added.
    */
  private def dependencyTable(name: String, entries: List[Entry]): Table =
    Table(name, entries.sortBy(_.key))

  /**
    * Returns the entry of `dep` in the `[dependencies]` table.
    *
    * A dependency is written in the style it was declared in, but as its version only while that
    * is all it declares. A table spells out the mount only when it is not the one derived from
    * the name of the repository, and the security context only when it is not the default.
    */
  private def flixDependencyEntry(dep: FlixDependency): Entry = {
    // A derived mount is not written, so that it is derived again.
    val mount = Option.when(!Mountpoint.ofRepoName(dep.id).contains(dep.mount))(dep.mount)
    dep match {
      case FlixDependency(id, version, _, SecurityContext.Default, DependencyStyle.VersionOnly) if mount.isEmpty =>
        Entry(id.toString, Value.Str(version.toString))

      case FlixDependency(id, version, _, sctx, _) =>
        val versionEntry = Entry("version", Value.Str(version.toString))
        val mountEntry = mount.map(m => Entry("mount", Value.Str(m.toString)))
        val securityEntry = Option.when(sctx != SecurityContext.Default)(Entry("security", Value.Str(sctx.toString)))
        Entry(id.toString, Value.InlineTable(versionEntry :: mountEntry.toList ::: securityEntry.toList))
    }
  }

  /** Returns the entry of `dep` in the `[mvn-dependencies]` table. */
  private def mavenDependencyEntry(dep: MavenDependency): Entry =
    Entry(dep.identifier, Value.Str(dep.versionTag))

  /** Returns the entry of `dep` in the `[jar-dependencies]` table. */
  private def jarDependencyEntry(dep: JarDependency): Entry =
    Entry(dep.identifier, Value.Str(s"url:${dep.url}"))

  /** Appends `table` to `sb`: its header, and then one line per entry. */
  private def appendTable(sb: StringBuilder, table: Table): Unit = {
    sb.append('[').append(key(table.name)).append(']').append('\n')
    // The keys are aligned by how they are written, since a quoted key is wider than it reads.
    val width = table.entries.map(entry => key(entry.key).length).maxOption.getOrElse(0)
    for (entry <- table.entries) {
      appendEntry(sb, entry, width)
      sb.append('\n')
    }
  }

  /** Appends `entry` to `sb` as `key = value`, where the key is padded to `width` characters. */
  private def appendEntry(sb: StringBuilder, entry: Entry, width: Int): Unit = {
    val k = key(entry.key)
    sb.append(k).append(" " * (width - k.length)).append(" = ")
    appendValue(sb, entry.value)
  }

  /** Appends `value` to `sb`. */
  private def appendValue(sb: StringBuilder, value: Value): Unit = value match {
    case Value.Str(s) =>
      sb.append(quote(s))

    case Value.InlineTable(entries) =>
      sb.append("{ ")
      for ((entry, i) <- entries.zipWithIndex) {
        if (i > 0) sb.append(", ")
        appendEntry(sb, entry, width = 0)
      }
      sb.append(" }")
  }

  /**
    * Returns `k` as TOML reads it back as one key.
    *
    * A key is quoted only when it must be: a package identifier holds `:` and `/`, and the name
    * of a jar holds `.`, none of which TOML reads as part of a bare key.
    */
  private def key(k: String): String =
    if (BareKey.matches(k)) k else quote(k)

  /**
    * Returns `s` as a TOML basic string: in quotes, and with `"`, `\`, and every character that
    * is not printable ASCII escaped.
    */
  private def quote(s: String): String =
    "\"" + Toml.tomlEscape(s) + "\""

  /** A table: the header `[name]` followed by one line per entry. */
  private case class Table(name: String, entries: List[Entry])

  /** The pair `key = value`. The key is as it reads, and is quoted when it is written. */
  private case class Entry(key: String, value: Value)

  /** The value of an [[Entry]]. */
  private sealed trait Value

  private object Value {

    /** A string, e.g. `"1.4.0"`. The string is as it reads, and is escaped when it is written. */
    case class Str(s: String) extends Value

    /** An inline table, e.g. `{ version = "1.4.0", mount = "Museum" }`. */
    case class InlineTable(entries: List[Entry]) extends Value

  }

}
