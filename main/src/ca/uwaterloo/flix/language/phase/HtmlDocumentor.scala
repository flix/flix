/*
 * Copyright 2023 Holger Dal Mogensen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, TypedAst}

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable

/**
  * A phase that emits a JSON file for library documentation.
  */
object HtmlDocumentor {

  /**
    * The "Pseudo-name" of the root namespace.
    */
  val RootNS: String = "Prelude"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./build/doc")

  def run(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = flix.phase("HtmlDocumentor") {
    //
    // Determine whether to generate documentation.
    //
    if (!flix.options.documentorHtml) {
      return root
    }

    val modules = splitModules(root)
    val outputs = modules.par.map(mod => (mod, documentModule(mod)))
    outputs.foreach {
      case (mod, out) => writeFile(mod, out)
    }

    root
  }

  private def splitModules(root: TypedAst.Root): Iterable[Module] = root.modules.map {
    case (sym, mod) =>
      val namespace = sym.ns
      val uses = root.uses.getOrElse(sym, List.empty)

      var submodules: List[Symbol.ModuleSym] = List.empty
      var classes: Map[Symbol.ClassSym, TypedAst.Class] = Map.empty
      var enums: Map[Symbol.EnumSym, TypedAst.Enum] = Map.empty
      var restrictableEnums: Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum] = Map.empty
      var effects: Map[Symbol.EffectSym, TypedAst.Effect] = Map.empty
      var typeAliases: Map[Symbol.TypeAliasSym, TypedAst.TypeAlias] = Map.empty
      var defs: Map[Symbol.DefnSym, TypedAst.Def] = Map.empty
      mod.foreach {
        case sym: Symbol.ModuleSym => submodules = sym :: submodules
        case sym: Symbol.ClassSym => classes += sym -> root.classes(sym)
        case sym: Symbol.EnumSym => enums += sym -> root.enums(sym)
        case sym: Symbol.RestrictableEnumSym => restrictableEnums += sym -> root.restrictableEnums(sym)
        case sym: Symbol.EffectSym => effects += sym -> root.effects(sym)
        case sym: Symbol.TypeAliasSym => typeAliases += sym -> root.typeAliases(sym)
        case sym: Symbol.DefnSym => defs += sym -> root.defs(sym)
        case _ => // No op
      }

      Module(
        namespace,
        uses,
        submodules,
        classes,
        enums,
        restrictableEnums,
        effects,
        typeAliases,
        defs,
      )
  }

  private def documentModule(mod: Module): String = {
    // Generate content here
    mod.namespace.mkString(".")
  }

  private def writeFile(mod: Module, output: String): Unit = {
    val name = if (mod.namespace.isEmpty) List(RootNS) else mod.namespace
    val path = OutputDirectory.resolve(s"${name.mkString(".")}.html")

    try {
      Files.createDirectories(OutputDirectory)
      val writer = Files.newBufferedWriter(path)
      writer.write(output)
      writer.close()
    } catch {
      case ex: IOException => throw new RuntimeException(s"Unable to write to path '$path'.", ex)
    }
  }

  private case class Module(namespace: List[String],
                            uses: List[Ast.UseOrImport],
                            submodules: List[Symbol.ModuleSym],
                            classes: Map[Symbol.ClassSym, TypedAst.Class],
                            enums: Map[Symbol.EnumSym, TypedAst.Enum],
                            restrictableEnums: Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum],
                            effects: Map[Symbol.EffectSym, TypedAst.Effect],
                            typeAliases: Map[Symbol.TypeAliasSym, TypedAst.TypeAlias],
                            defs: Map[Symbol.DefnSym, TypedAst.Def])
}
