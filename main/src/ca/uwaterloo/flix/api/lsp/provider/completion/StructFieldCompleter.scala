/*
 * Copyright 2024 Joseph Tan
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.ast.TypedAst

object StructFieldCompleter {
  def getCompletions(e: ResolutionError.UndefinedStructField, root: TypedAst.Root): Iterable[Completion.StructFieldCompletion] = {
    val fields = root.structs.values.flatMap(struct => struct.fields.values)
    val completions0 = fields.filter(_.sym.name.startsWith(e.field.name))
    val completions = e.struct match {
      case Some(sym) => completions0.filter(_.sym.structSym == sym)
      case None => completions0
    }
    completions.map(field => Completion.StructFieldCompletion(field.sym.name, e.field.loc, field.tpe, Priority.Lowest(0)))
  }
}
