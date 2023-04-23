/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Index, InsertTextFormat, TextEdit}
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.Priority
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.TypeBuiltinCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object TypeBuiltinCompleter extends Completer {

  /* This list is manually maintained. If a new built-in type is added, it must be extended.
   * Built-in types are typically described in TypeConstructor, Namer and Resolver.
   */
  private val BuiltinTypeNames: List[String] = List(
    "Unit",
    "Bool",
    "Char",
    "Float64",
    "BigDecimal",
    "Int32",
    "Int64",
    "BigInt",
    "String"
  )

  /* Built-in types with hardcoded low priority */
  private val LowPriorityBuiltinTypeNames: List[String] = List(
    "Int8",
    "Int16",
    "Float32"
  )

  /* Built-in types with type parameters */
  private val BuiltinTypeNamesWithTypeParameters: List[(String, List[String])] = List(
    ("Array", List("a", "r")),
    ("Vector", List("a")),
    ("Ref", List("a", "r")),
    ("Sender", List("t", "r")),
    ("Receiver", List("t", "r")),
    ("Lazy", List("t"))
  )

  /**
    * Returns a List of Completion for builtin types.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[TypeBuiltinCompletion] = {
    val builtinTypes = BuiltinTypeNames.map { name =>
      val internalPriority = Priority.high _
      Completion.TypeBuiltinCompletion(name, TypeCompleter.priorityBoostForTypes(internalPriority(name))(context), TextEdit(context.range, name),
        InsertTextFormat.PlainText)
    }

    val lowPriorityBuiltinTypes = LowPriorityBuiltinTypeNames.map { name =>
      val internalPriority = Priority.low _
      Completion.TypeBuiltinCompletion(name, TypeCompleter.priorityBoostForTypes(internalPriority(name))(context), TextEdit(context.range, name),
        InsertTextFormat.PlainText)
    }

    val builtinTypesWithParams = BuiltinTypeNamesWithTypeParameters.map {
      case (name, tparams) =>
        val internalPriority = Priority.boost _
        val fmtTparams = tparams.zipWithIndex.map { case (name, idx) => s"$${${idx + 1}:$name}" }.mkString(", ")
        val finalName = s"$name[${tparams.mkString(", ")}]"
        Completion.TypeBuiltinCompletion(finalName, TypeCompleter.priorityBoostForTypes(internalPriority(name))(context),
          TextEdit(context.range, s"$name[$fmtTparams]"), insertTextFormat = InsertTextFormat.Snippet)
    }

    builtinTypes ++ lowPriorityBuiltinTypes ++ builtinTypesWithParams
  }
}
