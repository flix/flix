/*
 * Copyright 2023 Lukas Rønn
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
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumTagCompletion
import ca.uwaterloo.flix.language.ast.Symbol.{CaseSym, EnumSym}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypeConstructor, TypedAst}

object EnumTagCompleter extends Completer {

  /**
    * Returns a List of Completion for enum tags.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[EnumTagCompletion] = {
    // We don't know if the user has provided a tag, so we have to try both cases
    val fqn = context.word.split('.').toList

    getEnumTagCompletionsWithoutTag(fqn) ++
      getEnumTagCompletionsWithTag(fqn)
  }

  /**
    * Gets completions for enum tags without tag provided
    */
  private def getEnumTagCompletionsWithoutTag(fqn: List[String])(implicit root: TypedAst.Root): Iterable[EnumTagCompletion] = {
    val enumSym = mkEnumSym(fqn)
    root.enums.get(enumSym) match {
      case None => // Case 1: Enum does not exist.
        Nil
      case Some(enm) => // Case 2: Enum does exist -> Get cases.
        enm.cases.map {
          case (_, cas) => EnumTagCompletion(enumSym, cas, cas.tpes.length)
        }
    }
  }

  /**
    * Gets completions for enum tags with tag provided
    */
  private def getEnumTagCompletionsWithTag(fqn: List[String])(implicit root: TypedAst.Root): Iterable[EnumTagCompletion] = {
    // The user has provided a tag
    // We know that there is at least one dot, so we split the context.word and
    // make a fqn from the everything but the the last (hence it could be the tag).
    if (fqn.isEmpty) {
      return Nil
    }

    val fqnWithTag = fqn.dropRight(1)
    val tag = fqn.takeRight(1).mkString
    val enumSym = mkEnumSym(fqnWithTag)

    root.enums.get(enumSym) match {
      case None => // Case 1: Enum does not exist.
        Nil
      case Some(enm) => // Case 2: Enum does exist
        enm.cases.flatMap {
          case (caseSym, cas) =>
            if (matchesTag(caseSym, tag)) {
              // Case 2.1: Tag provided and it matches the case
              Some(EnumTagCompletion(enumSym, cas, cas.tpes.length))
            } else {
              // Case 2.2: Tag provided doesn't match the case
              None
            }
        }
    }
  }

  /**
    * Checks if the provided tag matches the case.
    *
    * @param caseSym the caseSym of the case from the enum.
    * @param tag     the provided tag.
    * @return        true, if the provided tag matches the case, false otherwise.
    */
  private def matchesTag(caseSym: CaseSym, tag: String): Boolean = caseSym.name.startsWith(tag)

  /**
    * Generates an enumSym with a correct nameSpace. This is different from the one in Symbol.scala.
    *
    * Symbol.mkEnumSym("A.B.C.Color") would make an enumSym with namespace=List("A"), and name = "B.C.Color"
    *
    * This function: mkEnumSym(List["A","B","C","Color"]) makes an enumSym with namespace=List("A","B","C") and name = "Color"
    *
    * @param fqn the fully qualified name as a List[String].
    * @return    the enum symbol for the given fully qualified name.
    */
  private def mkEnumSym(fqn: List[String]): EnumSym = {
    if (fqn.isEmpty) {
      new EnumSym(Nil, "", SourceLocation.Unknown)
    } else {
      val ns = fqn.dropRight(1)
      val name = fqn.takeRight(1).mkString
      new EnumSym(ns, name, SourceLocation.Unknown)
    }
  }
}
