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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ImportCompletion
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.AnchorPosition
import ca.uwaterloo.flix.language.errors.ResolutionError

object ImportCompleter {

  /**
    * Returns a list of import completions to auto complete the class name and import the java class.
    *
    * Example:
    *  If we have an undefined name which is the prefix of an existing java class
    *
    *  {{{
    *    let s = Mat // undefined name error
    *  }}}
    *
    *  We propose to complete the name to `Math` and import the class `java.lang.Math`
    *
    *  {{{
    *    import java.lang.Math
    *    ...
    *    let s = Math
    *  }}}
    */
  def getCompletions(err: ResolutionError.UndefinedName)(implicit root: Root): Iterable[ImportCompletion] =
    javaClassCompletionsFromPrefix(err.qn.ident.name, err.ap)

  def getCompletions(err: ResolutionError.UndefinedType)(implicit root: Root): Iterable[ImportCompletion] =
    javaClassCompletionsFromPrefix(err.qn.ident.name, err.ap)

  def getCompletions(err: ResolutionError.UndefinedJvmClass)(implicit root: Root): Iterable[ImportCompletion] =
    javaClassCompletionsFromPrefix(err.name, err.ap)

  /**
    * Gets completions from a java class prefix.
    */
  private def javaClassCompletionsFromPrefix(prefix: String, ap: AnchorPosition)(implicit root: Root): Iterable[ImportCompletion] = {
    val availableClasses = root.availableClasses.byClass.m
    availableClasses.keys.filter(_.startsWith(prefix)).flatMap { className =>
      availableClasses(className).map{path =>
        val completePath = path.mkString(".") + "." + className
        ImportCompletion(className, completePath, ap, completePath)
      }
    }
  }
}
