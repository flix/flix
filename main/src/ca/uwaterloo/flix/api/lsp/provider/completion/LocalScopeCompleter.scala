/*
 * Copyright 2024 Chenhao Gao
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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.language.ast.shared.{LocalScope, Resolution}

/**
  * Provides completions for items in local scope, including:
  *   - Resolution.Declaration: functions, structs, enums, etc.
  *   - Resolution.JavaClass: java classes.
  *   - Resolution.Var: local variables, arguments.
  *   - Resolution.LocalDef: local definitions.
  */
object LocalScopeCompleter {
  /**
    * Returns a list of completions for UndefinedName.
    * We will provide all sorts of completions except for Resolution.TypeVar
    */
  def getCompletionsExpr(range: Range, scp: LocalScope): Iterable[Completion] =
   scp.scp.m.foldLeft(List.empty[Completion]){case (acc, (name, resolutions)) =>
      acc ++ mkJavaClassCompletion(name, resolutions, range) ++ mkVarCompletion(name, resolutions, range) ++ mkLocalDefCompletion(resolutions, range)
   }

  /**
    * Returns a list of completions for UndefinedType.
    * We will provide completions for Resolution.Declaration and Resolution.JavaClass
    */
  def getCompletionsType(range: Range, scp: LocalScope): Iterable[Completion] =
    scp.scp.m.foldLeft(List.empty[Completion]){case (acc, (name, resolutions)) =>
       acc ++ mkJavaClassCompletion(name, resolutions, range)
    }

  /**
    * Tries to create a JavaClassCompletion for the given name and resolutions.
    */
  private def mkJavaClassCompletion(name: String, resolutions: List[Resolution], range: Range): Iterable[Completion] = {
    resolutions.collect {
      case Resolution.JavaClass(clazz) => Completion.LocalJavaClassCompletion(name, clazz, range)
    }
  }

  /**
    * Tries to create a VarCompletion for the given name and resolutions.
    */
  private def mkVarCompletion(name: String, resolutions: List[Resolution], range: Range): Iterable[Completion] = {
    if (resolutions.exists{
      case Resolution.Var(_) => true
      case _ => false
    }) Completion.LocalVarCompletion(name, range) :: Nil else Nil
  }

  /**
    * Tries to create a LocalDefCompletion for the given name and resolutions.
    */
  private def mkLocalDefCompletion(resolutions: List[Resolution], range: Range): Iterable[Completion] =
    resolutions.collect{ case Resolution.LocalDef(sym, fparams) => Completion.LocalDefCompletion(sym, fparams, range)}

}
