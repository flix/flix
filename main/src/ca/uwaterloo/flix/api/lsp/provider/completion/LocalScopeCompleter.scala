/*
 * Copyright 2024 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
    scp.scp.m.foldLeft(List.empty[Completion]) { case (acc, (name, resolutions)) =>
      acc ++ mkJavaClassCompletion(name, resolutions, range) ++ mkVarCompletion(name, resolutions, range) ++ mkLocalDefCompletion(resolutions, range)
    }

  /**
    * Returns a list of completions for UndefinedType.
    * We will provide completions for Resolution.Declaration and Resolution.JavaClass
    */
  def getCompletionsType(range: Range, scp: LocalScope): Iterable[Completion] =
    scp.scp.m.foldLeft(List.empty[Completion]) { case (acc, (name, resolutions)) =>
      acc ++ mkJavaClassCompletion(name, resolutions, range)
    }

  /**
    * Tries to create a JavaClassCompletion for the given name and resolutions.
    */
  private def mkJavaClassCompletion(name: String, resolutions: List[Resolution], range: Range): Iterable[Completion] = {
    resolutions.collect {
      case Resolution.JavaClass(clazz) => Completion.LocalJavaClassCompletion(name, clazz.desc, range, Priority.High(0))
    }
  }

  /**
    * Tries to create a VarCompletion for the given name and resolutions.
    */
  private def mkVarCompletion(name: String, resolutions: List[Resolution], range: Range): Iterable[Completion] = {
    if (resolutions.exists {
      case Resolution.Var(_) => true
      case _ => false
    }) Completion.LocalVarCompletion(name, range, Priority.High(0)) :: Nil else Nil
  }

  /**
    * Tries to create a LocalDefCompletion for the given name and resolutions.
    */
  private def mkLocalDefCompletion(resolutions: List[Resolution], range: Range): Iterable[Completion] =
    resolutions.collect {
      case Resolution.LocalDef(_, sym, fparams) => Completion.LocalDefCompletion(sym, fparams, range, Priority.High(0))
    }

}
