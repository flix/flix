/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.MethodCompletion
import ca.uwaterloo.flix.language.ast.{Name, Type}
import ca.uwaterloo.flix.language.jvm.JavaMemberResolver
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaTypes

object InvokeMethodCompleter {

  def getCompletions(obj: Type, name: Name.Ident)(implicit flix: Flix): Iterable[MethodCompletion] = {
    JavaTypes.descriptorOf(obj) match {
      case None =>
        Nil
      case Some(desc) =>
        JavaMemberResolver.instanceMethods(desc).toOption.getOrElse(Nil).map(MethodCompletion(name, Priority.Lowest(0), _))
    }
  }

}
