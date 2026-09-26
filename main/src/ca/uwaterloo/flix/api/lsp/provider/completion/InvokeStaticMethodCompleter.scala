/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.MethodCompletion
import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.jvm.JavaMemberResolver

import java.lang.constant.ClassDesc

object InvokeStaticMethodCompleter {

  def getCompletions(clazz: ClassDesc, field: Name.Ident)(implicit flix: Flix): List[Completion] = {
    JavaMemberResolver.staticMethods(clazz).toOption.getOrElse(Nil).map(MethodCompletion(field, Priority.Lowest(0), _))
  }

}
