/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FieldCompletion
import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.jvm.JavaMemberResolver

import java.lang.constant.ClassDesc

object GetStaticFieldCompleter {

  def getCompletions(clazz: ClassDesc, field: Name.Ident)(implicit flix: Flix): List[Completion] = {
    val fields = JavaMemberResolver.fields(clazz).toOption.getOrElse(Nil)
    fields.filter(f => f.isStatic).map(FieldCompletion(field, Priority.Lowest(0), _))
  }

}
