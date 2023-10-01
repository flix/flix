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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.NewCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object NewCompleter extends Completer {
  def makeCompletion(text: String, context: CompletionContext): List[NewCompletion] = {
    NewCompletion(
        name = text,
        priority = Priority.normal("new completion"),
        textEdit = TextEdit(context.range, text),
        InsertTextFormat.Snippet
    ) :: Nil
  }
  def flixType(javaType: String): String = {
    if (javaType == "void") {
        "Unit"
    } else if (javaType.contains(".")) {
      // Not sure why this happens, but sometimes there is an extraneous [L in front of the type
      if (javaType.startsWith("[L")) {
        "##" + javaType.substring(2).replace(";", "")
      } else {
        "##" + javaType.replace(";", "")
      }
    } else {
        javaType
    }
  }
  def methodString(method: java.lang.reflect.Method, interfaceName: String, methodNum: Int): String = {
    val sb = new StringBuilder
    sb.append("def ")
    sb.append(method.getName)
    sb.append("(")
    val params = method.getParameters
    sb.append("_this: ##")
    sb.append(interfaceName)
    for (i <- 0 until params.length) {
        sb.append(", ")
        sb.append(params(i).getName)
        sb.append(": ")
        sb.append(flixType(params(i).getType.getName))
    }
    sb.append(")")
    val retType = method.getReturnType.getName
    sb.append(": ")
    sb.append(flixType(retType))
    sb.append(" \\ IO = ${" + methodNum + ":/* TODO */}")
    return sb.toString
  }
  def countLeadingSpaces(s: String): Int = {
    var i = 0
    while (i < s.length && s.charAt(i) == ' ') {
        i += 1
    }
    return i
  }
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[NewCompletion] = {
    val interfaceName = context.previousWord
    try {
        val cls = Class.forName(interfaceName)
        val methodStrings = cls.getMethods.zipWithIndex.map { case (method, idx) => methodString(method, interfaceName, idx + 1) }
        val indent = " " * countLeadingSpaces(context.prefix)
        val methodsString = "{" + methodStrings.map("\n" + indent + "    " + _).mkString("") + "\n" + indent + "}\n"
        return makeCompletion(methodsString, context)
    } catch {
        case e: ClassNotFoundException => return Nil
        case e: NoClassDefFoundError => return Nil
    }
  }
}
