/*
 * Copyright 2023 Magnus Madsen, Lukas Rønn
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
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.{Priority, convertJavaClassToFlixType}
import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, InsertTextFormat, TextEdit}
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.language.ast.Symbol.{EnumSym, TypeAliasSym}

import java.lang.reflect.{Constructor, Executable, Field, Method}

/**
  * A common super-type for auto-completions.
  */
sealed trait Completion {
  /**
    * Returns a LSP completion item for `this`.
    */
  def toCompletionItem: CompletionItem = this match {
    case Completion.KeywordCompletion(name, context) =>
      CompletionItem(label = name, sortText = Priority.normal(name), textEdit = TextEdit(context.range, s"$name "),
        kind = CompletionItemKind.Keyword)
    case Completion.FieldCompletion(name, context) =>
      CompletionItem(label = name, sortText = Priority.high(name), textEdit = TextEdit(context.range, s"$name "),
        kind = CompletionItemKind.Variable)
    case Completion.PredicateCompletion(name, priority, context) =>
      CompletionItem(label = name, sortText = priority, textEdit = TextEdit(context.range, s"$name "),
        kind = CompletionItemKind.Variable)
    case Completion.TypeBuiltinCompletion(name, priority, textEdit, insertTextFormat) =>
      CompletionItem(label = name, sortText = priority, textEdit = textEdit, insertTextFormat = insertTextFormat,
        kind = CompletionItemKind.Enum)
    case Completion.TypeEnumCompletion(enumSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(label = s"${enumSym.name}$nameSuffix", sortText = priority, textEdit = textEdit,
        documentation = documentation, insertTextFormat = InsertTextFormat.Snippet, kind = CompletionItemKind.Enum)
    case Completion.TypeAliasCompletion(aliasSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(label = s"${aliasSym.name}$nameSuffix", sortText = priority, textEdit = textEdit,
        documentation = documentation, insertTextFormat = InsertTextFormat.Snippet, kind = CompletionItemKind.Enum)
    case Completion.EffectCompletion(name, priority, documentation, context) =>
      CompletionItem(label = name, sortText = priority, textEdit = TextEdit(context.range, name),
        documentation = documentation, insertTextFormat = InsertTextFormat.Snippet, kind = CompletionItemKind.Enum)
    case Completion.WithCompletion(name, priority, textEdit, documentation, insertTextFormat) =>
      CompletionItem(label = name, sortText = priority, textEdit = textEdit, documentation = documentation,
        insertTextFormat = insertTextFormat, kind = CompletionItemKind.Class)
    case Completion.ImportNewCompletion(constructor, clazz, aliasSuggestion, context) =>
      val (label, priority, textEdit) = getExecutableCompletionInfo(constructor, clazz, aliasSuggestion, context)
      CompletionItem(label = label, sortText = priority, textEdit = textEdit, documentation = None,
        insertTextFormat = InsertTextFormat.Snippet, kind = CompletionItemKind.Method)
    case Completion.ImportMethodCompletion(method, clazz, context) =>
      val (label, priority, textEdit) = getExecutableCompletionInfo(method, clazz, None, context)
      CompletionItem(label = label, sortText = priority, textEdit = textEdit, documentation = None,
        insertTextFormat = InsertTextFormat.Snippet, kind = CompletionItemKind.Method)
    case Completion.ImportFieldCompletion(field, clazz, isGet, context) =>
      val ret = if (isGet) convertJavaClassToFlixType(field.getType) else "Unit"
      val asSuggestion = if (isGet) s"get${field.getName}" else s"set${field.getName}"
      val label = s"$clazz.${field.getName}: $ret"
      val textEdit = TextEdit(context.range, s"$label \\ IO as $${0:$asSuggestion};")
      CompletionItem(label = label, sortText = Priority.high(label), textEdit = textEdit, documentation = None,
        insertTextFormat = InsertTextFormat.Snippet, kind = CompletionItemKind.Field)
    case Completion.ClassCompletion(name, context) =>
      CompletionItem(label = name, sortText = Priority.high(name), textEdit = TextEdit(context.range, name),
        documentation = None, insertTextFormat = InsertTextFormat.PlainText, kind = CompletionItemKind.Class)
    case Completion.SnippetCompletion(name, snippet, documentation, context) =>
      CompletionItem(label = name, sortText = Priority.snippet(name), textEdit = TextEdit(context.range, snippet),
        documentation = Some(documentation), insertTextFormat = InsertTextFormat.Snippet, kind = CompletionItemKind.Snippet)
    case Completion.VarCompletion(sym, tpe, context, flix) =>
      CompletionItem(label = sym.text, sortText = Priority.local(sym.text), textEdit = TextEdit(context.range, sym.text),
        detail = Some(FormatType.formatType(tpe)(flix)), kind = CompletionItemKind.Variable)
  }

  /**
    * returns a triple from a java executable (method/constructor) instance, providing information the make the specific completion.
    * clazz is the clazz in string form used for the completion.
    * aliasSuggestion is used to suggest an alias for the function if applicable.
    */
  private def getExecutableCompletionInfo(exec: Executable, clazz: String, aliasSuggestion: Option[String], context: CompletionContext): (String, String, TextEdit) = {
    val typesString = exec.getParameters.map(param => convertJavaClassToFlixType(param.getType)).mkString("(", ", ", ")")
    val finalAliasSuggestion = aliasSuggestion match {
      case Some(aliasSuggestion) => s" as $${0:$aliasSuggestion}"
      case None => ""
    }
    // Get the name of the function if it is not a constructor.
    val name = if (exec.isInstanceOf[Constructor[_ <: Object]]) "" else s".${exec.getName}"
    // So for constructors we do not have a return type method but we know it is the declaring class.
    val returnType = exec match {
      case method: Method => method.getReturnType
      case _ => exec.getDeclaringClass
    }
    val label = s"$clazz$name$typesString"
    val replace = s"$clazz$name$typesString: ${convertJavaClassToFlixType(returnType)} \\ IO$finalAliasSuggestion;"
    (label, Priority.high(s"${exec.getParameterCount}$label"), TextEdit(context.range, replace))
  }
}

object Completion {

  /**
    * Represents a keyword completion.
    *
    * @param name the name of the keyword.
    */
  case class KeywordCompletion(name: String, context: CompletionContext) extends Completion

  /**
    * Represents a field completion.
    *
    * @param name the name of the field.
    */
  case class FieldCompletion(name: String, context: CompletionContext) extends Completion


  /**
    * Represents a predicate completion.
    *
    * @param name     the name of the predicate.
    * @param priority the priority of the predicate.
    */
  case class PredicateCompletion(name: String, priority: String, context: CompletionContext) extends Completion

  /**
    * Represents a type completion for builtin
    *
    * @param name               the name of the BuiltinType.
    * @param priority           the priority of the BuiltinType.
    * @param textEdit           the edit which is applied to a document when selecting this completion.
    * @param insertTextFormat   the format of the insert text.
    */
  case class TypeBuiltinCompletion(name: String, priority: String, textEdit: TextEdit,
                                   insertTextFormat: InsertTextFormat) extends Completion

  /**
    * Represents a type completion for enum
    *
    * @param enumSym       the enum symbol.
    * @param nameSuffix    the suffix for the name of the EnumType.
    * @param priority      the priority of the EnumType.
    * @param textEdit      the edit which is applied to a document when selecting this completion.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class TypeEnumCompletion(enumSym: EnumSym, nameSuffix: String, priority: String, textEdit: TextEdit,
                                documentation: Option[String]) extends Completion

  /**
    * Represents a type completion for alias
    *
    * @param aliasSym      the alias symbol.
    * @param nameSuffix    the suffix for the name of the AliasType.
    * @param priority      the priority of the AliasType.
    * @param textEdit      the edit which is applied to a document when selecting this completion.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class TypeAliasCompletion(aliasSym: TypeAliasSym, nameSuffix: String, priority: String, textEdit: TextEdit,
                                 documentation: Option[String]) extends Completion

  /**
    * Represents a Effect completion
    *
    * @param name          the name of the effect.
    * @param priority      the priority of the effect.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class EffectCompletion(name: String, priority: String, documentation: Option[String],
                              context: CompletionContext) extends Completion

  /**
    * Represents a With completion
    *
    * @param name               the name of the completion.
    * @param priority           the priority of the completion.
    * @param textEdit           the edit which is applied to a document when selecting this completion.
    * @param documentation      a human-readable string that represents a doc-comment.
    * @param insertTextFormat   the format of the insert text.
    */
  case class WithCompletion(name: String, priority: String, textEdit: TextEdit, documentation: Option[String],
                            insertTextFormat: InsertTextFormat) extends Completion


  /**
    * Represents an importNew completion (java constructors)
    *
    * @param constructor      the constructor.
    * @param clazz            clazz is the clazz in string form.
    * @param aliasSuggestion  an alias for the function.
    */
  case class ImportNewCompletion(constructor: Constructor[_], clazz: String, aliasSuggestion: Option[String], context: CompletionContext) extends Completion

  /**
    * Represents an importMethod completion (java methods)
    *
    * @param method  the method.
    * @param clazz   clazz is the clazz in string form.
    */
  case class ImportMethodCompletion(method: Method, clazz: String, context: CompletionContext) extends Completion


  /**
    * Represents an importField completion
    *
    * @param field the field.
    * @param clazz clazz is the clazz in string form.
    * @param isGet determines whether is it a set or get.
    */
  case class ImportFieldCompletion(field: Field, clazz: String, isGet: Boolean, context: CompletionContext) extends Completion

  /**
    * Represents a Class completion (java packages/classes)
    *
    * @param name the name of the class.
    */
  case class ClassCompletion(name: String, context: CompletionContext) extends Completion

  /**
    * Represents a Snippet completion
    *
    * @param name           the name of the snippet.
    * @param snippet        the snippet for TextEdit.
    * @param documentation  a human-readable string that represents a doc-comment.
    */
  case class SnippetCompletion(name: String, snippet: String, documentation: String, context: CompletionContext) extends Completion

  /**
    * Represents a Var completion
    *
    * @param sym      the Var symbol.
    * @param tpe      the type for FormatType to provide a human-readable string with additional information
    *                 about the symbol.
    * @param flix     Implicit parameter for FormatType.formatType(...)
    */
  case class VarCompletion(sym: Symbol.VarSym, tpe: Type, context: CompletionContext, flix: Flix) extends Completion
}
