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
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.Priority
import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, InsertTextFormat, TextEdit}
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatScheme, FormatType}
import ca.uwaterloo.flix.language.ast.Symbol.{CaseSym, EnumSym, TypeAliasSym}

import java.lang.reflect.{Constructor, Field, Method}

/**
  * A common super-type for auto-completions.
  */
sealed trait Completion {
  /**
    * Returns a LSP completion item for `this`.
    */
  def toCompletionItem(context: CompletionContext)(implicit flix: Flix): CompletionItem = this match {

    case Completion.EffectCompletion(sym, doc) =>
      val name = sym.toString
      CompletionItem(label = name,
        sortText = name,
        textEdit = TextEdit(context.range, name),
        documentation = Some(doc),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Enum)

    case Completion.KeywordCompletion(name) =>
      CompletionItem(label = name,
        sortText = Priority.normal(name),
        textEdit = TextEdit(context.range, s"$name "),
        kind = CompletionItemKind.Keyword)
    case Completion.FieldCompletion(name) =>
      CompletionItem(label = name,
        sortText = Priority.high(name),
        textEdit = TextEdit(context.range, s"$name "),
        kind = CompletionItemKind.Variable)

    case Completion.PredicateCompletion(name, arity, detail) =>
      val args = (1 until arity + 1).map(i => s"$${$i:x$i}").mkString(", ")
      CompletionItem(
        label = s"$name/$arity",
        sortText = Priority.normal(name),
        textEdit = TextEdit(context.range, s"$name($args)"),
        detail = Some(detail),
        kind = CompletionItemKind.Field,
        insertTextFormat = InsertTextFormat.Snippet
      )

    case Completion.TypeBuiltinCompletion(name, priority, textEdit, insertTextFormat) =>
      CompletionItem(label = name,
        sortText = priority,
        textEdit = textEdit,
        insertTextFormat = insertTextFormat,
        kind = CompletionItemKind.Enum)
    case Completion.TypeEnumCompletion(enumSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(label = s"${enumSym.name}$nameSuffix",
        sortText = priority,
        textEdit = textEdit,
        documentation = documentation,
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Enum)
    case Completion.TypeAliasCompletion(aliasSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(label = s"${aliasSym.name}$nameSuffix",
        sortText = priority,
        textEdit = textEdit,
        documentation = documentation,
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Enum)

    case Completion.WithCompletion(name, priority, textEdit, documentation, insertTextFormat) =>
      CompletionItem(label = name,
        sortText = priority,
        textEdit = textEdit,
        documentation = documentation,
        insertTextFormat = insertTextFormat,
        kind = CompletionItemKind.Class)
    case Completion.ImportNewCompletion(constructor, clazz, aliasSuggestion) =>
      val (label, priority, textEdit) = CompletionUtils.getExecutableCompletionInfo(constructor, clazz, aliasSuggestion, context)
      CompletionItem(label = label,
        sortText = priority,
        textEdit = textEdit,
        documentation = None,
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Method)
    case Completion.ImportMethodCompletion(method, clazz) =>
      val (label, priority, textEdit) = CompletionUtils.getExecutableCompletionInfo(method, clazz, None, context)
      CompletionItem(label = label,
        sortText = priority,
        textEdit = textEdit,
        documentation = None,
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Method)
    case Completion.ImportFieldCompletion(field, clazz, isGet) =>
      val ret = if (isGet) CompletionUtils.convertJavaClassToFlixType(field.getType) else "Unit"
      val asSuggestion = if (isGet) s"get${field.getName}" else s"set${field.getName}"
      val label = s"$clazz.${field.getName}: $ret"
      val textEdit = TextEdit(context.range, s"$label \\ IO as $${0:$asSuggestion};")
      CompletionItem(label = label,
        sortText = Priority.high(label),
        textEdit = textEdit,
        documentation = None,
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Field)
    case Completion.ClassCompletion(name) =>
      CompletionItem(label = name,
        sortText = Priority.high(name),
        textEdit = TextEdit(context.range, name),
        documentation = None,
        insertTextFormat = InsertTextFormat.PlainText,
        kind = CompletionItemKind.Class)
    case Completion.SnippetCompletion(name, snippet, documentation) =>
      CompletionItem(label = name,
        sortText = Priority.snippet(name),
        textEdit = TextEdit(context.range, snippet),
        documentation = Some(documentation),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Snippet)
    case Completion.VarCompletion(sym, tpe) =>
      CompletionItem(label = sym.text,
        sortText = Priority.local(sym.text),
        textEdit = TextEdit(context.range, sym.text),
        detail = Some(FormatType.formatType(tpe)(flix)),
        kind = CompletionItemKind.Variable)
    case Completion.DefCompletion(decl) =>
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      CompletionItem(label = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec)(flix),
        sortText = Priority.normal(name),
        filterText = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit = TextEdit(context.range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function)
    case Completion.SigCompletion(decl) =>
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      CompletionItem(label = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec)(flix),
        sortText = Priority.normal(name),
        filterText = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit = TextEdit(context.range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Interface)
    case Completion.OpCompletion(decl) =>
      // NB: priority is high because only an op can come after `do`
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      CompletionItem(label = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec)(flix),
        sortText = Priority.high(name),
        filterText = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit = TextEdit(context.range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Interface)
    case Completion.MatchCompletion(sym, completion, priority) =>
      val label = s"match $sym"
      CompletionItem(label = label,
        sortText = priority(label),
        textEdit = TextEdit(context.range, completion),
        documentation = None,
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Snippet)
    case Completion.InstanceCompletion(clazz, completion) =>
      val classSym = clazz.sym
      CompletionItem(label = s"$classSym[...]",
        sortText = Priority.high(classSym.toString),
        textEdit = TextEdit(context.range, completion),
        detail = Some(InstanceCompleter.fmtClass(clazz)),
        documentation = Some(clazz.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Snippet)
    case Completion.UseCompletion(name, kind) =>
      CompletionItem(
        label = name,
        sortText = Priority.high(name),
        textEdit = TextEdit(context.range, name),
        documentation = None,
        kind = kind)
    case Completion.FromErrorsCompletion(name) =>
      CompletionItem(label = name,
        sortText = Priority.high(name),
        textEdit = TextEdit(context.range, name + " "),
        detail = None,
        kind = CompletionItemKind.Variable)
    case Completion.EnumTagCompletion(enumSym, caseSym) =>
      val name = s"${enumSym.toString}.${caseSym.name}"
      CompletionItem(
        label = name,
        sortText = Priority.normal(name),
        textEdit = TextEdit(context.range, name),
        documentation = None,
        kind = CompletionItemKind.EnumMember)
  }
}

object Completion {

  /**
    * Represents an effect symbol completion.
    *
    * @param sym the effect symbol.
    * @param doc the documentation associated with the effect.
    */
  case class EffectCompletion(sym: Symbol.EffectSym, doc: String) extends Completion

  /**
    * Represents a keyword completion.
    *
    * @param name the name of the keyword.
    */
  case class KeywordCompletion(name: String) extends Completion

  /**
    * Represents a field completion.
    *
    * @param name the name of the field.
    */
  case class FieldCompletion(name: String) extends Completion

  /**
    * Represents a predicate completion.
    *
    * @param name   the name of the predicate.
    * @param arity  the arity of the predicate.
    * @param detail the type of the predicate.
    */
  case class PredicateCompletion(name: String, arity: Int, detail: String) extends Completion

  /**
    * Represents a type completion for builtin
    *
    * @param name             the name of the BuiltinType.
    * @param priority         the priority of the BuiltinType.
    * @param textEdit         the edit which is applied to a document when selecting this completion.
    * @param insertTextFormat the format of the insert text.
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
    * Represents a With completion
    *
    * @param name             the name of the completion.
    * @param priority         the priority of the completion.
    * @param textEdit         the edit which is applied to a document when selecting this completion.
    * @param documentation    a human-readable string that represents a doc-comment.
    * @param insertTextFormat the format of the insert text.
    */
  case class WithCompletion(name: String, priority: String, textEdit: TextEdit, documentation: Option[String],
                            insertTextFormat: InsertTextFormat) extends Completion


  /**
    * Represents an importNew completion (java constructors)
    *
    * @param constructor     the constructor.
    * @param clazz           clazz is the clazz in string form.
    * @param aliasSuggestion an alias for the function.
    */
  case class ImportNewCompletion(constructor: Constructor[_], clazz: String, aliasSuggestion: Option[String]) extends Completion

  /**
    * Represents an importMethod completion (java methods)
    *
    * @param method the method.
    * @param clazz  clazz is the clazz in string form.
    */
  case class ImportMethodCompletion(method: Method, clazz: String) extends Completion


  /**
    * Represents an importField completion
    *
    * @param field the field.
    * @param clazz clazz is the clazz in string form.
    * @param isGet determines whether is it a set or get.
    */
  case class ImportFieldCompletion(field: Field, clazz: String, isGet: Boolean) extends Completion

  /**
    * Represents a Class completion (java packages/classes)
    *
    * @param name the name of the class.
    */
  case class ClassCompletion(name: String) extends Completion

  /**
    * Represents a Snippet completion
    *
    * @param name          the name of the snippet.
    * @param snippet       the snippet for TextEdit.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class SnippetCompletion(name: String, snippet: String, documentation: String) extends Completion

  /**
    * Represents a Var completion
    *
    * @param sym the Var symbol.
    * @param tpe the type for FormatType to provide a human-readable string with additional information
    *            about the symbol.
    */
  case class VarCompletion(sym: Symbol.VarSym, tpe: Type) extends Completion

  /**
    * Represents a Def completion
    *
    * @param decl the def decl.
    */
  case class DefCompletion(decl: TypedAst.Def) extends Completion

  /**
    * Represents a Signature completion
    *
    * @param decl the signature decl.
    */
  case class SigCompletion(decl: TypedAst.Sig) extends Completion

  /**
    * Represents a Op completion
    *
    * @param decl the op decl.
    */
  case class OpCompletion(decl: TypedAst.Op) extends Completion

  /**
    * Represents an exhaustive Match completion
    *
    * @param sym        the match sym (it's name).
    * @param completion the completion string (used as information for TextEdit).
    * @param priority   the priority of the completion.
    */
  case class MatchCompletion(sym: String, completion: String, priority: String => String) extends Completion

  /**
    * Represents an Instance completion (based on type classes)
    *
    * @param clazz      the clazz.
    * @param completion the completion string (used as information for TextEdit).
    */
  case class InstanceCompletion(clazz: TypedAst.Class, completion: String) extends Completion

  /**
    * Represents an Use completion.
    *
    * @param name the name of the use completion.
    * @param kind the kind of completion.
    */
  case class UseCompletion(name: String, kind: CompletionItemKind) extends Completion

  /**
    * Represents a FromErrors completion
    *
    * @param name the name of the fromError completion.
    */
  case class FromErrorsCompletion(name: String) extends Completion

  /**
    * Represents an EnumTag completion
    *
    * @param enumSym the sym of the enum.
    * @param caseSym the sym of the case (for that specific enum).
    */
  case class EnumTagCompletion(enumSym: EnumSym, caseSym: CaseSym) extends Completion
}
