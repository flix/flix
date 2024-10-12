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
import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, InsertTextFormat, TextEdit}
import ca.uwaterloo.flix.language.ast.Symbol.{EnumSym, ModuleSym, StructSym, TypeAliasSym}
import ca.uwaterloo.flix.language.ast.{Range, Name, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatScheme, FormatType}

import java.lang.reflect.{Field, Method}

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
      CompletionItem(
        label            = name,
        sortText         = name,
        textEdit         = TextEdit(context.range, name),
        documentation    = Some(doc),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Enum
      )

    case Completion.KeywordCompletion(name, priority) =>
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(context.range, s"$name "),
        kind     = CompletionItemKind.Keyword
      )

    case Completion.KeywordLiteralCompletion(name, priority) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = TextEdit(context.range, name),
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Keyword
      )

    case Completion.LabelCompletion(label, prefix) =>
      val name = s"$prefix#${label.name}"
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(Priority.Highest, name),
        textEdit = TextEdit(context.range, name),
        kind     = CompletionItemKind.Variable
      )

    case Completion.PredicateCompletion(name, arity, detail) =>
      val args = (1 until arity + 1).map(i => s"$${$i:x$i}").mkString(", ")
      CompletionItem(
        label            = s"$name/$arity",
        sortText         = Priority.toSortText(Priority.Lower, name),
        textEdit         = TextEdit(context.range, s"$name($args)"),
        detail           = Some(detail),
        kind             = CompletionItemKind.Field,
        insertTextFormat = InsertTextFormat.Snippet
      )

    case Completion.TypeBuiltinCompletion(name, priority) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = TextEdit(context.range, name),
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Enum
      )

    case Completion.TypeBuiltinPolyCompletion(name, edit, priority) =>
      CompletionItem(label = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = TextEdit(context.range, edit),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Enum
      )

    case Completion.EnumCompletion(enumSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(
        label            = s"${enumSym.toString}$nameSuffix",
        sortText         = Priority.toSortText(priority, enumSym.name),
        textEdit         = textEdit,
        documentation    = documentation,
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Enum
      )

    case Completion.StructCompletion(structSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(
        label            = s"${structSym.toString}$nameSuffix",
        sortText         = Priority.toSortText(priority, structSym.name),
        textEdit         = textEdit,
        documentation    = documentation,
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Struct
      )

    case Completion.TypeAliasCompletion(aliasSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(
        label            = s"${aliasSym.name}$nameSuffix",
        sortText         = Priority.toSortText(priority, aliasSym.name),
        textEdit         = textEdit,
        documentation    = documentation,
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Enum
      )

    case Completion.WithCompletion(name, priority, textEdit, documentation, insertTextFormat) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = textEdit,
        documentation = documentation,
        insertTextFormat = insertTextFormat,
        kind = CompletionItemKind.Class
      )

    case Completion.WithHandlerCompletion(name, textEdit) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(Priority.Highest, name),
        textEdit = textEdit,
        documentation    = None,
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.ImportCompletion(name) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(Priority.Highest, name),
        textEdit         = TextEdit(context.range, name),
        documentation    = None,
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Class
      )

    case Completion.SnippetCompletion(name, snippet, documentation) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(Priority.High, name),
        textEdit         = TextEdit(context.range, snippet),
        documentation    = Some(documentation),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.VarCompletion(sym, tpe) =>
      CompletionItem(
        label    = sym.text,
        sortText = Priority.toSortText(Priority.Low, sym.text),
        textEdit = TextEdit(context.range, sym.text),
        detail   = Some(FormatType.formatType(tpe)(flix)),
        kind     = CompletionItemKind.Variable
      )

    case Completion.DefCompletion(decl) =>
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      CompletionItem(
        label            = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec)(flix),
        sortText         = Priority.toSortText(Priority.Lower, name),
        filterText       = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit         = TextEdit(context.range, snippet),
        detail           = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation    = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Function
      )

    case Completion.SigCompletion(decl) =>
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      CompletionItem(
        label            = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec)(flix),
        sortText         = Priority.toSortText(Priority.Lower, name),
        filterText       = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit         = TextEdit(context.range, snippet),
        detail           = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation    = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Interface
      )

    case Completion.OpCompletion(decl) =>
      // NB: priority is high because only an op can come after `do`
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      CompletionItem(
        label            = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec)(flix),
        sortText         = Priority.toSortText(Priority.Highest, name),
        filterText       = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit         = TextEdit(context.range, snippet),
        detail           = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation    = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Interface
      )

    case Completion.MatchCompletion(enm, completion) =>
      val label = s"match ${enm.sym.toString}"
      CompletionItem(
        label            = label,
        sortText         = Priority.toSortText(Priority.Lower, label),
        textEdit         = TextEdit(context.range, completion),
        documentation    = None,
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.InstanceCompletion(trt, completion) =>
      val traitSym = trt.sym
      CompletionItem(
        label            = s"$traitSym[...]",
        sortText         = Priority.toSortText(Priority.Highest, traitSym.toString),
        textEdit         = TextEdit(context.range, completion),
        detail           = Some(InstanceCompleter.fmtTrait(trt)),
        documentation    = Some(trt.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.UseEnumCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Enum
      )

    case Completion.UseEffCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Enum
      )

    case Completion.UseDefCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.UseEnumTagCompletion(sym, caze) =>
      val name = s"${sym.toString}.${caze.sym.name}"
      CompletionItem(
        label         = name,
        sortText      = Priority.toSortText(Priority.Lower, name),
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.UseOpCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.UseSignatureCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.EnumTagCompletion(enumSym, cas, arity) =>
      val name = s"${enumSym.toString}.${cas.sym.name}"
      val args = (1 until arity + 1).map(i => s"?elem$i").mkString(", ")
      val snippet = if (args.isEmpty) name else s"$name($args)"
      CompletionItem(
        label            = CompletionUtils.getLabelForEnumTags(name, cas, arity),
        sortText         = Priority.toSortText(Priority.Lower, name),
        textEdit         = TextEdit(context.range, snippet),
        detail           = Some(enumSym.name),
        documentation    = None,
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.EnumMember
      )

    case Completion.ModCompletion(modSym) =>
      val name = modSym.toString
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(Priority.Lowest, name),
        textEdit = TextEdit(context.range, name),
        kind     = CompletionItemKind.Module
      )

    case Completion.FieldCompletion(ident, field) =>
      val label = field.getName
      val text = field.getName
      val range = ident.loc.range

      CompletionItem(
        label            = label,
        sortText         = Priority.toSortText(Priority.Lowest, label),
        textEdit         = TextEdit(range, text),
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Method
      )

    case Completion.StructFieldCompletion(field, loc, tpe) =>
      CompletionItem(
        label    = field,
        sortText = Priority.toSortText(Priority.Lowest, field),
        textEdit = TextEdit(loc.range, field),
        detail   = Some(FormatType.formatType(tpe)(flix)),
        kind     = CompletionItemKind.Property,
      )

    case Completion.MethodCompletion(ident, method) =>
      val argsWithName = method.getParameters.map(_.getName)
      val argsWithNameAndType = method.getParameters.map(p => p.getName + ": " + p.getType.getSimpleName)
      val returnType = method.getReturnType.getSimpleName
      val returnEffect = "IO"

      val label = method.getName + "(" + argsWithNameAndType.mkString(", ") + "): " + returnType + " \\ " + returnEffect
      val text = method.getName + "(" + argsWithName.zipWithIndex.map {case (arg, i) => s"$${${i + 1}:$arg}" }.mkString(", ") + ")"
      val range = ident.loc.range

      CompletionItem(
        label            = label,
        sortText         = Priority.toSortText(Priority.Lowest, label),
        textEdit         = TextEdit(range, text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Method
      )

    case Completion.HoleCompletion(sym, decl, priority, loc) =>
      val name = decl.sym.toString
      val args = decl.spec.fparams.dropRight(1).zipWithIndex.map {
        case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.sym.text}}"
      } ::: sym.text :: Nil
      val params = args.mkString(", ")
      val snippet = s"$name($params)"
      CompletionItem(label = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec),
        filterText = Some(s"${sym.text}?$name"),
        sortText = priority,
        textEdit = TextEdit(loc.range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function)

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
    * @param name      the name of the keyword.
    * @param priority  the completion priority of the keyword.
    */
  case class KeywordCompletion(name: String, priority: Priority) extends Completion

  /**
    * Represents a keyword literal completion (i.e. `true`).
    *
    * The reason we differentiate bewteen normal keywords and these literals
    * is because completions for the former should include a trailing space
    * whereas completions for the latter we might not want one.
    *
    * To illustrate this consider the two following correct completions (where ˽ denotes a space)
    *
    * `de`      --->    `def˽`
    * `f(fal)`  --->    `f(false)`
    *
    * After the keyword `def` we *always* want a space but if we were
    * to add a trailing space after `false` we would get the unnatural completion
    *
    * `f(fal)`  --->    `f(false˽)`
    *
    * @param literal   the literal keyword text.
    * @param priority  the priority of the keyword.
    */
  case class KeywordLiteralCompletion(literal: String, priority: Priority) extends Completion

  /**
    * Represents a label completion.
    *
    * @param label  the label.
    * @param prefix the prefix.
    */
  case class LabelCompletion(label: Name.Label, prefix: String) extends Completion

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
    */
  case class TypeBuiltinCompletion(name: String, priority: Priority) extends Completion

  /**
    * Represents a type completion for a builtin polymorphic type.
    *
    * @param name      the name of the type.
    * @param priority  the priority of the type.
    */
  case class TypeBuiltinPolyCompletion(name: String, edit: String, priority: Priority) extends Completion

  /**
    * Represents a type completion for enum
    *
    * @param enumSym       the enum symbol.
    * @param nameSuffix    the suffix for the name of the Enum.
    * @param priority      the priority of the Enum.
    * @param textEdit      the edit which is applied to a document when selecting this completion.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class EnumCompletion(enumSym: EnumSym, nameSuffix: String, priority: Priority, textEdit: TextEdit,
                            documentation: Option[String]) extends Completion

  /**
    * Represents a type completion for struct
    *
    * @param structSym     the struct symbol.
    * @param nameSuffix    the suffix for the name of the Struct.
    * @param priority      the priority of the Struct.
    * @param textEdit      the edit which is applied to a document when selecting this completion.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class StructCompletion(structSym: StructSym, nameSuffix: String, priority: Priority, textEdit: TextEdit,
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
  case class TypeAliasCompletion(aliasSym: TypeAliasSym, nameSuffix: String, priority: Priority, textEdit: TextEdit,
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
  case class WithCompletion(name: String, priority: Priority, textEdit: TextEdit, documentation: Option[String],
                            insertTextFormat: InsertTextFormat) extends Completion

  /**
    * Represents a WithHandler completion
    *
    * @param name             the name of the completion.
    * @param textEdit         the edit which is applied to a document when selecting this completion.
    */
  case class WithHandlerCompletion(name: String, textEdit: TextEdit) extends Completion

  /**
    * Represents a package, class, or interface completion.
    *
    * @param name the name to be completed.
    */
  case class ImportCompletion(name: String) extends Completion

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
    * @param enm        the enum for the match.
    * @param completion the completion string (used as information for TextEdit).
    */
  case class MatchCompletion(enm: TypedAst.Enum, completion: String) extends Completion

  /**
    * Represents an Instance completion (based on traits)
    *
    * @param trt        the trait.
    * @param completion the completion string (used as information for TextEdit).
    */
  case class InstanceCompletion(trt: TypedAst.Trait, completion: String) extends Completion

  /**
    * Represents a Use Enum completion.
    *
    * @param name the name of the use enum completion.
    */
  case class UseEnumCompletion(name: String) extends Completion

  /**
    * Represents a Use Effect completion.
    *
    * @param name the name of the use effect completion.
    */
  case class UseEffCompletion(name: String) extends Completion

  /**
    * Represents a Use Def completion.
    *
    * @param name the name of the use def completion.
    */
  case class UseDefCompletion(name: String) extends Completion

  /**
    * Represents a Use Enum Tag completion
    *
    * @param enumSym the sym of the enum.
    * @param caze    the case of the enum.
    */
  case class UseEnumTagCompletion(enumSym: EnumSym, caze: TypedAst.Case) extends Completion

  /**
    * Represents a Use Op completion.
    *
    * @param name the name of the use op completion.
    */
  case class UseOpCompletion(name: String) extends Completion

  /**
    * Represents a Use Signature completion.
    *
    * @param name the name of the use signature completion.
    */
  case class UseSignatureCompletion(name: String) extends Completion

  /**
    * Represents an EnumTag completion
    *
    * @param enumSym the sym of the enum.
    * @param cas     the case (for that specific enum).
    * @param arity   the arity of the enumTag.
    */
  case class EnumTagCompletion(enumSym: EnumSym, cas: TypedAst.Case, arity: Int) extends Completion

  /**
   * Represents a struct field completion.
   *
   * @param field the candidate field.
   */
  case class StructFieldCompletion(field: String, symLoc: SourceLocation, tpe: Type) extends Completion

  /**
    * Represents a Module completion.
    *
    * @param modSym the module symbol.
    */
  case class ModCompletion(modSym: ModuleSym) extends Completion

  /**
   * Represents a Java field completion.
   *
   * @param ident  the partial field name.
   * @param field the candidate field.
   */
  case class FieldCompletion(ident: Name.Ident, field: Field) extends Completion

  /**
    * Represents a Java method completion.
    *
    * @param ident  the partial method name.
    * @param method the candidate method.
    */
  case class MethodCompletion(ident: Name.Ident, method: Method) extends Completion

  /**
    * Represents a hole completion.
    *
    * @param sym      the variable symbol being completed on.
    * @param decl     the proposed def declaration to call.
    * @param priority the priority of the completion (multiple suggestions are possible and they are ranked).
    * @param loc      the source location of the hole.
    */
  case class HoleCompletion(sym: Symbol.VarSym, decl: TypedAst.Def, priority: String, loc: SourceLocation) extends Completion

}
