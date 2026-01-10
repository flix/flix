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
import ca.uwaterloo.flix.api.lsp.{Command, CompletionItem, CompletionItemKind, CompletionItemLabelDetails, InsertTextFormat, LspUtil, Position, Range, TextEdit}
import ca.uwaterloo.flix.language.ast.shared.AnchorPosition
import ca.uwaterloo.flix.language.ast.{Name, ResolvedAst, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatScheme, FormatType}

import java.lang.reflect.{Field, Method}

/**
  * A common super-type for auto-completions.
  */
sealed trait Completion {

  /**
    * Returns the [[Priority]] of `this` completion.
    */
  def priority: Priority

  /**
    * Returns the LSP [[CompletionItem]] for `this` completion.
    */
  def toCompletionItem(implicit flix: Flix): CompletionItem = this match {

    case Completion.AnnotationCompletion(name, range, priority) =>
      CompletionItem(
        label = "@" + name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, "@" + name),
        kind = CompletionItemKind.Constant
      )

    case Completion.KeywordCompletion(name, range, priority, withSpace) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, if (withSpace) name + " " else name),
        kind = CompletionItemKind.Keyword
      )

    case Completion.KindCompletion(name, range, priority) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        kind = CompletionItemKind.TypeParameter
      )

    case Completion.PredicateCompletion(name, range, priority, arity, detail) =>
      val args = (1 until arity + 1).map(i => s"$${$i:x$i}").mkString(", ")
      CompletionItem(
        label = s"$name/$arity",
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, s"$name($args)"),
        detail = Some(detail),
        kind = CompletionItemKind.Field,
        insertTextFormat = InsertTextFormat.Snippet
      )

    case Completion.TypeBuiltinCompletion(name, range, priority) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        insertTextFormat = InsertTextFormat.PlainText,
        kind = CompletionItemKind.Enum
      )

    case Completion.TypeBuiltinPolyCompletion(name, edit, range, priority) =>
      CompletionItem(label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, edit),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Enum
      )

    case Completion.ImportCompletion(name, range, priority, isPackage) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        documentation = None,
        insertTextFormat = InsertTextFormat.PlainText,
        kind = {
          if (isPackage) CompletionItemKind.Module
          else CompletionItemKind.Class
        }
      )

    case Completion.AutoImportCompletion(name, path, range, ap, labelDetails, priority) =>
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        insertTextFormat = InsertTextFormat.PlainText,
        kind = CompletionItemKind.Class,
        additionalTextEdits = List(Completion.mkTextEdit(ap, s"import $path"))
      )

    case Completion.SnippetCompletion(name, range, priority, snippet, documentation) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        documentation = Some(documentation),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Snippet
      )

    case Completion.MagicDefCompletion(decl, label, snippet, range, priority) =>
      val labelDetails = CompletionItemLabelDetails(Some(CompletionUtils.getLabelForSpec(decl.spec)(flix)), None)
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, decl.sym.toString),
        textEdit = TextEdit(range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Snippet
      )

    case Completion.MagicMatchCompletion(name, range, priority, snippet, documentation) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        documentation = Some(documentation),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Snippet
      )

    case Completion.LocalVarCompletion(name, range, priority) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        kind = CompletionItemKind.Variable
      )

    case Completion.LocalJavaClassCompletion(name, clazz, range, priority) =>
      val description = Option(clazz.getCanonicalName)
      val labelDetails = CompletionItemLabelDetails(None, description)
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        kind = CompletionItemKind.Class
      )

    case Completion.LocalDefCompletion(sym, fparams, range, priority) =>
      val snippet = sym.text + fparams.zipWithIndex.map { case (fparam, idx) => s"$${${idx + 1}:${fparam.sym.text}}" }.mkString("(", ", ", ")")
      CompletionItem(
        label = sym.text,
        sortText = Priority.toSortText(priority, sym.text),
        textEdit = TextEdit(range, snippet),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function
      )

    case Completion.DefCompletion(decl, range, priority, ap, qualified, inScope, ectx) =>
      val qualifiedName = decl.sym.toString
      val label = if (qualified) qualifiedName else decl.sym.name
      val snippet = LspUtil.mkSpecSnippet(label, decl.spec, ectx)
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(Some(CompletionUtils.getLabelForSpec(decl.spec)(flix)), description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, qualifiedName),
        filterText = Some(CompletionUtils.getFilterTextForName(qualifiedName)),
        textEdit = TextEdit(range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function,
        additionalTextEdits = additionalTextEdit,
        command = Some(Command("editor.action.triggerParameterHints", "editor.action.triggerParameterHints", Nil))
      )

    case Completion.EnumCompletion(enm, range, priority, ap, qualified, inScope, withTypeParameters) =>
      val qualifiedName = enm.sym.toString
      val name = if (qualified) qualifiedName else enm.sym.name
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val snippet = if (withTypeParameters)
        name + CompletionUtils.formatTParamsSnippet(enm.tparams)
      else
        name
      val label = if (withTypeParameters)
        name + CompletionUtils.formatTParams(enm.tparams)
      else
        name
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, qualifiedName),
        filterText = Some(CompletionUtils.getFilterTextForName(qualifiedName)),
        textEdit = TextEdit(range, snippet),
        insertTextFormat = InsertTextFormat.Snippet,
        documentation = Some(enm.doc.text),
        kind = CompletionItemKind.Enum,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.StructCompletion(struct, range, priority, ap, qualified, inScope) =>
      val qualifiedName = struct.sym.toString
      val name = if (qualified) qualifiedName else struct.sym.name
      val label = name + CompletionUtils.formatTParams(struct.tparams)
      val snippet = name + CompletionUtils.formatTParamsSnippet(struct.tparams)
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        documentation = Some(struct.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Struct,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.TraitCompletion(trt, range, priority, ap, qualified, inScope, withTypeParameter) =>
      val qualifiedName = trt.sym.toString
      val name = if (qualified) qualifiedName else trt.sym.name
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val label = if (withTypeParameter) name + CompletionUtils.formatTParams(List(trt.tparam)) else name
      val snippet = if (withTypeParameter) name + CompletionUtils.formatTParamsSnippet(List(trt.tparam)) else name
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        documentation = Some(trt.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Interface,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.InstanceCompletion(trt, range, priority, ap, qualified, inScope) =>
      val qualifiedName = trt.sym.toString
      val name = if (qualified) qualifiedName else trt.sym.name
      val label = name + CompletionUtils.formatTParams(List(trt.tparam))
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val snippet = CompletionUtils.fmtInstanceSnippet(trt, qualified = qualified)
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        documentation = Some(trt.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Interface,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.EffectCompletion(effect, range, priority, ap, qualified, inScope) =>
      val qualifiedName = effect.sym.toString
      val name = if (qualified) qualifiedName else effect.sym.name
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        documentation = Some(effect.doc.text),
        kind = CompletionItemKind.Event,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.HandlerCompletion(effect, range, priority, ap, qualified, inScope) =>
      val qualifiedName = effect.sym.toString
      val name = if (qualified) qualifiedName else effect.sym.name
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val opStrings = effect.ops.map(CompletionUtils.fmtOp)
      val snippet = s"$name {\n${opStrings.mkString("\n")}\n}"
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        documentation = Some(effect.doc.text),
        kind = CompletionItemKind.Event,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.TypeAliasCompletion(typeAlias, range, priority, ap, qualified, inScope) =>
      val qualifiedName = typeAlias.sym.toString
      val name = if (qualified) qualifiedName else typeAlias.sym.name
      val label = name + CompletionUtils.formatTParams(typeAlias.tparams)
      val snippet = name + CompletionUtils.formatTParamsSnippet(typeAlias.tparams)
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        documentation = Some(typeAlias.doc.text),
        kind = CompletionItemKind.TypeParameter,
        insertTextFormat = InsertTextFormat.Snippet,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.OpCompletion(op, namespace, range, priority, ap, qualified, inScope, ectx) =>
      val qualifiedName = if (namespace.nonEmpty)
        s"$namespace.${op.sym.name}"
      else
        op.sym.toString
      val name = if (qualified) qualifiedName else op.sym.name
      val snippet = LspUtil.mkSpecSnippet(name, op.spec, ectx)
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(Some(CompletionUtils.getLabelForSpec(op.spec)(flix)), description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        detail = Some(FormatScheme.formatScheme(op.spec.declaredScheme)(flix)),
        documentation = Some(op.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function,
        additionalTextEdits = additionalTextEdit,
        command = Some(Command("editor.action.triggerParameterHints", "editor.action.triggerParameterHints", Nil))
      )

    case Completion.OpHandlerCompletion(op, range, priority) =>
      val name = op.sym.name
      val snippet = CompletionUtils.getOpHandlerSnippet(name, op.spec.fparams)
      val description = Some(name)
      val labelDetails = CompletionItemLabelDetails(Some(CompletionUtils.getLabelForSpec(op.spec)(flix)), description)
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        detail = Some(FormatScheme.formatScheme(op.spec.declaredScheme)(flix)),
        documentation = Some(op.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function,
      )

    case Completion.SigCompletion(sig, namespace, range, priority, ap, qualified, inScope, ectx) =>
      val qualifiedName = if (namespace.nonEmpty)
        s"$namespace.${sig.sym.name}"
      else
        sig.sym.toString
      val name = if (qualified) qualifiedName else sig.sym.name
      val snippet = LspUtil.mkSpecSnippet(name, sig.spec, ectx)
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        detail = Some(FormatScheme.formatScheme(sig.spec.declaredScheme)(flix)),
        documentation = Some(sig.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function,
        additionalTextEdits = additionalTextEdit,
        command = Some(Command("editor.action.triggerParameterHints", "editor.action.triggerParameterHints", Nil))
      )

    case Completion.EnumTagCompletion(tag, namespace, range, priority, ap, qualified, inScope, ectx) =>
      val qualifiedName = if (namespace.nonEmpty)
        s"$namespace.${tag.sym.name}"
      else
        tag.sym.toString
      val name = if (qualified) qualifiedName else tag.sym.name
      val snippet = ectx match {
        case ExprContext.InsideApply => name
        case ExprContext.InsidePipeline => name + CompletionUtils.formatTypesSnippet(tag.tpes.dropRight(1))
        case ExprContext.InsideRunWith =>
          // Technically not possible, but we just revert to default behavior.
          name + CompletionUtils.formatTypesSnippet(tag.tpes)
        case ExprContext.Unknown => name + CompletionUtils.formatTypesSnippet(tag.tpes)
      }
      val label = name + CompletionUtils.formatTypes(tag.tpes)
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, snippet),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.EnumMember,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.ModuleCompletion(module, range, priority, ap, qualified, inScope) =>
      val qualifiedName = module.toString
      val name = if (qualified) qualifiedName else module.ns.last
      val description = if (!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        kind = CompletionItemKind.Module,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.UseCompletion(name, range, priority, kind) =>
      CompletionItem(
        label = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(range, name),
        documentation = None,
        kind = kind
      )

    case Completion.FieldCompletion(ident, priority, field) =>
      val label = field.getName
      val text = field.getName
      val range = Range.from(ident.loc)

      CompletionItem(
        label = label,
        sortText = Priority.toSortText(priority, label),
        textEdit = TextEdit(range, text),
        insertTextFormat = InsertTextFormat.PlainText,
        kind = CompletionItemKind.Method
      )

    case Completion.StructFieldCompletion(field, loc, tpe, priority) =>
      CompletionItem(
        label = field,
        sortText = Priority.toSortText(priority, field),
        textEdit = TextEdit(Range.from(loc), field),
        detail = Some(FormatType.formatType(tpe)(flix)),
        kind = CompletionItemKind.Property,
      )

    case Completion.MethodCompletion(ident, priority, method) =>
      val argsWithName = method.getParameters.map(_.getName)
      val argsWithNameAndType = method.getParameters.map(p => p.getName + ": " + p.getType.getSimpleName)
      val returnType = method.getReturnType.getSimpleName
      val returnEffect = "IO"

      val label = method.getName
      val labelDetails = CompletionItemLabelDetails(
        Some("(" + argsWithNameAndType.mkString(", ") + "): " + returnType + " \\ " + returnEffect),
        None
      )
      val text = method.getName + "(" + argsWithName.zipWithIndex.map { case (arg, i) => s"$${${i + 1}:$arg}" }.mkString(", ") + ")"
      val range = Range.from(ident.loc)

      CompletionItem(
        label = label,
        labelDetails = Some(labelDetails),
        sortText = Priority.toSortText(priority, label),
        textEdit = TextEdit(range, text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Method
      )

    case Completion.HoleCompletion(sym, decl, priority, loc) =>
      val name = decl.sym.toString
      val args = decl.spec.fparams.dropRight(1).zipWithIndex.map {
        case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.bnd.sym.text}}"
      } ::: sym.text :: Nil
      val params = args.mkString(", ")
      val snippet = s"$name($params)"
      val labelDetails = CompletionItemLabelDetails(
        Some(CompletionUtils.getLabelForSpec(decl.spec)(flix)),
        None)
      CompletionItem(
        label = name,
        labelDetails = Some(labelDetails),
        filterText = Some(s"${sym.text}?$name"),
        sortText = Priority.toSortText(priority, ""),
        textEdit = TextEdit(Range.from(loc), snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function)

  }
}

object Completion {

  /**
    * Represents an annotation completion.
    *
    * @param name     the name of the annotation.
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    */
  case class AnnotationCompletion(name: String, range: Range, priority: Priority) extends Completion

  /**
    * Represents a keyword completion.
    *
    * @param name      the name of the keyword.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param withSpace whether the completion should be followed by a space.
    */
  case class KeywordCompletion(name: String, range: Range, priority: Priority, withSpace: Boolean = true) extends Completion {
    override def toString: String = s"KeywordCompletion($name, $priority, $range)"
  }

  /**
    * Represents a completion for a kind.
    *
    * @param kind     the name of the kind.
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    */
  case class KindCompletion(kind: String, range: Range, priority: Priority) extends Completion

  /**
    * Represents a predicate completion.
    *
    * @param name     the name of the predicate.
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    * @param arity    the arity of the predicate.
    * @param detail   the type of the predicate.
    */
  case class PredicateCompletion(name: String, range: Range, priority: Priority, arity: Int, detail: String) extends Completion

  /**
    * Represents a type completion for builtin
    *
    * @param name     the name of the BuiltinType.
    * @param range    the range of the completion.
    * @param priority the priority of the BuiltinType.
    */
  case class TypeBuiltinCompletion(name: String, range: Range, priority: Priority) extends Completion

  /**
    * Represents a type completion for a builtin polymorphic type.
    *
    * @param name     the name of the type.
    * @param edit     the edit to be applied.
    * @param range    the range of the completion.
    * @param priority the priority of the type.
    */
  case class TypeBuiltinPolyCompletion(name: String, edit: String, range: Range, priority: Priority) extends Completion

  /**
    * Represents a package, class, or interface completion.
    *
    * @param name      the name to be completed.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param isPackage whether the completion is a package.
    */
  case class ImportCompletion(name: String, range: Range, priority: Priority, isPackage: Boolean) extends Completion

  /**
    * Represents an auto-import completion.
    *
    * @param name          the name to be completed under cursor.
    * @param qualifiedName the path of the java class we will auto-import.
    * @param range         the range of the completion.
    * @param ap            the anchor position.
    * @param labelDetails  to show the namespace of class we are going to import
    * @param priority      the priority of the completion.
    */
  case class AutoImportCompletion(name: String, qualifiedName: String, range: Range, ap: AnchorPosition, labelDetails: CompletionItemLabelDetails, priority: Priority) extends Completion {
    override def toString: String = s"AutoImportCompletion($name, $qualifiedName, $priority, $range)"
  }

  /**
    * Represents a Snippet completion
    *
    * @param name          the name of the snippet.
    * @param range         the range of the completion.
    * @param priority      the priority of the completion.
    * @param snippet       the snippet for TextEdit.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class SnippetCompletion(name: String, range: Range, priority: Priority, snippet: String, documentation: String) extends Completion

  /**
    * Represents a Magic Def completion
    *
    * @param decl     the declaration.
    * @param label    the label.
    * @param snippet  the snippet.
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    */
  case class MagicDefCompletion(decl: TypedAst.Def, label: String, snippet: String, range: Range, priority: Priority) extends Completion {
    override def toString: String = s"MagicDefCompletion(${decl.sym}, $priority, $range)"
  }

  /**
    * Represents a Snippet completion
    *
    * @param name          the name of the snippet.
    * @param range         the range for TextEdit.
    * @param priority      the priority of the completion.
    * @param snippet       the snippet for TextEdit.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class MagicMatchCompletion(name: String, range: Range, priority: Priority, snippet: String, documentation: String) extends Completion

  /**
    * Represents a Var completion
    *
    * @param name     the name of the variable to complete.
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    */
  case class LocalVarCompletion(name: String, range: Range, priority: Priority) extends Completion {
    override def toString: String = s"LocalVarCompletion($name, $priority, $range)"
  }

  /**
    * Represents a Java Class completion
    *
    * @param name     the name of the java class to complete.
    * @param clazz    the java class to complete.
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    */
  case class LocalJavaClassCompletion(name: String, clazz: Class[?], range: Range, priority: Priority) extends Completion

  /**
    * Represents a local def completion
    *
    * @param sym      the symbol of the local function
    * @param fparams  the formal parameters of the local function
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    */
  case class LocalDefCompletion(sym: Symbol.VarSym, fparams: List[ResolvedAst.FormalParam], range: Range, priority: Priority) extends Completion

  /**
    * Represents a Def completion
    *
    * @param decl      the def decl.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the def is inScope.
    * @param ectx      the expression context.
    */
  case class DefCompletion(decl: TypedAst.Def, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean, ectx: ExprContext) extends Completion {
    override def toString: String = s"DefCompletion(${decl.sym}, $priority, $range)"
  }

  /**
    * Represents an Enum completion
    *
    * @param enm                the enum construct.
    * @param range              the range of the completion
    * @param priority           the priority of the completion.
    * @param ap                 the anchor position for the use statement.
    * @param qualified          indicate whether to use a qualified label.
    * @param inScope            indicate whether to the enum is inScope.
    * @param withTypeParameters indicate whether to include type parameters in the completion.
    */
  case class EnumCompletion(enm: TypedAst.Enum, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean, withTypeParameters: Boolean) extends Completion

  /**
    * Represents a struct completion
    *
    * @param struct    the struct construct.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the enum is inScope.
    */
  case class StructCompletion(struct: TypedAst.Struct, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion

  /**
    * Represents a trait completion
    *
    * @param trt               trait construct.
    * @param range             the range of the completion.
    * @param priority          the priority of the completion.
    * @param ap                the anchor position for the use statement.
    * @param qualified         indicate whether to use a qualified label.
    * @param inScope           indicate whether to the trait is inScope.
    * @param withTypeParameter indicate whether to include the type parameter in the completion.
    */
  case class TraitCompletion(trt: TypedAst.Trait, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean, withTypeParameter: Boolean) extends Completion {
    override def toString: String = s"TraitCompletion(${trt.sym}, $priority, $range)"
  }

  /**
    * Represents a trait completion
    *
    * @param trt       trait construct.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the trait is inScope.
    */
  case class InstanceCompletion(trt: TypedAst.Trait, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion

  /**
    * Represents an Effect completion
    *
    * @param effect    the effect construct.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the effect is inScope.
    */
  case class EffectCompletion(effect: TypedAst.Effect, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion {
    override def toString: String = s"EffectCompletion(${effect.sym}, $priority, $range)"
  }

  /**
    * Represents a handler completion
    *
    * @param effect    the related effect.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the related effect is inScope.
    */
  case class HandlerCompletion(effect: TypedAst.Effect, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion

  /**
    * Represents a TypeAlias completion
    *
    * @param typeAlias the type alias.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the type alias is inScope.
    */
  case class TypeAliasCompletion(typeAlias: TypedAst.TypeAlias, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion

  /**
    * Represents an Op completion
    *
    * @param decl      the operation declaration.
    * @param namespace the namespace of the op, if not provided, we use the fully qualified name.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the op is inScope.
    * @param ectx      the expression context.
    */
  case class OpCompletion(decl: TypedAst.Op, namespace: String, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean, ectx: ExprContext) extends Completion {
    override def toString: String = s"OpCompletion(${decl.sym}, $range)"
  }

  /**
    * Represents an Op Handler completion
    *
    * @param op       the op.
    * @param range    the range of the completion.
    * @param priority the priority of the completion.
    */
  case class OpHandlerCompletion(op: TypedAst.Op, range: Range, priority: Priority) extends Completion

  /**
    * Represents a Signature completion
    *
    * @param decl      the signature declaration.
    * @param namespace the namespace of the signature, if not provided, we use the fully qualified name.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the signature is inScope.
    * @param ectx      the expression context.
    */
  case class SigCompletion(decl: TypedAst.Sig, namespace: String, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean, ectx: ExprContext) extends Completion {
    override def toString: String = s"SigCompletion(${decl.sym}, $priority, $range)"
  }

  /**
    * Represents an Enum Tag completion
    *
    * @param tag       the tag.
    * @param namespace the namespace of the tag, if not provided, we use the fully qualified name.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the signature is inScope.
    * @param ectx      the expression context.
    */
  case class EnumTagCompletion(tag: TypedAst.Case, namespace: String, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean, ectx: ExprContext) extends Completion {
    override def toString: String = s"EnumTagCompletion(${tag.sym}, $priority, $range)"
  }

  /**
    * Represents a Module completion
    *
    * @param module    the module.
    * @param range     the range of the completion.
    * @param priority  the priority of the completion.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the signature is inScope.
    */
  case class ModuleCompletion(module: Symbol.ModuleSym, range: Range, priority: Priority, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion {
    override def toString: String = s"ModuleCompletion($module, $priority, $range)"
  }

  /**
    * Represents a Use completion.
    *
    * @param name               the name of the use completion.
    * @param range              the range of the completion.
    * @param priority           the priority of the completion.
    * @param completionItemKind the kind of the completion.
    */
  case class UseCompletion(name: String, range: Range, priority: Priority, completionItemKind: CompletionItemKind) extends Completion

  /**
    * Represents a struct field completion.
    *
    * @param field    the candidate field.
    * @param priority the priority of the completion.
    */
  case class StructFieldCompletion(field: String, symLoc: SourceLocation, tpe: Type, priority: Priority) extends Completion

  /**
    * Represents a Java field completion.
    *
    * @param ident    the partial field name.
    * @param priority the priority of the completion.
    * @param field    the candidate field.
    */
  case class FieldCompletion(ident: Name.Ident, priority: Priority, field: Field) extends Completion

  /**
    * Represents a Java method completion.
    *
    * @param ident    the partial method name.
    * @param priority the priority of the completion.
    * @param method   the candidate method.
    */
  case class MethodCompletion(ident: Name.Ident, priority: Priority, method: Method) extends Completion

  /**
    * Represents a hole completion.
    *
    * @param sym      the variable symbol being completed on.
    * @param decl     the proposed def declaration to call.
    * @param priority the priority of the completion.
    * @param loc      the source location of the hole.
    */
  case class HoleCompletion(sym: Symbol.VarSym, decl: TypedAst.Def, priority: Priority, loc: SourceLocation) extends Completion

  /**
    * Returns a TextEdit that is inserted and indented according to the given `ap`.
    * This function will:
    *   - add leadingSpaces before the given text.
    *   - add leadingSpaces after each newline.
    *   - add a newline at the end.
    *
    * Example:
    * Given text = "\ndef foo(): =\n", ap = AnchorPosition(line=1, col=0, spaces=4)
    * The result will be:
    * TextEdit(Range(Position(1, 0), Position(1, 0)), "    \n    def foo(): =\n    \n")
    */
  private def mkTextEdit(ap: AnchorPosition, text: String): TextEdit = {
    val insertPosition = Position.fromAnchorPosition(ap)
    val leadingSpaces = " " * ap.spaces
    TextEdit(
      Range(insertPosition, insertPosition),
      leadingSpaces + text.replace("\n", s"\n$leadingSpaces") + "\n"
    )
  }
}
