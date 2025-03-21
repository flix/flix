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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.api.lsp.acceptors.InsideAcceptor
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.provider.completion.*
import ca.uwaterloo.flix.api.lsp.provider.completion.semantic.{GetStaticFieldCompleter, InvokeStaticMethodCompleter}
import ca.uwaterloo.flix.api.lsp.provider.completion.syntactic.{ExprSnippetCompleter, KeywordCompleter}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.shared.{SyntacticContext, TraitUsageKind}
import ca.uwaterloo.flix.language.errors.{ParseError, ResolutionError, TypeError, WeederError}

/**
  * CompletionProvider
  *
  * Takes a source file, along with the position of the cursor within that file, and returns a list of CompletionItems.
  *
  * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
  *
  * This list is not displayed to the user as-is, the client always both sorts and filters the list (or at least,
  * VSCode does). Therefore we always need to provide both filterText (currently copied from label) and sortText.
  *
  * Note that we use textEdit rather than insertText to avoid relying on VSCode's tokenisation, so we can ensure that
  * we're consistent with Flix's parser.
  */
object CompletionProvider {

  def autoComplete(uri: String, pos: Position, currentErrors: List[CompilationMessage])(implicit root: Root, flix: Flix): CompletionList = {
      val stack = getStack(uri, pos)
      val undefinedNameContext: UndefinedNameContext = getUndefinedNameContext(stack)
      val items = getCompletions(uri, pos, currentErrors, stack)(root, flix).map(_.toCompletionItem(undefinedNameContext))
      CompletionList(isIncomplete = true, items)
  }

  /**
    * Returns all completions in the given `uri` at the given position `pos`.
    */
  private def getCompletions(uri: String, pos: Position, currentErrors: List[CompilationMessage], stack: List[AnyRef])(implicit root: Root, flix: Flix): List[Completion] = {
    if (currentErrors.isEmpty)
      HoleCompleter.getHoleCompletion(uri, pos, root).toList
    else
      errorsAt(uri, pos, currentErrors).flatMap {
        case err: WeederError.UndefinedAnnotation => KeywordCompleter.getModKeywords(Range.from(err.loc))

        case err: WeederError.UnqualifiedUse => UseCompleter.getCompletions(err.qn, Range.from(err.loc))

        case err: ResolutionError.UndefinedTag =>
          val ap = err.ap
          val scp = err.scp
          val qn = err.qn
          val range = Range.from(err.loc)
          EnumCompleter.getCompletions(qn, range, ap, scp, withTypeParameters = false) ++
            EnumTagCompleter.getCompletions(qn, range, ap, scp) ++
            ModuleCompleter.getCompletions(qn, range, ap, scp)

        case err: ResolutionError.UndefinedName =>
          val ap = err.ap
          val scp = err.scp
          val ident = err.qn.ident.name
          val qn = err.qn
          val range = Range.from(err.loc)
          AutoImportCompleter.getCompletions(ident, range, ap, scp) ++
            LocalScopeCompleter.getCompletionsExpr(range, scp) ++
            KeywordCompleter.getExprKeywords(range) ++
            DefCompleter.getCompletions(qn, range, ap, scp) ++
            EnumCompleter.getCompletions(qn, range, ap, scp, withTypeParameters = false) ++
            EffectCompleter.getCompletions(qn, range, ap, scp, inHandler = false) ++
            OpCompleter.getCompletions(qn, range, ap, scp) ++
            SignatureCompleter.getCompletions(qn, range, ap, scp) ++
            EnumTagCompleter.getCompletions(qn, range, ap, scp) ++
            TraitCompleter.getCompletions(qn, TraitUsageKind.Expr, range, ap, scp) ++
            ModuleCompleter.getCompletions(qn, range, ap, scp)

        case err: ResolutionError.UndefinedType =>
          val ap = err.ap
          val scp = err.scp
          val ident = err.qn.ident.name
          val qn = err.qn
          val range = Range.from(err.loc)
          TypeBuiltinCompleter.getCompletions(range) ++
            AutoImportCompleter.getCompletions(ident, range, ap, scp) ++
            LocalScopeCompleter.getCompletionsType(range, scp) ++
            EnumCompleter.getCompletions(qn, range, ap, scp, withTypeParameters = true) ++
            StructCompleter.getCompletions(qn, range, ap, scp) ++
            EffectCompleter.getCompletions(qn, range, ap, scp, inHandler = false) ++
            TypeAliasCompleter.getCompletions(qn, range, ap, scp) ++
            ModuleCompleter.getCompletions(qn, range, ap, scp)

        case err: ResolutionError.UndefinedEffect => EffectCompleter.getCompletions(err.qn, Range.from(err.loc), err.ap, err.scp, inHandler = true)
        case err: ResolutionError.UndefinedJvmImport => ImportCompleter.getCompletions(err.name, Range.from(err.loc))
        case err: ResolutionError.UndefinedJvmStaticField => GetStaticFieldCompleter.getCompletions(err.clazz, err.field) ++ InvokeStaticMethodCompleter.getCompletions(err.clazz, err.field)
        case err: ResolutionError.UndefinedKind => KindCompleter.getCompletions(err.qn.ident.name, Range.from(err.loc))
        case err: ResolutionError.UndefinedOp => HandlerCompleter.getCompletions(err.qn, Range.from(err.loc))
        case err: ResolutionError.UndefinedStructField => StructFieldCompleter.getCompletions(err, root)
        case err: ResolutionError.UndefinedTrait => TraitCompleter.getCompletions(err.qn, err.traitUseKind, Range.from(err.loc), err.ap, err.scp)
        case err: ResolutionError.UndefinedUse => UseCompleter.getCompletions(err.qn, Range.from(err.loc))

        case err: TypeError.FieldNotFound => MagicMatchCompleter.getCompletions(err.tpe, Range.from(err.loc), err.base) ++ InvokeMethodCompleter.getCompletions(err.tpe, err.fieldName)
        case err: TypeError.MethodNotFound => InvokeMethodCompleter.getCompletions(err.tpe, err.methodName)

        case err: ParseError => getSyntacticCompletions(uri, err)

        case _ => Nil
      }
  }

  /**
    * Returns completions based on the syntactic context.
    */
  private def getSyntacticCompletions(uri: String, e: ParseError)(implicit root: Root, flix: Flix): List[Completion] = {
    val range: Range = Range.from(e.loc)
    if (range.isEmpty)
      Nil
    else e.sctx match {
      // Expressions.
      case SyntacticContext.Expr.Constraint => (PredicateCompleter.getCompletions(uri, range) ++ KeywordCompleter.getConstraintKeywords(range)).toList
      case SyntacticContext.Expr.OtherExpr => KeywordCompleter.getExprKeywords(range)

      // Declarations.
      case SyntacticContext.Decl.Enum => KeywordCompleter.getEnumKeywords(range)
      case SyntacticContext.Decl.Effect => KeywordCompleter.getEffectKeywords(range)
      case SyntacticContext.Decl.Instance => KeywordCompleter.getInstanceKeywords(range)
      case SyntacticContext.Decl.Module => KeywordCompleter.getModKeywords(range) ++ ExprSnippetCompleter.getCompletions(range)
      case SyntacticContext.Decl.Struct => KeywordCompleter.getStructKeywords(range)
      case SyntacticContext.Decl.Trait => KeywordCompleter.getTraitKeywords(range)
      case SyntacticContext.Decl.Type => KeywordCompleter.getTypeKeywords(range)

      case SyntacticContext.Unknown => Nil
    }
  }

  /**
    * Returns the undefined name context from the stack.
    *
    * Transfer:
    *   Start -- UndefinedName --> CanBeApplied
    *   CanBeApplied -- ApplyClo --> CanBeApplied
    *   CanBeApplied -- ApplyDef --> Done
    *   AnyState -- Anything --> Done
    */
  private def getUndefinedNameContext(stack: List[AnyRef]): UndefinedNameContext = {
    stack.foldLeft((UndefinedNameContextState.Start: UndefinedNameContextState, UndefinedNameContext(None, pipelined = None))) {
      case ((UndefinedNameContextState.Start, ctx), TypedAst.Expr.Error(_: ResolutionError.UndefinedName, _, _)) => (UndefinedNameContextState.CanBeApplied, ctx)
      case ((UndefinedNameContextState.CanBeApplied, ctx), _: TypedAst.Expr.ApplyClo) => (UndefinedNameContextState.CanBeApplied, ctx.copy(applied = Some(ctx.applied.getOrElse(0) + 1)))
      case ((UndefinedNameContextState.CanBeApplied, ctx), TypedAst.Expr.ApplyDef(DefSymUse(sym, _), _, _, _, _, _)) =>
        sym.text match {
          case "|>" | "!>" => (UndefinedNameContextState.Done, ctx.copy(pipelined = Some(1)))
          case "||>" => (UndefinedNameContextState.Done, ctx.copy(pipelined = Some(2)))
          case _ => (UndefinedNameContextState.Done, ctx)
        }
      case ((_, ctx), _) => (UndefinedNameContextState.Done, ctx) // Jump to the final state in other cases
    }._2
  }

  /**
    * Returns the stack of AST nodes at the given position.
    *
    * Every AST node that includes the given position is pushed onto the stack.
    * Represented as a list of AnyRef. The head of the list is the innermost AST node.
    */
  private def getStack(uri: String, pos: Position)(implicit root: Root): List[AnyRef] = {
    val stack = StackConsumer()

    if (pos.character >= 2) {
      val leftPos = Position(pos.line, pos.character - 1)
      Visitor.visitRoot(root, stack, InsideAcceptor(uri, leftPos))
    }

    stack.getStack
  }

  /**
    * Filters the list of errors to only those that occur at the given position.
    */
  private def errorsAt(uri: String, pos: Position, errors: List[CompilationMessage]): List[CompilationMessage] =
    errors.filter(err => uri == err.loc.source.name && pos.line <= err.loc.beginLine)
}
