/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.jsonrpc.messages.Either3
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.{
  CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem, CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams, CodeActionParams, CodeLensParams, ColorInformation, ColorPresentation, ColorPresentationParams, CompletionParams, CreateFilesParams, DeclarationParams, DefinitionParams, DeleteFilesParams, DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidChangeWorkspaceFoldersParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentColorParams, DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentFormattingParams, DocumentHighlightParams, DocumentLink, DocumentLinkParams, DocumentOnTypeFormattingParams, DocumentRangeFormattingParams, DocumentSymbolParams, ExecuteCommandParams, FoldingRange, FoldingRangeRequestParams, Hover, HoverParams, ImplementationParams, InitializeParams, InitializeResult, InitializedParams, InlayHintParams, InlineValue, InlineValueParams, LinkedEditingRangeParams, LinkedEditingRanges, Moniker, MonikerParams, PrepareRenameDefaultBehavior, PrepareRenameParams, PrepareRenameResult, ReferenceParams, RenameFilesParams, RenameParams, SelectionRange, SelectionRangeParams, SemanticTokens, SemanticTokensDelta, SemanticTokensDeltaParams, SemanticTokensParams, SemanticTokensRangeParams, SetTraceParams, SignatureHelp, SignatureHelpParams, TypeDefinitionParams, TypeHierarchyItem, TypeHierarchyPrepareParams, TypeHierarchySubtypesParams, TypeHierarchySupertypesParams, WillSaveTextDocumentParams, WorkDoneProgressCancelParams, WorkspaceDiagnosticParams, WorkspaceDiagnosticReport, WorkspaceSymbol, WorkspaceSymbolParams
}
import org.eclipse.lsp4j.services.{NotebookDocumentService, TextDocumentService, WorkspaceService}

import java.util
import java.util.concurrent.CompletableFuture

object LspServer {

  def run(): Unit = {
    val in = System.in
    val out = System.out

    val server = new Server
    LSPLauncher.createServerLauncher(server, in, out).startListening
  }

  private class Server() extends org.eclipse.lsp4j.services.LanguageServer {

    private val txtDocService = new TxtDocService
    private val wrkSpaceService = new WrkSpaceService

    override def initialize(initializeParams: InitializeParams): CompletableFuture[InitializeResult] = {
      System.err.println("initialize")

      CompletableFuture.completedFuture(new InitializeResult())
    }

    override def shutdown(): CompletableFuture[AnyRef] = {
      System.err.println("shutdown")
      CompletableFuture.completedFuture(new Object)
    }

    override def exit(): Unit = {
      System.err.println("exit")
    }

    override def getTextDocumentService: TextDocumentService = txtDocService

    override def getWorkspaceService: WorkspaceService = wrkSpaceService

    override def initialized(params: InitializedParams): Unit = super.initialized(params)

    override def getNotebookDocumentService: NotebookDocumentService = super.getNotebookDocumentService

    override def cancelProgress(params: WorkDoneProgressCancelParams): Unit = super.cancelProgress(params)

    override def setTrace(params: SetTraceParams): Unit = super.setTrace(params)
  }

  private class TxtDocService extends TextDocumentService {
    @JsonRequest(useSegment = false)
    override def didOpen(didOpenTextDocumentParams: DidOpenTextDocumentParams): Unit = {
      System.err.println(s"didOpen $didOpenTextDocumentParams")
    }

    @JsonRequest(useSegment = false)
    override def didChange(didChangeTextDocumentParams: DidChangeTextDocumentParams): Unit = {
      System.err.println(s"didChange $didChangeTextDocumentParams")
    }

    @JsonRequest(useSegment = false)
    override def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit = {
      System.err.println(s"didClose $didCloseTextDocumentParams")
    }

    @JsonRequest(useSegment = false)
    override def didSave(didSaveTextDocumentParams: DidSaveTextDocumentParams): Unit = {
      System.err.println(s"didSave $didSaveTextDocumentParams")
    }

    @JsonRequest(useSegment = false)
    override def completion(position: CompletionParams): CompletableFuture[messages.Either[util.List[lsp4j.CompletionItem], lsp4j.CompletionList]] = super.completion(position)

    @JsonRequest(useSegment = false)
    override def resolveCompletionItem(unresolved: lsp4j.CompletionItem): CompletableFuture[lsp4j.CompletionItem] = super.resolveCompletionItem(unresolved)

    @JsonRequest(useSegment = false)
    override def hover(params: HoverParams): CompletableFuture[Hover] = super.hover(params)

    @JsonRequest(useSegment = false)
    override def signatureHelp(params: SignatureHelpParams): CompletableFuture[SignatureHelp] = super.signatureHelp(params)

    @JsonRequest(useSegment = false)
    override def declaration(params: DeclarationParams): CompletableFuture[messages.Either[util.List[_ <: lsp4j.Location], util.List[_ <: lsp4j.LocationLink]]] = super.declaration(params)

    @JsonRequest(useSegment = false)
    override def definition(params: DefinitionParams): CompletableFuture[messages.Either[util.List[_ <: lsp4j.Location], util.List[_ <: lsp4j.LocationLink]]] = super.definition(params)

    @JsonRequest(useSegment = false)
    override def typeDefinition(params: TypeDefinitionParams): CompletableFuture[messages.Either[util.List[_ <: lsp4j.Location], util.List[_ <: lsp4j.LocationLink]]] = super.typeDefinition(params)

    @JsonRequest(useSegment = false)
    override def implementation(params: ImplementationParams): CompletableFuture[messages.Either[util.List[_ <: lsp4j.Location], util.List[_ <: lsp4j.LocationLink]]] = super.implementation(params)

    @JsonRequest(useSegment = false)
    override def references(params: ReferenceParams): CompletableFuture[util.List[_ <: lsp4j.Location]] = super.references(params)

    @JsonRequest(useSegment = false)
    override def documentHighlight(params: DocumentHighlightParams): CompletableFuture[util.List[_ <: lsp4j.DocumentHighlight]] = super.documentHighlight(params)

    @JsonRequest(useSegment = false)
    override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[util.List[messages.Either[lsp4j.SymbolInformation, lsp4j.DocumentSymbol]]] = super.documentSymbol(params)

    @JsonRequest(useSegment = false)
    override def codeAction(params: CodeActionParams): CompletableFuture[util.List[messages.Either[lsp4j.Command, lsp4j.CodeAction]]] = super.codeAction(params)

    @JsonRequest(useSegment = false)
    override def resolveCodeAction(unresolved: lsp4j.CodeAction): CompletableFuture[lsp4j.CodeAction] = super.resolveCodeAction(unresolved)

    @JsonRequest(useSegment = false)
    override def codeLens(params: CodeLensParams): CompletableFuture[util.List[_ <: lsp4j.CodeLens]] = super.codeLens(params)

    @JsonRequest(useSegment = false)
    override def resolveCodeLens(unresolved: lsp4j.CodeLens): CompletableFuture[lsp4j.CodeLens] = super.resolveCodeLens(unresolved)

    @JsonRequest(useSegment = false)
    override def formatting(params: DocumentFormattingParams): CompletableFuture[util.List[_ <: lsp4j.TextEdit]] = super.formatting(params)

    @JsonRequest(useSegment = false)
    override def rangeFormatting(params: DocumentRangeFormattingParams): CompletableFuture[util.List[_ <: lsp4j.TextEdit]] = super.rangeFormatting(params)

    @JsonRequest(useSegment = false)
    override def onTypeFormatting(params: DocumentOnTypeFormattingParams): CompletableFuture[util.List[_ <: lsp4j.TextEdit]] = super.onTypeFormatting(params)

    @JsonRequest(useSegment = false)
    override def rename(params: RenameParams): CompletableFuture[lsp4j.WorkspaceEdit] = super.rename(params)

    @JsonRequest(useSegment = false)
    override def linkedEditingRange(params: LinkedEditingRangeParams): CompletableFuture[LinkedEditingRanges] = super.linkedEditingRange(params)

    @JsonRequest(useSegment = false)
    override def willSave(params: WillSaveTextDocumentParams): Unit = super.willSave(params)

    @JsonRequest(useSegment = false)
    override def willSaveWaitUntil(params: WillSaveTextDocumentParams): CompletableFuture[util.List[lsp4j.TextEdit]] = super.willSaveWaitUntil(params)

    @JsonRequest(useSegment = false)
    override def documentLink(params: DocumentLinkParams): CompletableFuture[util.List[DocumentLink]] = super.documentLink(params)

    @JsonRequest(useSegment = false)
    override def documentLinkResolve(params: DocumentLink): CompletableFuture[DocumentLink] = super.documentLinkResolve(params)

    @JsonRequest(useSegment = false)
    override def documentColor(params: DocumentColorParams): CompletableFuture[util.List[ColorInformation]] = super.documentColor(params)

    @JsonRequest(useSegment = false)
    override def colorPresentation(params: ColorPresentationParams): CompletableFuture[util.List[ColorPresentation]] = super.colorPresentation(params)

    @JsonRequest(useSegment = false)
    override def foldingRange(params: FoldingRangeRequestParams): CompletableFuture[util.List[FoldingRange]] = super.foldingRange(params)

    @JsonRequest(useSegment = false)
    override def prepareRename(params: PrepareRenameParams): CompletableFuture[Either3[lsp4j.Range, PrepareRenameResult, PrepareRenameDefaultBehavior]] = super.prepareRename(params)

    @JsonRequest(useSegment = false)
    override def prepareTypeHierarchy(params: TypeHierarchyPrepareParams): CompletableFuture[util.List[TypeHierarchyItem]] = super.prepareTypeHierarchy(params)

    @JsonRequest(useSegment = false)
    override def typeHierarchySupertypes(params: TypeHierarchySupertypesParams): CompletableFuture[util.List[TypeHierarchyItem]] = super.typeHierarchySupertypes(params)

    @JsonRequest(useSegment = false)
    override def typeHierarchySubtypes(params: TypeHierarchySubtypesParams): CompletableFuture[util.List[TypeHierarchyItem]] = super.typeHierarchySubtypes(params)

    @JsonRequest(useSegment = false)
    override def prepareCallHierarchy(params: CallHierarchyPrepareParams): CompletableFuture[util.List[CallHierarchyItem]] = super.prepareCallHierarchy(params)

    @JsonRequest(useSegment = false)
    override def callHierarchyIncomingCalls(params: CallHierarchyIncomingCallsParams): CompletableFuture[util.List[CallHierarchyIncomingCall]] = super.callHierarchyIncomingCalls(params)

    @JsonRequest(useSegment = false)
    override def callHierarchyOutgoingCalls(params: CallHierarchyOutgoingCallsParams): CompletableFuture[util.List[CallHierarchyOutgoingCall]] = super.callHierarchyOutgoingCalls(params)

    @JsonRequest(useSegment = false)
    override def selectionRange(params: SelectionRangeParams): CompletableFuture[util.List[SelectionRange]] = super.selectionRange(params)

    @JsonRequest(useSegment = false)
    override def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] = super.semanticTokensFull(params)

    @JsonRequest(useSegment = false)
    override def semanticTokensFullDelta(params: SemanticTokensDeltaParams): CompletableFuture[messages.Either[SemanticTokens, SemanticTokensDelta]] = super.semanticTokensFullDelta(params)

    @JsonRequest(useSegment = false)
    override def semanticTokensRange(params: SemanticTokensRangeParams): CompletableFuture[SemanticTokens] = super.semanticTokensRange(params)

    @JsonRequest(useSegment = false)
    override def moniker(params: MonikerParams): CompletableFuture[util.List[Moniker]] = super.moniker(params)

    @JsonRequest(useSegment = false)
    override def inlayHint(params: InlayHintParams): CompletableFuture[util.List[lsp4j.InlayHint]] = super.inlayHint(params)

    @JsonRequest(useSegment = false)
    override def resolveInlayHint(unresolved: lsp4j.InlayHint): CompletableFuture[lsp4j.InlayHint] = super.resolveInlayHint(unresolved)

    @JsonRequest(useSegment = false)
    override def inlineValue(params: InlineValueParams): CompletableFuture[util.List[InlineValue]] = super.inlineValue(params)

    @JsonRequest(useSegment = false)
    override def diagnostic(params: DocumentDiagnosticParams): CompletableFuture[DocumentDiagnosticReport] = super.diagnostic(params)
  }

  private class WrkSpaceService extends WorkspaceService {
    @JsonRequest(useSegment = false)
    override def didChangeConfiguration(didChangeConfigurationParams: DidChangeConfigurationParams): Unit = {
      System.err.println(s"didChangeConfiguration $didChangeConfigurationParams")
    }

    @JsonRequest(useSegment = false)
    override def didChangeWatchedFiles(didChangeWatchedFilesParams: DidChangeWatchedFilesParams): Unit = {
      System.err.println(s"didChangeWatchedFiles $didChangeWatchedFilesParams")
    }

    @JsonRequest(useSegment = false)
    override def executeCommand(params: ExecuteCommandParams): CompletableFuture[AnyRef] = super.executeCommand(params)

    @JsonRequest(useSegment = false)
    override def symbol(params: WorkspaceSymbolParams): CompletableFuture[messages.Either[util.List[_ <: lsp4j.SymbolInformation], util.List[_ <: WorkspaceSymbol]]] = super.symbol(params)

    @JsonRequest(useSegment = false)
    override def resolveWorkspaceSymbol(workspaceSymbol: WorkspaceSymbol): CompletableFuture[WorkspaceSymbol] = super.resolveWorkspaceSymbol(workspaceSymbol)

    @JsonRequest(useSegment = false)
    override def didChangeWorkspaceFolders(params: DidChangeWorkspaceFoldersParams): Unit = super.didChangeWorkspaceFolders(params)

    @JsonRequest(useSegment = false)
    override def willCreateFiles(params: CreateFilesParams): CompletableFuture[lsp4j.WorkspaceEdit] = super.willCreateFiles(params)

    @JsonRequest(useSegment = false)
    override def didCreateFiles(params: CreateFilesParams): Unit = super.didCreateFiles(params)

    @JsonRequest(useSegment = false)
    override def willRenameFiles(params: RenameFilesParams): CompletableFuture[lsp4j.WorkspaceEdit] = super.willRenameFiles(params)

    @JsonRequest(useSegment = false)
    override def didRenameFiles(params: RenameFilesParams): Unit = super.didRenameFiles(params)

    @JsonRequest(useSegment = false)
    override def willDeleteFiles(params: DeleteFilesParams): CompletableFuture[lsp4j.WorkspaceEdit] = super.willDeleteFiles(params)

    @JsonRequest(useSegment = false)
    override def didDeleteFiles(params: DeleteFilesParams): Unit = super.didDeleteFiles(params)

    @JsonRequest(useSegment = false)
    override def diagnostic(params: WorkspaceDiagnosticParams): CompletableFuture[WorkspaceDiagnosticReport] = super.diagnostic(params)
  }

}
