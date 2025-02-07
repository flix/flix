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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.{CodeActionProvider, HighlightProvider, HoverProvider, SemanticTokensProvider}
import ca.uwaterloo.flix.api.{CrashHandler, Flix}
import ca.uwaterloo.flix.api.lsp.{Position, PublishDiagnosticsParams}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.extra.CodeHinter
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Options
import org.eclipse.lsp4j
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}

import java.util
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object LspServer {
  def run(o: Options): Unit = {
    System.err.println(s"Starting Default LSP Server...")

    val server = new FlixLanguageServer(o)
    val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
    val client = launcher.getRemoteProxy
    server.connect(client)
    launcher.startListening().get()

    System.err.println(s"LSP Server Terminated.")
  }

  private class FlixLanguageServer(o: Options) extends LanguageServer with LanguageClientAware {
    /**
      * The Flix instance (the same instance is used for incremental compilation).
      */
    val flix: Flix = new Flix().setFormatter(NoFormatter).setOptions(o)

    /**
      * A map from source URIs to source code.
      */
    val sources: mutable.Map[String, String] = mutable.Map.empty

    /**
      * The current AST root. The root is null until the source code is compiled.
      */
    var root: Root = TypedAst.empty

    /**
      * The current compilation errors.
      */
    var currentErrors: List[CompilationMessage] = Nil

    /**
      * The proxy to the language client.
      * Used to send messages to the client.
      */
    var flixLanguageClient: LanguageClient = _

    /**
      * The client capabilities.
      * Will be set during the initialization.
      */
    private var clientCapabilities: ClientCapabilities = _

    private val flixTextDocumentService = new FlixTextDocumentService(this, flixLanguageClient)
    private val flixWorkspaceService = new FlixWorkspaceService(this, flixLanguageClient)

    /**
      * Initializes the language server.
      *
      * During the initialization, we should:
      * - Store the client capabilities.
      * - Return the server capabilities.
      */
    override def initialize(initializeParams: InitializeParams): CompletableFuture[InitializeResult] = {
      System.err.println(s"initialize: $initializeParams")

      clientCapabilities = initializeParams.getCapabilities

      CompletableFuture.completedFuture(new InitializeResult(mkServerCapabilities()))
    }

    private def mkServerCapabilities(): ServerCapabilities = {
      val serverCapabilities = new ServerCapabilities
      serverCapabilities.setHoverProvider(true)
      serverCapabilities.setDocumentHighlightProvider(true)
      serverCapabilities.setSemanticTokensProvider(
        new SemanticTokensWithRegistrationOptions(
          new SemanticTokensLegend(
            List("class", "enum", "enumMember", "function", "interface", "operator", "parameter", "property", "method", "namespace", "type", "typeParameter", "variable").asJava,
            List("declaration", "definition").asJava
          ),
          true
        )
      )
      serverCapabilities.setCodeActionProvider(true)
      serverCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)// TODO: make it incremental

      serverCapabilities
    }

    override def shutdown(): CompletableFuture[AnyRef] = {
      System.err.println("shutdown")
      CompletableFuture.completedFuture(new Object)
    }

    override def exit(): Unit = {
      System.err.println("exit")
    }

    override def connect(client: LanguageClient): Unit = {
      System.err.println("connect to the client")
      flixLanguageClient = client
    }

    override def getTextDocumentService: TextDocumentService = flixTextDocumentService

    override def getWorkspaceService: WorkspaceService = flixWorkspaceService

    /**
      * Adds the given source code to the Flix instance.
      */
    def addSourceCode(uri: String, src: String): Unit = {
      flix.addSourceCode(uri, src)(SecurityContext.AllPermissions)
      sources.put(uri, src)
    }

    /**
      * Compile the current source code.
      */
    def processCheck(): Unit = {
      try {
        val diagnostics = flix.check() match {
          // Case 1: Compilation was successful or partially successful so that we have the root and errors.
          case (Some(root), errors) =>
            this.root = root
            this.currentErrors = errors
            // We provide diagnostics for errors and code hints.
            val codeHints = CodeHinter.run(sources.keySet.toSet)(root)
            PublishDiagnosticsParams.fromMessages(currentErrors, flix.options.explain) ::: PublishDiagnosticsParams.fromCodeHints(codeHints)

          // Case 2: Compilation failed so that we have only errors.
          case (None, errors) =>
            this.currentErrors = errors
            // We provide diagnostics only for errors.
            PublishDiagnosticsParams.fromMessages(currentErrors, flix.options.explain)
        }
        publishDiagnostics(diagnostics)
      } catch {
        case ex: Throwable =>
          val reportPath = CrashHandler.handleCrash(ex)(flix)
          flixLanguageClient.showMessage(new MessageParams(MessageType.Error, s"The flix compiler crashed. See the crash report for details:\n${reportPath.map(_.toString)}"))
      }
    }

    /**
      * Publishes the given diagnostics to the client.
      * We need to publish empty diagnostics for sources that do not have any diagnostics to clear previous diagnostics.
      */
    private def publishDiagnostics(diagnostics: List[PublishDiagnosticsParams]): Unit = {
      val sourcesWithDiagnostics = diagnostics.map(d => d.uri).toSet
      val sourcesWithoutDiagnostics = sources.keySet.diff(sourcesWithDiagnostics)
      sourcesWithoutDiagnostics.foreach { source =>
        flixLanguageClient.publishDiagnostics(PublishDiagnosticsParams(source, Nil).toLsp4j)
      }
      diagnostics.foreach { diagnostic =>
        flixLanguageClient.publishDiagnostics(diagnostic.toLsp4j)
      }
    }
  }



  private class FlixTextDocumentService(flixLanguageServer: FlixLanguageServer, flixLanguageClient: LanguageClient) extends TextDocumentService {
    /**
      * Called when a text document is opened.
      * If the document is a Flix source file, we add the source code to the Flix instance and check it.
      */
    override def didOpen(didOpenTextDocumentParams: DidOpenTextDocumentParams): Unit = {
      System.err.println(s"didOpen: $didOpenTextDocumentParams")
      val textDocument = didOpenTextDocumentParams.getTextDocument
      if (textDocument.getLanguageId == "flix") {
        flixLanguageServer.addSourceCode(textDocument.getUri, textDocument.getText)
        flixLanguageServer.processCheck()
      }
    }

    /**
      * Called when a text document is changed.
      * If the document is a Flix source file, we update the source code in the Flix instance and check it.
      */
    override def didChange(didChangeTextDocumentParams: DidChangeTextDocumentParams): Unit = {
      System.err.println(s"didChange: $didChangeTextDocumentParams")
      val uri = didChangeTextDocumentParams.getTextDocument.getUri
      if (flixLanguageServer.sources.contains(uri)) {
        //Since the TextDocumentSyncKind is Full, we can assume that there is only one change that is a full content change.
        val src = didChangeTextDocumentParams.getContentChanges.get(0).getText
        flixLanguageServer.addSourceCode(uri, src)
        flixLanguageServer.processCheck()
      }
    }

    override def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit = {
      System.err.println(s"didCloseTextDocumentParams: $didCloseTextDocumentParams")
    }

    override def didSave(didSaveTextDocumentParams: DidSaveTextDocumentParams): Unit = {
      System.err.println(s"didSaveTextDocumentParams: $didSaveTextDocumentParams")
    }

    override def codeAction(params: CodeActionParams): CompletableFuture[util.List[messages.Either[Command, CodeAction]]] = {
      val uri = params.getTextDocument.getUri
      val range = Range.fromLsp4j(params.getRange)
      val codeActions =
        CodeActionProvider
        .getCodeActions(uri, range, flixLanguageServer.currentErrors)(flixLanguageServer.root)
        .map(_.toLsp4j)
        .map(messages.Either.forRight[Command, CodeAction])
        .asJava
      CompletableFuture.completedFuture(codeActions)
    }

    /**
      * Returns the hover information for the given position in the given document.
      *
      * Now a mock implementation that just returns a simple greeting.
      */
    override def hover(params: HoverParams): CompletableFuture[Hover] = {
      val uri = params.getTextDocument.getUri
      val position = Position.fromLsp4j(params.getPosition)
      val hover = HoverProvider.processHover(uri, position)(flixLanguageServer.root, flixLanguageServer.flix).map(_.toLsp4j).orNull
      CompletableFuture.completedFuture(hover)
    }

    override def documentHighlight(params: DocumentHighlightParams): CompletableFuture[java.util.List[_ <: DocumentHighlight]] = {
      val uri = params.getTextDocument.getUri
      val position = Position.fromLsp4j(params.getPosition)
      val highlights = HighlightProvider.processHighlight(uri, position)(flixLanguageServer.root)
      CompletableFuture.completedFuture(highlights.map(_.toLsp4j).toList.asJava)
    }

    /**
      * Returns the semantic tokens (full) for the given document, which is used to provide semantic highlighting.
      */
    override def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] = {
      val uri = params.getTextDocument.getUri
      val tokens = SemanticTokensProvider.provideSemanticTokens(uri)(flixLanguageServer.root)
      val semanticTokens = new lsp4j.SemanticTokens()
      semanticTokens.setData(tokens.map(Int.box).asJava)
      CompletableFuture.completedFuture(semanticTokens)
    }
  }

  private class FlixWorkspaceService(flixLanguageServer: FlixLanguageServer, flixLanguageClient: LanguageClient) extends WorkspaceService {
    override def didChangeConfiguration(didChangeConfigurationParams: DidChangeConfigurationParams): Unit = {
      System.err.println(s"didChangeConfiguration: $didChangeConfigurationParams")
    }

    override def didChangeWatchedFiles(didChangeWatchedFilesParams: DidChangeWatchedFilesParams): Unit = {
      System.err.println(s"didChangeWatchedFiles: $didChangeWatchedFilesParams")
    }
  }
}
