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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.{Options, StreamOps}
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.{LanguageClient, LanguageClientAware, LanguageServer, TextDocumentService, WorkspaceService}

import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture
import scala.collection.mutable

object LspServer {
  def run(o: Options): Unit = {
    val in = System.in
    val out = System.out
    val server = new FlixLanguageServer(o)
    System.err.println(s"Starting Default LSP Server...")
    val launcher = LSPLauncher.createServerLauncher(server, in, out)
    server.connect(launcher.getRemoteProxy)
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

    var flixLanguageClient: LanguageClient = _

    private var clientCapabilities: ClientCapabilities = _;

    private val flixTextDocumentService = new FlixTextDocumentService(this)
    private val flixWorkspaceService = new FlixWorkspaceService(this)

    override def initialize(initializeParams: InitializeParams): CompletableFuture[InitializeResult] = {
      System.err.println(s"initialize: $initializeParams")

      clientCapabilities = initializeParams.getCapabilities

      val serverCapabilities = new ServerCapabilities
      serverCapabilities.setHoverProvider(true)
      serverCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)

      try {
        loadMain(initializeParams)
      } catch {
        case ex: Throwable =>
          System.err.println(s"Failed to initialize the LSP: ${ex.getMessage}")
          return CompletableFuture.failedFuture(ex)
      }

      CompletableFuture.completedFuture(new InitializeResult(serverCapabilities))
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

    private def loadMain(initializeParams: InitializeParams): Unit = {
      if (initializeParams.getWorkspaceFolders == null || initializeParams.getWorkspaceFolders.isEmpty) {
        System.err.println("No workspace folders.")
        throw new RuntimeException("No workspace folders")
      }
      val mainPath = Paths.get(initializeParams.getWorkspaceFolders.get(0).getName, "Main.flix")
      val mainUri = mainPath.toString
      if (Files.exists(mainPath)) {
        val mainSrc = StreamOps.readAll(Files.newInputStream(mainPath))
        addSourceCode(mainUri, mainSrc)
      } else {
        System.err.println(s"Main file not found: $mainPath")
        throw new RuntimeException("Main file not found")
      }
    }

    private def addSourceCode(uri: String, src: String): Unit = {
      flix.addSourceCode(uri, src)(SecurityContext.AllPermissions)
      sources.put(uri, src)
    }
  }

  private class FlixTextDocumentService(flixLanguageServer: FlixLanguageServer) extends TextDocumentService {
    override def didOpen(didOpenTextDocumentParams: DidOpenTextDocumentParams): Unit = {
      System.err.println(s"didOpen: $didOpenTextDocumentParams")
    }

    override def didChange(didChangeTextDocumentParams: DidChangeTextDocumentParams): Unit = {
      System.err.println(s"didChange: $didChangeTextDocumentParams")
    }

    override def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit = {
      System.err.println(s"didCloseTextDocumentParams: $didCloseTextDocumentParams")
    }

    override def didSave(didSaveTextDocumentParams: DidSaveTextDocumentParams): Unit = {
      System.err.println(s"didSaveTextDocumentParams: $didSaveTextDocumentParams")
    }

    override def hover(params: HoverParams): CompletableFuture[Hover] = {
      System.err.println(s"hover: $params")

      val h = new Hover(new MarkupContent("plaintext", "Hello World from Hover!"))
      CompletableFuture.completedFuture(h)
    }
  }

  private class FlixWorkspaceService(flixLanguageServer: FlixLanguageServer) extends WorkspaceService {
    override def didChangeConfiguration(didChangeConfigurationParams: DidChangeConfigurationParams): Unit = {
      System.err.println(s"didChangeConfiguration: $didChangeConfigurationParams")
    }

    override def didChangeWatchedFiles(didChangeWatchedFilesParams: DidChangeWatchedFilesParams): Unit = {
      System.err.println(s"didChangeWatchedFiles: $didChangeWatchedFilesParams")
    }
  }

}
