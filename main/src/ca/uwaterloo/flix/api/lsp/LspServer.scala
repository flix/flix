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
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.Options
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.{LanguageServer, TextDocumentService, WorkspaceService}

import java.util.concurrent.CompletableFuture

class LspServer(o: Options) {

  /**
    * The Flix instance (the same instance is used for incremental compilation).
    */
  private val flix: Flix = new Flix().setFormatter(NoFormatter).setOptions(o)

  /**
    * The current AST root. The root is null until the source code is compiled.
    */
  private var root: Root = TypedAst.empty

  def run(): Unit = {
    val in = System.in
    val out = System.out
    val server = new FlixLanguageServer
    System.err.println(s"Starting Default LSP Server...")
    LSPLauncher.createServerLauncher(server, in, out).startListening.get()
    System.err.println(s"LSP Server Terminated.")
  }

  private class FlixLanguageServer extends LanguageServer {

    private val flixTextDocumentService = new FlixTextDocumentService
    private val flixWorkspaceService = new FlixWorkspaceService

    override def initialize(initializeParams: InitializeParams): CompletableFuture[InitializeResult] = {
      System.err.println(s"initialize: $initializeParams")

      val capabilities = new ServerCapabilities
      capabilities.setHoverProvider(true)
      CompletableFuture.completedFuture(new InitializeResult(capabilities))
    }

    override def shutdown(): CompletableFuture[AnyRef] = {
      System.err.println("shutdown")
      CompletableFuture.completedFuture(new Object)
    }

    override def exit(): Unit = {
      System.err.println("exit")
    }

    override def getTextDocumentService: TextDocumentService = flixTextDocumentService

    override def getWorkspaceService: WorkspaceService = flixWorkspaceService
  }

  private class FlixTextDocumentService extends TextDocumentService {
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

  private class FlixWorkspaceService extends WorkspaceService {
    override def didChangeConfiguration(didChangeConfigurationParams: DidChangeConfigurationParams): Unit = {
      System.err.println(s"didChangeConfiguration: $didChangeConfigurationParams")
    }

    override def didChangeWatchedFiles(didChangeWatchedFilesParams: DidChangeWatchedFilesParams): Unit = {
      System.err.println(s"didChangeWatchedFiles: $didChangeWatchedFilesParams")
    }
  }

}
