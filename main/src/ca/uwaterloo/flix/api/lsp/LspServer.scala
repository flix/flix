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

import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.launch.LSPLauncher
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

    private val txtDocService = new FlixTextDocumentService
    private val wrkSpaceService = new FlixWorkspaceService

    override def initialize(initializeParams: InitializeParams): CompletableFuture[InitializeResult] = {
      System.err.println(s"initialize: $initializeParams")

      val capabilities = new ServerCapabilities
      CompletableFuture.completedFuture(new InitializeResult(capabilities))
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
