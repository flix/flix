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

import ca.uwaterloo.flix.api.lsp.provider.*
import ca.uwaterloo.flix.api.lsp.{ClientUri, CompletionList, FormattingOptions, Position, PublishDiagnosticsParams, Range}
import ca.uwaterloo.flix.api.{CrashHandler, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.extra.CodeHinter
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.{FileOps, Options, Result}
import org.eclipse.lsp4j
import org.eclipse.lsp4j.*
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.*

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util
import java.util.concurrent.CompletableFuture
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object LspServer {
  def run(opts: Options): Unit = {
    System.err.println(s"Starting Default LSP Server...")

    // Explicitly ensure that the progressbar is disabled.
    val o = opts.copy(progress = false)

    val server = new FlixLanguageServer(o)
    val launcher = LSPLauncher.createServerLauncher(server, System.in, System.out)
    val client = launcher.getRemoteProxy
    server.connect(client)
    launcher.startListening().get()

    System.err.println(s"LSP Server Terminated.")
  }

  /**
    * The trigger characters for completion.
    * By default, the client will only trigger completion requests on [a-zA-Z].
    * These are the additional trigger characters.
    */
  private val TriggerChars = List("#", ".", "/", "?")

  /**
    * The glob pattern of the JARs and packages watched for changes.
    */
  private val DependencyGlob = "**/lib/**/*.{jar,fpkg}"

  /**
    * The id of the registration of the dependency watcher.
    */
  private val DependencyWatcherId = "flix/dependencies"

  private class FlixLanguageServer(o: Options) extends LanguageServer with LanguageClientAware {
    /**
      * The Flix instance (the same instance is used for incremental compilation).
      *
      * The JARs and packages are fixed for the lifetime of an instance, so the instance is
      * replaced during [[initialize]] and whenever a JAR or package changes (see [[processCheck]]).
      */
    var flix: Flix = new Flix().setFormatter(NoFormatter).setOptions(o)

    /**
      * The workspace folders that exist on disk.
      */
    private var workspaceRoots: List[Path] = Nil

    /**
      * Whether a JAR or package changed since [[flix]] was constructed.
      */
    private var dependenciesChanged: Boolean = false

    /**
      * A map from source URIs to source code.
      */
    val sources: mutable.Map[URI, String] = mutable.Map.empty

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
    private var flixLanguageClient: LanguageClient = _

    /**
      * The client capabilities.
      * Will be set during the initialization.
      */
    private var clientCapabilities: ClientCapabilities = _

    private val flixTextDocumentService = new FlixTextDocumentService(this)
    private val flixWorkspaceService = new FlixWorkspaceService(this)

    /**
      * Initializes the language server.
      *
      * During the initialization, we should:
      * - Store the client capabilities.
      * - Load all Flix resources, including source files JAR files and flix package files.
      * - Return the server capabilities.
      */
    override def initialize(initializeParams: InitializeParams): CompletableFuture[InitializeResult] = {
      System.err.println(s"initialize: $initializeParams")

      clientCapabilities = initializeParams.getCapabilities
      if (initializeParams.getWorkspaceFolders != null)
        loadFlixProject(initializeParams.getWorkspaceFolders.asScala.toList)
      else {
        flixLanguageClient.showMessage(new MessageParams(MessageType.Error, "Please provide WorkspaceFolders in the initialization options."))
        System.err.println("Please provide WorkspaceFolders in the initialization options.")
      }

      CompletableFuture.completedFuture(new InitializeResult(mkServerCapabilities()))
    }

    /**
      * Registers a file watcher for the JARs and packages under `lib/`, if the client supports dynamic
      * registration. The client then reports changes through `didChangeWatchedFiles`.
      */
    override def initialized(params: InitializedParams): Unit = {
      if (supportsWatchedFilesRegistration) {
        val watcher = new FileSystemWatcher(messages.Either.forLeft(DependencyGlob))
        val options = new DidChangeWatchedFilesRegistrationOptions(List(watcher).asJava)
        val registration = new Registration(DependencyWatcherId, "workspace/didChangeWatchedFiles", options)
        flixLanguageClient.registerCapability(new RegistrationParams(List(registration).asJava))
      } else {
        System.err.println("The client does not support dynamic registration of file watchers: changes to JARs and packages are not detected.")
      }
    }

    /**
      * Returns `true` if the client supports dynamic registration of `didChangeWatchedFiles`.
      */
    private def supportsWatchedFilesRegistration: Boolean = {
      val workspace = clientCapabilities.getWorkspace
      workspace != null &&
        workspace.getDidChangeWatchedFiles != null &&
        java.lang.Boolean.TRUE.equals(workspace.getDidChangeWatchedFiles.getDynamicRegistration)
    }

    /**
      * Loads all Flix resources in the workspace, including:
      *   - JAR files (lib/**/*.jar).
      *   - Flix package files (lib/**/*.fpkg).
      *   - Flix source files (*.flix, src/**/*.flix, test/**/*.flix).
      *
      * The JARs and packages are fixed for the lifetime of a Flix instance, so the instance is
      * constructed once they are known, and the source files are added afterwards.
      */
    private def loadFlixProject(roots: List[WorkspaceFolder]): Unit = {
      val paths = mutable.ArrayBuffer.empty[Path]
      for (root <- roots) {
        val path = Paths.get(root.getName)
        if (Files.exists(path) && Files.isDirectory(path)) {
          paths += path
        }
      }
      workspaceRoots = paths.toList

      flix.close()
      flix = mkFlix()

      for (path <- workspaceRoots) {
        loadFlixSources(path)
      }
    }

    /**
      * Returns the JARs and packages under `lib/` of every workspace root.
      *
      * A file that is not a valid JAR or package (for example one that is still being written) is skipped.
      */
    private def scanDependencies(): (List[(Path, SecurityContext)], List[Path]) = {
      val pkgs = mutable.ArrayBuffer.empty[(Path, SecurityContext)]
      val jars = mutable.ArrayBuffer.empty[Path]
      for (path <- workspaceRoots) {
        for (p <- FileOps.getFilesIn(path.resolve("lib"), Int.MaxValue)) {
          if (FileOps.checkExt(p, "jar")) {
            FileOps.isValidJarFile(p) match {
              case Result.Ok(()) => jars += p
              case Result.Err(ex) => System.err.println(s"Skipping JAR: ${ex.getMessage}")
            }
          } else if (FileOps.checkExt(p, "fpkg")) {
            FileOps.isValidFpkgFile(p) match {
              case Result.Ok(()) => pkgs += (p -> SecurityContext.Unrestricted)
              case Result.Err(ex) => System.err.println(s"Skipping package: ${ex.getMessage}")
            }
          }
        }
      }
      (pkgs.toList, jars.toList)
    }

    /**
      * Returns a new Flix instance with the current JARs and packages under `lib/` and the current [[sources]].
      */
    private def mkFlix(): Flix = {
      val (pkgs, jars) = scanDependencies()
      val flix = new Flix(pkgs = pkgs, jars = jars).setFormatter(NoFormatter).setOptions(o)
      for ((uri, src) <- sources) {
        flix.addSource(uri, src, SecurityContext.Unrestricted)
      }
      flix
    }

    /**
      * Records that a JAR or package changed and re-checks the project with a new Flix instance.
      */
    def onDependencyChange(): Unit = {
      dependenciesChanged = true
      processCheck()
    }

    /**
      * Loads all Flix source files in the workspace. including:
      *   - *.flix
      *   - src/**/*.flix
      *   - test/**/*.flix
      */
    private def loadFlixSources(path: Path): Unit = {
      val flixSources =
        FileOps.getFilesIn(path, 1) ++
        FileOps.getFilesIn(path.resolve("src"), Int.MaxValue) ++
        FileOps.getFilesIn(path.resolve("test"), Int.MaxValue)

      flixSources.foreach { case p =>
        if (FileOps.checkExt(p, "flix")) {
          val source = Files.readString(p)
          addUri(p.toUri, source)
        }
      }
    }

    private def mkServerCapabilities(): ServerCapabilities = {
      val serverCapabilities = new ServerCapabilities
      serverCapabilities.setHoverProvider(true)
      serverCapabilities.setDocumentHighlightProvider(true)
      serverCapabilities.setSemanticTokensProvider(
        new SemanticTokensWithRegistrationOptions(
          new SemanticTokensLegend(
            SemanticTokenType.getWholeList.asJava,
            SemanticTokenModifier.getWholeList.asJava
          ),
          true
        )
      )
      serverCapabilities.setSignatureHelpProvider(new SignatureHelpOptions(List("(", ",").asJava))
      serverCapabilities.setCodeActionProvider(true)
      serverCapabilities.setCodeLensProvider(new CodeLensOptions(true))
      serverCapabilities.setCompletionProvider(new CompletionOptions(true, TriggerChars.asJava))
      serverCapabilities.setReferencesProvider(true)
      serverCapabilities.setDefinitionProvider(true)
      serverCapabilities.setImplementationProvider(true)
      serverCapabilities.setRenameProvider(new RenameOptions(false))
      serverCapabilities.setDocumentSymbolProvider(true)
      serverCapabilities.setWorkspaceSymbolProvider(true)
      serverCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)// TODO: make it incremental
      serverCapabilities.setDocumentFormattingProvider(true)
      serverCapabilities.setFoldingRangeProvider(true)

      serverCapabilities
    }

    override def shutdown(): CompletableFuture[AnyRef] = {
      System.err.println("shutdown")
      flix.close()
      CompletableFuture.completedFuture(null)
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
    def addUri(uri: URI, src: String): Unit = {
      flix.addSource(uri, src, SecurityContext.Unrestricted)
      sources.put(uri, src)
    }

    /**
      * Compile the current source code.
      */
    def processCheck(): Unit = {
      try {
        // The JARs and packages are fixed for the lifetime of a Flix instance: if they changed, replace the instance.
        if (dependenciesChanged) {
          flix.close()
          flix = mkFlix()
          dependenciesChanged = false
        }

        val diagnostics = flix.check() match {
          // Case 1: Compilation was successful or partially successful so that we have the root and errors.
          case (Some(root1), errors) =>
            this.root = root1
            this.currentErrors = errors
            // We provide diagnostics for errors and code hints.
            val codeHints = CodeHinter.run(sources.keysIterator.map(_.toString).toSet)(root1)
            PublishDiagnosticsParams.fromMessages(currentErrors, Some(this.root)) ::: PublishDiagnosticsParams.fromCodeHints(codeHints)

          // Case 2: Compilation failed so that we have only errors.
          case (None, errors) =>
            this.currentErrors = errors
            // We provide diagnostics only for errors.
            PublishDiagnosticsParams.fromMessages(currentErrors, None)
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
      // We do not publish diagnostics for errors from the library.
      // Merge entries with the same URI so that errors and code hints for the same file are not published separately
      // (each publishDiagnostics call replaces previous diagnostics for that URI in the LSP protocol).
      val validDiagnostics = PublishDiagnosticsParams.merge(diagnostics.filter(_.uri.startsWith("file://")))
      val sourcesWithDiagnostics = validDiagnostics.map(d => d.uri).toSet
      val sourcesWithoutDiagnostics = sources.keysIterator.map(_.toString).toSet.diff(sourcesWithDiagnostics)
      sourcesWithoutDiagnostics.foreach { source =>
        flixLanguageClient.publishDiagnostics(PublishDiagnosticsParams(source, Nil).toLsp4j)
      }
      validDiagnostics.foreach { diagnostic =>
        flixLanguageClient.publishDiagnostics(diagnostic.toLsp4j)
      }
    }
  }



  private class FlixTextDocumentService(flixLanguageServer: FlixLanguageServer) extends TextDocumentService {
    /**
      * Called when a text document is opened.
      * If the document is a Flix source file, we add the source code to the Flix instance and check it.
      */
    override def didOpen(didOpenTextDocumentParams: DidOpenTextDocumentParams): Unit = {
      System.err.println(s"didOpen: $didOpenTextDocumentParams")
      val textDocument = didOpenTextDocumentParams.getTextDocument
      if (textDocument.getLanguageId == "flix") {
        ClientUri.shadow(textDocument.getUri)
        val uri = new URI(textDocument.getUri)
        flixLanguageServer.addUri(uri, textDocument.getText)
        flixLanguageServer.processCheck()
      }
    }

    /**
      * Called when a text document is changed.
      * If the document is a Flix source file, we update the source code in the Flix instance and check it.
      */
    override def didChange(didChangeTextDocumentParams: DidChangeTextDocumentParams): Unit = {
      System.err.println(s"didChange: $didChangeTextDocumentParams")
      ClientUri.shadow(didChangeTextDocumentParams.getTextDocument.getUri)
      val uri = new URI(didChangeTextDocumentParams.getTextDocument.getUri)
      if (flixLanguageServer.sources.contains(uri)) {
        //Since the TextDocumentSyncKind is Full, we can assume that there is only one change that is a full content change.
        val src = didChangeTextDocumentParams.getContentChanges.get(0).getText
        flixLanguageServer.addUri(uri, src)
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
      ClientUri.shadow(uri)
      val range = Range.fromLsp4j(params.getRange)
      val codeActions =
        CodeActionProvider
        .getCodeActions(uri, range, flixLanguageServer.currentErrors)(flixLanguageServer.root, flixLanguageServer.flix)
        .map(_.toLsp4j)
        .map(messages.Either.forRight[Command, CodeAction])
        .asJava
      CompletableFuture.completedFuture(codeActions)
    }

    override def codeLens(params: CodeLensParams): CompletableFuture[util.List[? <: CodeLens]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val codeLens = CodeLensProvider.processCodeLens(uri)(flixLanguageServer.root).map(_.toLsp4j).asJava
      CompletableFuture.completedFuture(codeLens)
    }

    override def completion(params: CompletionParams): CompletableFuture[messages.Either[util.List[CompletionItem], lsp4j.CompletionList]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val pos = Position.fromLsp4j(params.getPosition)
      val completions = CompletionProvider
        .getCompletions(uri, pos, flixLanguageServer.currentErrors)(flixLanguageServer.root, flixLanguageServer.flix)
        .map(_.toCompletionItem(flixLanguageServer.flix))
      val completionList = CompletionList(isIncomplete = true, completions).toLsp4j
      CompletableFuture.completedFuture(messages.Either.forRight[util.List[CompletionItem], lsp4j.CompletionList](completionList))
    }

    override def definition(params: DefinitionParams): CompletableFuture[messages.Either[util.List[? <: Location], util.List[? <: LocationLink]]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val pos = Position.fromLsp4j(params.getPosition)
      val definition = GotoProvider.processGoto(uri, pos)(flixLanguageServer.root)
      CompletableFuture.completedFuture(messages.Either.forRight(definition.map(_.toLsp4j).toList.asJava))
    }

    /**
      * Returns the hover information for the given position in the given document.
      *
      * Now a mock implementation that just returns a simple greeting.
      */
    override def hover(params: HoverParams): CompletableFuture[Hover] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val position = Position.fromLsp4j(params.getPosition)
      val hover = HoverProvider.processHover(uri, position)(flixLanguageServer.root, flixLanguageServer.flix).map(_.toLsp4j).orNull
      CompletableFuture.completedFuture(hover)
    }

    override def documentHighlight(params: DocumentHighlightParams): CompletableFuture[java.util.List[? <: DocumentHighlight]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val position = Position.fromLsp4j(params.getPosition)
      val highlights = HighlightProvider.processHighlight(uri, position)(flixLanguageServer.root)
      CompletableFuture.completedFuture(highlights.map(_.toLsp4j).toList.asJava)
    }

    /**
      * Returns the semantic tokens (full) for the given document, which is used to provide semantic highlighting.
      */
    override def semanticTokensFull(params: SemanticTokensParams): CompletableFuture[SemanticTokens] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val tokens = SemanticTokensProvider.provideSemanticTokens(uri)(flixLanguageServer.root)
      val semanticTokens = new lsp4j.SemanticTokens()
      semanticTokens.setData(tokens.map(Int.box).asJava)
      CompletableFuture.completedFuture(semanticTokens)
    }

    override def references(params: ReferenceParams): CompletableFuture[util.List[? <: Location]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val pos = Position.fromLsp4j(params.getPosition)
      val references = FindReferencesProvider.findRefs(uri, pos)(flixLanguageServer.root)
      CompletableFuture.completedFuture(references.map(_.toLsp4j).toList.asJava)
    }

    override def rename(params: RenameParams): CompletableFuture[WorkspaceEdit] = {
      val newName = params.getNewName
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val pos = Position.fromLsp4j(params.getPosition)
      RenameProvider.processRename(newName, uri, pos)(flixLanguageServer.root) match {
        case Some(rename) => CompletableFuture.completedFuture(rename.toLsp4j)

        // If nothing is found it's OK to return the empty WorkspaceEdit.
        case None => CompletableFuture.completedFuture(new WorkspaceEdit())
      }
    }

    override def signatureHelp(params: SignatureHelpParams): CompletableFuture[SignatureHelp] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val pos = Position.fromLsp4j(params.getPosition)
      val signatureHelp = SignatureHelpProvider.provideSignatureHelp(uri, pos)(flixLanguageServer.root, flixLanguageServer.flix)
      CompletableFuture.completedFuture(signatureHelp.map(_.toLsp4j).orNull)
    }

    override def implementation(params: ImplementationParams): CompletableFuture[messages.Either[util.List[? <: Location], util.List[_ <: LocationLink]]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val pos = Position.fromLsp4j(params.getPosition)
      val implementation = GotoProvider.processGoto(uri, pos)(flixLanguageServer.root)
      CompletableFuture.completedFuture(messages.Either.forRight(implementation.map(_.toLsp4j).toList.asJava))
    }

    override def inlayHint(params: InlayHintParams): CompletableFuture[util.List[InlayHint]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val range = Range.fromLsp4j(params.getRange)
      val hints = InlayHintProvider.getInlayHints(uri, range, flixLanguageServer.currentErrors)(flixLanguageServer.root)
      CompletableFuture.completedFuture(hints.map(_.toLsp4j).asJava)
    }

    override def documentSymbol(params: DocumentSymbolParams): CompletableFuture[util.List[messages.Either[SymbolInformation, DocumentSymbol]]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val symbols = SymbolProvider.processDocumentSymbols(uri)(flixLanguageServer.root)
      CompletableFuture.completedFuture(symbols.map(_.toLsp4j).map(messages.Either.forRight[SymbolInformation, DocumentSymbol]).asJava)
    }

    override def foldingRange(params: FoldingRangeRequestParams): CompletableFuture[util.List[FoldingRange]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val foldingRanges = FoldingRangeProvider.getFoldingRanges(uri)(flixLanguageServer.root).map(_.toLsp4j).asJava
      CompletableFuture.completedFuture(foldingRanges)
    }

    /**
      * Formats the given document.
      *
      * @param params the document formatting parameters
      * @return a future containing the list of text edits
      */
    override def formatting(params: DocumentFormattingParams): CompletableFuture[util.List[? <: TextEdit]] = {
      val uri = params.getTextDocument.getUri
      ClientUri.shadow(uri)
      val options = FormattingOptions.fromLsp4j(params.getOptions)

      val editsJava: util.List[TextEdit] =
        FormattingProvider.formatDocument(uri, options)(flixLanguageServer.flix)
          .map(_.toLsp4j)
          .asJava

      java.util.concurrent.CompletableFuture.completedFuture(editsJava.asInstanceOf[util.List[? <: TextEdit]])
    }
  }

  private class FlixWorkspaceService(flixLanguageServer: FlixLanguageServer) extends WorkspaceService {
    override def didChangeConfiguration(didChangeConfigurationParams: DidChangeConfigurationParams): Unit = {
      System.err.println(s"didChangeConfiguration: $didChangeConfigurationParams")
    }

    /**
      * Called when a watched file changes. Only JARs and packages are watched (see `initialized`).
      */
    override def didChangeWatchedFiles(didChangeWatchedFilesParams: DidChangeWatchedFilesParams): Unit = {
      var dependencyChanged = false
      for (event <- didChangeWatchedFilesParams.getChanges.asScala) {
        val uri = event.getUri
        ClientUri.shadow(uri)
        if (uri.endsWith(".jar") || uri.endsWith(".fpkg")) {
          dependencyChanged = true
        }
      }
      if (dependencyChanged) {
        flixLanguageServer.onDependencyChange()
      }
    }

    override def symbol(params: WorkspaceSymbolParams): CompletableFuture[messages.Either[util.List[? <: SymbolInformation], util.List[? <: WorkspaceSymbol]]] = {
      val query = params.getQuery
      val symbols = SymbolProvider.processWorkspaceSymbols(query)(flixLanguageServer.root)
      CompletableFuture.completedFuture(messages.Either.forRight(symbols.map(_.toLsp4j).asJava))
    }
  }
}
