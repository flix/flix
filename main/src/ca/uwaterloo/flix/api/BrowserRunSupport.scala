/*
 * Copyright 2026 Magnus Madsen
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

package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.llvm.{LlvmWasmDriver, LlvmWasmExportWriter}
import ca.uwaterloo.flix.util.{InternalCompilerException, Options}

import java.io.IOException
import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, Paths, SimpleFileVisitor, StandardCopyOption}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object BrowserRunSupport {

  private val BundledBrowserRunHtmlResource = "/tools/wasm-runner-js/browser/run_component.html"
  private val BundledBrowserRunModuleResource = "/tools/wasm-runner-js/browser/run_component.mjs"
  private val BundledBrowserChromeRunnerResource = "/tools/wasm-runner-js/browser/run_headless_chrome.mjs"
  private val BundledRunnerModuleResource = "/tools/wasm-runner-js/runner.mjs"
  private val BundledOpfsHandlersResource = "/tools/wasm-runner-js/opfs-handlers.mjs"
  private val BundledServeScriptResource = "/tools/wasm-runner-js/browser/serve.mjs"

  private val DefaultBrowserRunHtml = Paths.get("tools/wasm-runner-js/browser/run_component.html").toAbsolutePath.normalize()
  private val DefaultBrowserRunModule = Paths.get("tools/wasm-runner-js/browser/run_component.mjs").toAbsolutePath.normalize()
  private val DefaultBrowserChromeRunner = Paths.get("tools/wasm-runner-js/browser/run_headless_chrome.mjs").toAbsolutePath.normalize()
  private val DefaultRunnerModule = Paths.get("tools/wasm-runner-js/runner.mjs").toAbsolutePath.normalize()
  private val DefaultOpfsHandlersModule = Paths.get("tools/wasm-runner-js/opfs-handlers.mjs").toAbsolutePath.normalize()
  private val DefaultServeScript = Paths.get("tools/wasm-runner-js/browser/serve.mjs").toAbsolutePath.normalize()

  private case class BrowserAssets(runHtml: Path, chromeRunner: Path, serveScript: Path)

  def runBrowser(options: Options, rootDir: Path, args: Array[String], headless: Boolean): Either[String, Unit] = {
    try {
      val root = rootDir.toAbsolutePath.normalize()
      val outputPath = options.outputPath.toAbsolutePath.normalize()
      if (!outputPath.startsWith(root)) {
        return Left(s"Browser runner requires the output path to be inside the current working directory.\nrootDir: $root\noutputPath: $outputPath")
      }

      val componentJs = LlvmWasmDriver.componentJsPath(options.outputPath, options.artifactName)
      val exportsManifest = LlvmWasmExportWriter.manifestPath(options.outputPath, options.artifactName)
      if (!Files.exists(componentJs)) return Left(s"Missing wasm JS component artifact: $componentJs")
      if (!Files.exists(exportsManifest)) return Left(s"Missing wasm exports manifest: $exportsManifest")

      val chrome = findChromeBinary().getOrElse {
        return Left("Chrome/Chromium not found. Set FLIX_CHROME=/path/to/chrome or install google-chrome/chromium.")
      }

      val assets = resolveBrowserAssets(outputPath)
      val seedManifest = writeSeedManifest(root, outputPath)
      val port = findAvailablePort()
      val server = startServer(assets.serveScript, root, port)
      val shutdownHook = new Thread(() => stopServer(server))
      Runtime.getRuntime.addShutdownHook(shutdownHook)

      try {
        waitForPort(port, timeoutMs = 5_000)
        val url = mkBrowserUrl(
          port = port,
          servedRoot = root,
          runHtml = assets.runHtml,
          componentJs = componentJs,
          exportsManifest = exportsManifest,
          seedManifest = seedManifest,
          args = args
        )

        runChromeCdp(assets.chromeRunner, chrome, url, headless) match {
          case (0, output) =>
            if (output.nonEmpty) {
              print(output)
            }
            Right(())
          case (exit, output) =>
            Left(s"Browser runner exited with code: $exit\n$output")
        }
      } finally {
        stopServer(server)
        try Runtime.getRuntime.removeShutdownHook(shutdownHook)
        catch {
          case _: IllegalStateException => ()
        }
      }
    } catch {
      case NonFatal(ex) => Left(Option(ex.getMessage).getOrElse(ex.toString))
    }
  }

  def findChromeBinary(): Option[String] = {
    val fromEnv = sys.env.get("FLIX_CHROME").map(_.trim).filter(_.nonEmpty)
    fromEnv match {
      case Some(bin) =>
        if (Files.isExecutable(Paths.get(bin))) return Some(bin)
      case None =>
    }

    val absCandidates = List(
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Chromium.app/Contents/MacOS/Chromium",
    )
    absCandidates.find(p => Files.isExecutable(Paths.get(p))) match {
      case some@Some(_) => return some
      case None =>
    }

    val cmdCandidates = List(
      "google-chrome",
      "google-chrome-stable",
      "chromium",
      "chromium-browser",
    )
    cmdCandidates.find(c => hasCmd(List(c, "--version")))
  }

  private def hasCmd(cmd: List[String]): Boolean = {
    try {
      val p = new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
      p.waitFor(2, TimeUnit.SECONDS) && p.exitValue() == 0
    } catch {
      case _: IOException => false
      case _: InterruptedException => false
    }
  }

  private def resolveBrowserAssets(outputPath: Path): BrowserAssets = {
    val outDir = outputPath.resolve("llvm").toAbsolutePath.normalize()
    val browserDir = outDir.resolve("tools").resolve("wasm-runner-js").resolve("browser")
    val runnerDir = browserDir.getParent
    Files.createDirectories(browserDir)
    Files.createDirectories(runnerDir)

    val runHtml = copyLocalOrBundled(DefaultBrowserRunHtml, BundledBrowserRunHtmlResource, browserDir.resolve("run_component.html"))
    val runModule = copyLocalOrBundled(DefaultBrowserRunModule, BundledBrowserRunModuleResource, browserDir.resolve("run_component.mjs"))
    val chromeRunner = copyLocalOrBundled(DefaultBrowserChromeRunner, BundledBrowserChromeRunnerResource, browserDir.resolve("run_headless_chrome.mjs"))
    copyLocalOrBundled(DefaultRunnerModule, BundledRunnerModuleResource, runnerDir.resolve("runner.mjs"))
    copyLocalOrBundled(DefaultOpfsHandlersModule, BundledOpfsHandlersResource, runnerDir.resolve("opfs-handlers.mjs"))
    val serveScript = copyLocalOrBundled(DefaultServeScript, BundledServeScriptResource, browserDir.resolve("serve.mjs"))

    BrowserAssets(
      runHtml = runHtml,
      chromeRunner = chromeRunner,
      serveScript = serveScript,
    )
  }

  private def copyBundledResource(resource: String, dest: Path): Path = {
    val is = Option(getClass.getResourceAsStream(resource)).getOrElse {
      throw InternalCompilerException(s"Missing bundled browser runner resource: '$resource'.", SourceLocation.Unknown)
    }
    try {
      Files.createDirectories(dest.getParent)
      Files.copy(is, dest, StandardCopyOption.REPLACE_EXISTING)
    } finally {
      is.close()
    }
    dest
  }

  private def copyLocalOrBundled(local: Path, resource: String, dest: Path): Path = {
    if (Files.exists(local)) {
      Files.createDirectories(dest.getParent)
      Files.copy(local, dest, StandardCopyOption.REPLACE_EXISTING)
      dest
    } else {
      copyBundledResource(resource, dest)
    }
  }

  private def writeSeedManifest(rootDir: Path, outputPath: Path): Path = {
    val manifestPath = outputPath.resolve("llvm").resolve("browser").resolve("flix-browser-seed.json").toAbsolutePath.normalize()
    Files.createDirectories(manifestPath.getParent)

    val excludedPrefixes = List(
      rootDir.resolve("build").normalize(),
      rootDir.resolve(".git").normalize(),
      rootDir.resolve(".gradle").normalize(),
      rootDir.resolve(".idea").normalize(),
      rootDir.resolve(".bsp").normalize(),
      rootDir.resolve(".scala-build").normalize(),
      rootDir.resolve("lib").normalize(),
    )

    val files = scala.collection.mutable.ListBuffer.empty[(String, String)]
    Files.walkFileTree(rootDir, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        val norm = dir.toAbsolutePath.normalize()
        if (excludedPrefixes.exists(prefix => norm == prefix || norm.startsWith(prefix))) {
          java.nio.file.FileVisitResult.SKIP_SUBTREE
        } else {
          java.nio.file.FileVisitResult.CONTINUE
        }
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        if (attrs.isRegularFile) {
          val rel = rootDir.relativize(file.toAbsolutePath.normalize())
          val relString = rel.iterator().asScala.mkString("/").replace('\\', '/')
          if (relString.nonEmpty) {
            files += relString -> ("/" + relString)
          }
        }
        java.nio.file.FileVisitResult.CONTINUE
      }
    })

    val json =
      s"""{
         |  "schema": "flix-browser-seed-v0",
         |  "files": [
         |${files.map { case (path, url) =>
            s"""    {"path": "${escapeJson(path)}", "url": "${escapeJson(url)}"}"""
          }.mkString(",\n")}
         |  ]
         |}
         |""".stripMargin

    Files.writeString(manifestPath, json, StandardCharsets.UTF_8)
    manifestPath
  }

  private def escapeJson(s: String): String =
    s.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c => c.toString
    }

  private def mkBrowserUrl(port: Int,
                           servedRoot: Path,
                           runHtml: Path,
                           componentJs: Path,
                           exportsManifest: Path,
                           seedManifest: Path,
                           args: Array[String]): String = {
    val base =
      s"http://127.0.0.1:$port${toUrlPath(runHtml, servedRoot)}?component=${encodeQueryValue(toUrlPath(componentJs, servedRoot))}&exports=${encodeQueryValue(toUrlPath(exportsManifest, servedRoot))}&seed=${encodeQueryValue(toUrlPath(seedManifest, servedRoot))}&budget=500"
    args.foldLeft(base) { case (acc, arg) => s"$acc&argv=${encodeQueryValue(arg)}" }
  }

  private def encodeQueryValue(s: String): String =
    java.net.URLEncoder.encode(s, StandardCharsets.UTF_8).replace("+", "%20")

  private def toUrlPath(p: Path, servedRoot: Path): String = {
    val abs = p.toAbsolutePath.normalize()
    val root = servedRoot.toAbsolutePath.normalize()
    if (!abs.startsWith(root)) {
      throw InternalCompilerException(s"Browser runner path '$abs' is not under served root '$root'.", SourceLocation.Unknown)
    }
    "/" + root.relativize(abs).iterator().asScala.mkString("/").replace('\\', '/')
  }

  private def startServer(serveScript: Path, rootDir: Path, port: Int): Process = {
    val pb = new ProcessBuilder(List("node", serveScript.toString).asJava)
    pb.redirectErrorStream(true)
    pb.environment().put("PORT", port.toString)
    pb.environment().put("ROOT_DIR", rootDir.toString)
    pb.start()
  }

  private def stopServer(p: Process): Unit = {
    try {
      p.destroy()
      p.waitFor(2, TimeUnit.SECONDS)
    } catch {
      case _: InterruptedException => ()
    } finally {
      p.destroyForcibly()
    }
  }

  private def findAvailablePort(): Int = {
    val ss = new ServerSocket()
    try {
      ss.bind(new InetSocketAddress("127.0.0.1", 0))
      ss.getLocalPort
    } finally {
      ss.close()
    }
  }

  private def waitForPort(port: Int, timeoutMs: Long): Unit = {
    val deadline = System.currentTimeMillis() + timeoutMs
    while (System.currentTimeMillis() < deadline) {
      try {
        val s = new Socket()
        s.connect(new InetSocketAddress("127.0.0.1", port), 200)
        s.close()
        return
      } catch {
        case _: IOException => Thread.sleep(50)
      }
    }
    throw InternalCompilerException(s"Timed out waiting for browser harness server on 127.0.0.1:$port.", SourceLocation.Unknown)
  }

  private def runChromeCdp(chromeRunner: Path, chrome: String, url: String, headless: Boolean): (Int, String) = {
    val cmd = List(
      "node",
      chromeRunner.toString,
      "--chrome",
      chrome,
      "--url",
      url,
      "--headless",
      headless.toString,
      "--timeoutMs",
      "60000",
    )
    val pb = new ProcessBuilder(cmd.asJava)
    pb.redirectErrorStream(true)
    val p = pb.start()
    val out = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val ok = p.waitFor(70, TimeUnit.SECONDS)
    if (!ok) {
      p.destroyForcibly()
      return (1, s"Timed out running browser runner for URL: $url")
    }
    (p.exitValue(), out)
  }
}
