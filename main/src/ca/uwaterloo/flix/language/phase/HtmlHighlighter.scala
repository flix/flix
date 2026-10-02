/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.lsp.provider.SemanticTokensProvider
import ca.uwaterloo.flix.api.lsp.{SemanticToken, SemanticTokenType}
import ca.uwaterloo.flix.language.ast.shared.{Origin, Source}
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.util.LocalResource

import java.io.IOException
import java.nio.file.{Files, Path}

/**
  * Generates a page for each source that shows its code, highlighted by its semantic tokens.
  *
  * The pages are part of the API documentation, but all that the documentation has to know about
  * them is [[run]], which generates them, and [[link]], which links to them.
  */
object HtmlHighlighter {

  /**
    * The path to the stylesheet of the pages, relative to the resources folder.
    */
  private val Stylesheet: String = "/doc/highlight.css"

  /**
    * The path to the script of the pages, relative to the resources folder.
    */
  private val Script: String = "/doc/highlight.js"

  /**
    * The extension of a Flix source file, which the file name of its page leaves out.
    */
  private val SourceExtension: String = ".flix"

  /**
    * Writes a page for every source in `root` that comes from `origin` to `outputDir`, together
    * with the stylesheet and the script of the pages.
    *
    * A page is given its frame by `mkPage`, which returns the document of the page with the given
    * name, file name, and content.
    */
  def run(root: TypedAst.Root, origin: Origin, outputDir: Path)(mkPage: (String, String, String) => String): Unit = {
    for {
      src <- root.sources.keys
      if src.origin == origin
      path <- pathOf(src)
    } {
      val fileName = fileNameOf(path)
      val page = mkPage(segmentsOf(path).mkString("/"), fileName, mkContent(src, path)(root))
      writeFile(fileName, page, outputDir)
    }

    writeFile("highlight.css", LocalResource.get(Stylesheet), outputDir)
    writeFile("highlight.js", LocalResource.get(Script), outputDir)
  }

  /**
    * Returns the link to the lines of `loc` on the page of its source, relative to the directory
    * of the pages, if the source has a path.
    *
    * The fragment names the first and the last line, e.g. `#L10-L20`, which the script marks and
    * scrolls to. A single line is named by its id alone, which the browser can jump to by itself.
    *
    * The link depends on nothing but `loc`, so it is to a page that exists only if [[run]] is
    * given the origin of its source.
    */
  def link(loc: SourceLocation): Option[String] = pathOf(loc.source).map { path =>
    val lines = if (loc.startLine == loc.endLine) s"L${loc.startLine}" else s"L${loc.startLine}-L${loc.endLine}"
    s"${fileNameOf(path)}#$lines"
  }

  /**
    * Returns the path of `src` as it is displayed, if it has one.
    *
    * A source of the library is named by its path within the library. Any other source is named
    * relative to the working directory, which is the root of the project when its documentation
    * is generated, or by its file name alone if it lies outside it.
    */
  private def pathOf(src: Source): Option[Path] = {
    val path = src.sourceName.toPath.map { path =>
      src.origin match {
        case Origin.Library => path
        case _ =>
          val cwd = Path.of("").toAbsolutePath.normalize()
          val absolute = path.toAbsolutePath.normalize()
          if (absolute.startsWith(cwd)) cwd.relativize(absolute) else absolute.getFileName
      }
    }
    // A path without a name, e.g. the root of the file system, does not name a file.
    path.filter(p => p != null && p.getNameCount > 0)
  }

  /**
    * Returns the segments of `path`, e.g. `Fs` and `FileSystem.flix` for `Fs/FileSystem.flix`.
    */
  private def segmentsOf(path: Path): List[String] =
    List.tabulate(path.getNameCount)(i => path.getName(i).toString)

  /**
    * Returns the file name of the page of the source at `path`, e.g. `Fs.FileSystem.src.html`
    * for `Fs/FileSystem.flix`.
    *
    * The directories are joined by `.`, like in the file name of the page of a module. The name
    * cannot clash with the page of a declaration, since no declaration is named `src`.
    *
    * Any character of a segment that is not a letter, a digit, `_`, or `-` is replaced by `_`, so
    * the name can be linked as is and `A/B.flix` does not share its page with `A.B.flix`.
    */
  private def fileNameOf(path: Path): String = {
    val segments = segmentsOf(path)
    val names = segments.init :+ segments.last.stripSuffix(SourceExtension)
    names.map(_.replaceAll("[^A-Za-z0-9_-]", "_")).mkString(".") + ".src.html"
  }

  /**
    * Returns the content of the page of `src`, whose path is `path`.
    *
    * The content brings its own stylesheet and script, so the frame of the page does not have to
    * know about them. They follow those of the frame, which the stylesheet builds on.
    */
  private def mkContent(src: Source, path: Path)(implicit root: TypedAst.Root): String = {
    implicit val sb: StringBuilder = new StringBuilder()
    val segments = segmentsOf(path)

    sb.append("<link href='highlight.css' rel='stylesheet'>")
    // The bold weight of the font of code, which the keywords are displayed in.
    sb.append("<link href='https://fonts.googleapis.com/css?family=Fira+Code:700&display=swap' rel='stylesheet'>")

    if (segments.length > 1) {
      sb.append("<div class='breadcrumbs'>")
      for (dir <- segments.init) {
        esc(dir, 0, dir.length)
        sb.append(" / ")
      }
      sb.append("<span>")
      esc(segments.last, 0, segments.last.length)
      sb.append("</span>")
      sb.append("</div>")
    }

    sb.append("<h1>")
    esc(segments.last, 0, segments.last.length)
    sb.append("</h1>")

    highlight(src)

    sb.append("<script type='module' src='./highlight.js'></script>")

    sb.toString()
  }

  /**
    * Appends the text of `src` to `sb` as a `<pre>` element.
    *
    * Every line is a `<span>` with the id `L<n>`, where `n` is its one-indexed line number,
    * followed by a newline. A token within a line is a `<span>` with the class of its type, see
    * [[cssClass]].
    *
    * The semantic tokens only cover part of the text, and some of them have no class. The text in
    * between is emitted as is, so the element always holds the entire text of `src`.
    */
  private def highlight(src: Source)(implicit root: TypedAst.Root, sb: StringBuilder): Unit = {
    // The semantic tokens are split by line, i.e. no token spans more than one line.
    val tokens = SemanticTokensProvider.getSemanticTokens(src.sourceName).groupBy(_.loc.startLine)

    sb.append("<pre class='source-code'><code>")
    for ((line, i) <- lines(src).zipWithIndex) {
      val lineNo = i + 1
      sb.append(s"<span id='L$lineNo'>")
      highlightLine(line, tokens.getOrElse(lineNo, Nil))
      sb.append("</span>\n")
    }
    sb.append("</code></pre>")
  }

  /**
    * Returns the lines of `src`, each without its line terminator.
    *
    * A newline at the very end of the text terminates the last line, i.e. it does not begin an
    * empty one.
    */
  private def lines(src: Source): Array[String] = {
    val text = new String(src.data)
    if (text.isEmpty) {
      return Array.empty
    }
    text.stripSuffix("\n").split("\n", -1).map(_.stripSuffix("\r"))
  }

  /**
    * Appends `line` to `sb`, with each of `tokens`, which must all be on the line, in a `<span>`.
    *
    * A token that overlaps a token before it is dropped, which leaves its text in place: every
    * character of `line` is emitted exactly once, whether or not it is part of a token.
    */
  private def highlightLine(line: String, tokens: List[SemanticToken])(implicit sb: StringBuilder): Unit = {
    // The index of the first character of `line` that has not been emitted yet.
    var i = 0
    for (token <- tokens.sortBy(t => (t.loc.startCol, t.loc.endCol))) {
      // The columns are one-indexed and the end is exclusive. A token cannot be trusted to lie
      // within the line, so it is clamped to it.
      val start = Math.min(token.loc.startCol - 1, line.length)
      val end = Math.min(token.loc.endCol - 1, line.length)
      if (i <= start && start < end) {
        esc(line, i, start)
        cssClass(token.tpe) match {
          case Some(cls) =>
            sb.append(s"<span class='$cls'>")
            esc(line, start, end)
            sb.append("</span>")
          case None =>
            esc(line, start, end)
        }
        i = end
      }
    }
    esc(line, i, line.length)
  }

  /**
    * Returns the CSS class of a token of the type `tpe`, if it has one.
    *
    * The classes are defined in `highlight.css`. They are specific to the highlighter, i.e. no
    * other part of the documentation uses them, and they are short, since a page has one per token.
    *
    * A type without a class is displayed as plain text.
    */
  private def cssClass(tpe: SemanticTokenType): Option[String] = tpe match {
    case SemanticTokenType.Class => Some("ty")
    case SemanticTokenType.Comment => Some("cm")
    case SemanticTokenType.Decorator => Some("an")
    case SemanticTokenType.Effect => Some("ef")
    case SemanticTokenType.Enum => Some("ty")
    case SemanticTokenType.Function => Some("fn")
    case SemanticTokenType.Interface => Some("ty")
    case SemanticTokenType.Keyword => Some("kw")
    case SemanticTokenType.Method => Some("fn")
    case SemanticTokenType.Modifier => Some("kw")
    case SemanticTokenType.Number => Some("nu")
    case SemanticTokenType.Parameter => Some("va")
    case SemanticTokenType.Regexp => Some("st")
    case SemanticTokenType.String => Some("st")
    case SemanticTokenType.Struct => Some("ty")
    case SemanticTokenType.Type => Some("ty")
    case SemanticTokenType.TypeParameter => Some("ty")
    case SemanticTokenType.Variable => Some("va")
    case _ => None
  }

  /**
    * Appends the characters of `s` from `start` until `end` to `sb`, escaped as the content of an
    * element.
    *
    * Every other character is appended as is: unlike `xml.Utility.escape`, no character is dropped.
    */
  private def esc(s: String, start: Int, end: Int)(implicit sb: StringBuilder): Unit = {
    var i = start
    while (i < end) {
      s.charAt(i) match {
        case '&' => sb.append("&amp;")
        case '<' => sb.append("&lt;")
        case '>' => sb.append("&gt;")
        case c => sb.append(c)
      }
      i += 1
    }
  }

  /**
    * Writes `content` to the file called `name` in `outputDir`.
    */
  private def writeFile(name: String, content: String, outputDir: Path): Unit = {
    val path = outputDir.resolve(name)
    try {
      Files.createDirectories(outputDir)
      Files.writeString(path, content)
    } catch {
      case ex: IOException => throw new RuntimeException(s"Unable to write to path '$path'.", ex)
    }
  }

}
