/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation
import org.json4s.JValue
import org.json4s.native.JsonMethods

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, LinkOption, Path, SimpleFileVisitor, StandardCopyOption, StandardOpenOption}
import java.util.{Calendar, GregorianCalendar}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.collection.mutable
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Using

object FileOps {

  /**
    * Checks if `path` exists on the file system.
    *
    * @param path the path to check for existence.
    * @return `Ok(true)` if the path exists on the file system, `Ok(true)` otherwise.
    *         Returns `Err(e)` if an exception `e` occurred.
    */
  def exists(path: Path): Result[Boolean, Exception] = {
    try {
      Result.Ok(Files.exists(path))
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Returns the contents of the file at `path` as a string.
    *
    * @param path the path to read. The caller must check if it exists first.
    * @return the contents of the file.
    */
  def readString(path: Path): Result[String, Exception] = {
    try {
      Result.Ok(Files.readString(path))
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Deletes `path` if it exists. Wraps any error `e` in `Result.Err(e)`.
    *
    * @param path the path to delete
    * @return `Ok(())` if `path` was successfully deleted, `Err(e)` otherwise
    */
  def delete(path: Path): Result[Unit, Exception] = {
    try {
      if (Files.deleteIfExists(path)) {
        Result.Ok(())
      } else {
        Result.Err(new RuntimeException(s"path '$path' does not exist"))
      }
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Moves the path `source` to `target`. Wraps any error `e` in `Result.Err(e)`.
    *
    * @param source the source path to move from.
    * @param target the target path to move to.
    * @return `Ok(())` if `path` was successfully moved, `Err(e)` otherwise.
    */
  def move(source: Path, target: Path): Result[Unit, Exception] = {
    try {
      Files.move(source, target, StandardCopyOption.ATOMIC_MOVE)
      Result.Ok(())
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Moves the entire directory `sourceDir` to `targetDir` (including all its contents).
    *
    * Fails if `sourceDir` does not exist. Creates `targetDir` if it does not exist.
    *
    * @param sourceDir the source path directory to move from.
    * @param targetDir the target path directory to move to.
    * @return `Ok(())` if `sourceDir` was successfully moved, `Err(e)` otherwise.
    */
  def moveDir(sourceDir: Path, targetDir: Path): Result[Unit, Exception] = {
    exists(sourceDir) match {
      case Result.Err(e) => return Result.Err(e)
      case Result.Ok(false) => return Result.Err(new RuntimeException(s"path '$sourceDir' does not exist"))
      case Result.Ok(true) => ()
    }
    exists(targetDir) match {
      case Result.Err(e) => return Result.Err(e)
      case Result.Ok(_) if Files.isRegularFile(targetDir) => return Result.Err(new RuntimeException(s"path '$targetDir' is not a valid target directory. It is a file.'"))
      case Result.Ok(false) => FileOps.newDirectoryIfAbsent(targetDir)
      case Result.Ok(true) => ()
    }
    // Attempt to move directory as-is by renaming.
    FileOps.move(sourceDir, targetDir) match {
      case Result.Ok(()) =>
        // Successful move, nothing more to do.
        Result.Ok(())

      case Result.Err(_) =>
        // Error happened, fall back to recursively copying and then deleting.
        // 1. Recursively copy.
        FileOps.walkTreePreOrder(sourceDir).flatMap {
          paths =>
            Result.traverse(paths) {
              sourcePath =>
                val relativeResourcePath = sourceDir.relativize(sourcePath)
                val targetPath = targetDir.resolve(relativeResourcePath).normalize()
                if (Files.isDirectory(sourceDir)) {
                  FileOps.createDir(targetPath)
                } else {
                  FileOps.copy(sourcePath, targetPath)
                }
            }.map(_ => ())
        } match {
          // 2. Recursively delete if no error occurred.
          case Result.Err(e) => Result.Err(e)
          case Result.Ok(()) => FileOps.deleteDir(sourceDir)
        }
    }
  }

  private def copy(source: Path, destination: Path): Result[Unit, Exception] = {
    try {
      Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING)
      Result.Ok(())
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Deletes all contents of the directory `dir` and the directory itself.
    *
    * Fails if `dir` does not exist or is not a directory.
    *
    * @param dir the directory to be removed.
    * @return `Ok(())` if `dir` was successfully deleted, `Err(e)` otherwise.
    */
  def deleteDir(dir: Path): Result[Unit, Exception] = {
    try {
      if (!Files.exists(dir)) {
        return Result.Err(new RuntimeException(s"path '$dir' does not exist"))
      }
      if (!Files.isDirectory(dir)) {
        return Result.Err(new RuntimeException(s"path '$dir' is not a directory"))
      }
      walkTreePostOrder(dir).flatMap { paths =>
        Result.sequence(paths.map(delete)).map(_ => ())
      }
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Reads the first line of the file at the given path `p` if it is possible.
    */
  def readLine(p: Path): Option[String] = {
    try {
      Some(Files.lines(p).findFirst().get())
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Writes the given string `s` to the given file path `p`.
    *
    * Creates the parent directory of `p` if needed.
    *
    * @param append if set to true, the content will be appended to the file
    */
  def writeString(p: Path, s: String, append: Boolean = false): Unit = {
    Files.createDirectories(p.getParent)

    // Check if the file already exists.
    if (Files.exists(p)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(p, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$p'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(p)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$p'.", SourceLocation.Unknown)
      }
    }

    if (append) {
      Files.write(p, s.getBytes, StandardOpenOption.APPEND)
    } else {
      Files.write(p, s.getBytes)
    }
  }

  /**
    * Writes the given json `j` to the given file path `p`.
    *
    * Creates the parent directory of `p` if needed.
    */
  def writeJSON(p: Path, j: JValue): Unit = {
    FileOps.writeString(p, JsonMethods.pretty(JsonMethods.render(j)))
  }

  /**
    * Creates a new directory at the given path `p`.
    *
    * The path must not already contain a non-directory.
    */
  def createDir(path: Path): Result[Unit, Exception] = {
    try {
      if (Files.exists(path)) {
        return Result.Err(new RuntimeException(s"Path '$path' already exists.'"))
      }
      if (!Files.isDirectory(path)) {
        return Result.Err(new RuntimeException(s"Path '$path' is not a directory.'"))
      }
      Files.createDirectories(path)
      Result.Ok(())
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Creates a new directory at the given path `p`.
    *
    * The path must not already contain a non-directory.
    */
  def newDirectoryIfAbsent(p: Path): Unit = {
    if (!Files.exists(p)) {
      Files.createDirectories(p)
    }
  }

  /**
    * Creates a new file at the given path `p` with the given content `s` if the file does not already exist.
    */
  def newFileIfAbsent(p: Path)(s: String): Unit = {
    if (!Files.exists(p)) {
      Files.writeString(p, s, StandardOpenOption.CREATE)
    }
  }

  /**
    * Returns all files ending with `.flix` in `path`.
    *
    * The search is limited at `depth - 1` levels of subdirectories.
    *
    * E.g., if `depth = 1` then given the directory structure below,
    * `Subfile.flix` will not be included.
    *
    * {{{
    * path
    * ├── Main.flix
    * └── subdir
    *     └── Subfile.flix
    * }}}
    */
  def getFlixFilesIn(path: Path, depth: Int): List[Path] = {
    walkTreeSorted(path, depth).filter(checkExt(_, "flix"))
  }

  /**
    * Returns a sorted list of all files (excluding directories) in the given path, visited recursively.
    * The depth parameter is the maximum number of levels of directories to visit.
    * Use a depth of 0 to only visit the given directory.
    * Use a depth of 1 to only visit the files in the given directory.
    * Use a depth of [[Int.MaxValue]] to visit all files in the directory and its subdirectories.
    */
  def getFilesIn(path: Path, depth: Int): List[Path] = {
    walkTreeSorted(path, depth).filter(Files.isRegularFile(_))
  }

  /**
    * Returns a list of all directories in the given path, visited recursively.
    * The list contains the directories in order of outermost to innermost, e.g., `a :: a/b :: a/b/c :: Nil`.
    *
    * The depth parameter is the maximum number of levels of directories to visit.
    * Use a depth of 0 to only visit the given directory.
    * Use a depth of 1 to only visit the files in the given directory.
    * Use a depth of [[Int.MaxValue]] to visit all files in the directory and its subdirectories.
    */
  def getDirectoriesIn(path: Path, depth: Int): List[Path] = {
    walkTreeSorted(path, depth).filter(Files.isDirectory(_))
  }

  /**
    * Returns a sorted list of all paths in the given path (including `path`), visited recursively.
    * The list is sorted by the path name, i.e., the shortest path name first.
    * Reverse the list if you need to
    *
    * The depth parameter is the maximum number of levels of directories to visit.
    * Use a depth of 0 to only visit the given directory.
    * Use a depth of 1 to only visit the files in the given directory.
    * Use a depth of [[Int.MaxValue]] to visit all files in the directory and its subdirectories.
    */
  private def walkTreeSorted(path: Path, depth: Int): List[Path] = {
    walkTree(path, depth).sorted
  }

  /**
    * Returns a list of all paths in the given path (including `path`), visited recursively, depth-first.
    * The depth parameter is the maximum number of levels of directories to visit.
    * Use a depth of 0 to only visit the given directory.
    * Use a depth of 1 to only visit the files in the given directory.
    * Use a depth of [[Int.MaxValue]] to visit all files in the directory and its subdirectories.
    */
  private def walkTree(path: Path, depth: Int): List[Path] = {
    if (Files.exists(path) && Files.isDirectory(path))
      Files.walk(path, depth)
        .iterator().asScala
        .toList
    else
      List.empty
  }

  private def walkTreePreOrder(path: Path): Result[List[Path], Exception] = {
    try {
      val result = mutable.ArrayBuffer.empty[Path]
      Files.walkFileTree(path, new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
          result.addOne(dir)
          FileVisitResult.CONTINUE
        }

        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          result.addOne(file)
          FileVisitResult.CONTINUE
        }
      })
      Result.Ok(result.toList)
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  private def walkTreePostOrder(path: Path): Result[List[Path], Exception] = {
    try {
      val result = mutable.ArrayBuffer.empty[Path]
      Files.walkFileTree(path, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          result.addOne(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          if (exc != null) {
            throw exc
          }
          result.addOne(dir)
          FileVisitResult.CONTINUE
        }
      })
      Result.Ok(result.toList)
    } catch {
      case e: Exception => Result.Err(e)
    }
  }

  /**
    * Returns a list of all files in the given path with the extension `.ext`, visited recursively.
    *
    * @param path  the path to start the file walk from.
    * @param ext   the file extension to match. Must not begin with `.`
    * @param depth the maximum number of levels of directories to visit.
    *              Use a depth of 0 to only visit the given directory.
    *              Use a depth of 1 to only visit the files in the given directory.
    *              Use a depth of [[Int.MaxValue]] to visit all files in the directory and its subdirectories.
    */
  def getFilesWithExtIn(path: Path, ext: String, depth: Int): List[Path] = {
    if (Files.exists(path) && Files.isDirectory(path))
      Files.walk(path, depth)
        .iterator().asScala
        .filter(checkExt(_, ext))
        .toList.sorted
    else
      List.empty
  }

  /** Returns `true` if the given `path` exists and is a Java Virtual Machine class file. */
  def isClassFile(path: Path): Boolean = {
    if (Files.exists(path) && Files.isReadable(path) && Files.isRegularFile(path)) {
      // Read the first four bytes of the file.
      val is = Files.newInputStream(path)
      val b1 = is.read()
      val b2 = is.read()
      val b3 = is.read()
      val b4 = is.read()
      is.close()

      // Check if the four first bytes match CAFE BABE.
      return b1 == 0xCA && b2 == 0xFE && b3 == 0xBA && b4 == 0xBE
    }
    false
  }

  /** Returns `true` if the given `path` exists and has 0 bytes of data. */
  def isEmpty(path: Path): Boolean = Files.size(path) == 0L

  /**
    * Checks if the given path is a regular file with the expected extension.
    *
    * @param p           the path to the file to check.
    * @param expectedExt the file extension to match. Must not begin with `.`
    */
  def checkExt(p: Path, expectedExt: String): Boolean = {
    require(!expectedExt.startsWith("."), s"The file extension '$expectedExt' must not start with '.'")
    Files.isRegularFile(p) && p.getFileName.toString.endsWith(s".$expectedExt")
  }

  /**
    * To support DOS time, Java 8+ treats dates before the 1980 January in special way.
    * Here we use 2014-06-27 (the date of the first commit to Flix) to avoid the complexity introduced by this hack.
    *
    * @see <a href="https://bugs.openjdk.java.net/browse/JDK-4759491">JDK-4759491 that introduced the hack around 1980 January from Java 8+</a>
    * @see <a href="https://bugs.openjdk.java.net/browse/JDK-6303183">JDK-6303183 that explains why the second should be even to create ZIP files in platform-independent way</a>
    * @see <a href="https://github.com/gradle/gradle/blob/445deb9aa988e506120b7918bf91acb421e429ba/subprojects/core/src/main/java/org/gradle/api/internal/file/archive/ZipCopyAction.java#L42-L57">A similar case from Gradle</a>
    */
  private val ENOUGH_OLD_CONSTANT_TIME: Long = new GregorianCalendar(2014, Calendar.JUNE, 27, 0, 0, 0).getTimeInMillis

  /**
    * Adds an entry to the given zip file.
    */
  def addToZip(zip: ZipOutputStream, name: String, p: Path): Unit = {
    if (Files.exists(p) && Files.isReadable(p)) {
      addToZip(zip, name, Files.readAllBytes(p))
    }
  }

  /**
    * Adds an entry to the given zip file.
    */
  def addToZip(zip: ZipOutputStream, name: String, d: Array[Byte]): Unit = {
    val entry = new ZipEntry(name)
    entry.setTime(ENOUGH_OLD_CONSTANT_TIME)
    zip.putNextEntry(entry)
    zip.write(d)
    zip.closeEntry()
  }

  /**
    * Returns `true` if the given path `p` is a zip-archive.
    */
  def isZipArchive(p: Path): Boolean = {
    if (Files.exists(p) && Files.isReadable(p) && Files.isRegularFile(p)) {
      // Read the first four bytes of the file.
      return Using(Files.newInputStream(p)) { is =>
        val b1 = is.read()
        val b2 = is.read()
        val b3 = is.read()
        val b4 = is.read()
        // Check if the four first bytes match 0x50, 0x4b, 0x03, 0x04
        return b1 == 0x50 && b2 == 0x4b && b3 == 0x03 && b4 == 0x04
      }.get
    }
    false
  }

  /**
    * Sorts `paths` using `prefix`.
    *
    * The `prefix` parameter must be a prefix of all paths in `paths`.
    *
    * Given a `p` in `paths` defined as `prefix/p1`, it converts `p1` to a string
    * representation and replaces `\` (backslash) with `/` (forward slash) and sorts
    * the list of paths on their new string representations.
    *
    * Returns the list of paths along with their string representations.
    */
  def sortPlatformIndependently(prefix: Path, paths: List[Path]): List[(Path, String)] = {
    require(paths.forall(_.startsWith(prefix)), "All paths in 'paths' must start with 'prefix'.")
    paths.map { path =>
      (path, convertPathToRelativeFileName(prefix, path))
    }.sortBy(_._2)
  }

  /**
    * @param prefix the root directory to compute a relative path from the given path
    * @param path   the path to be converted to a relative path based on the given root directory
    * @return relative file name separated by slashes, like `path/to/file.ext`
    */
  private def convertPathToRelativeFileName(prefix: Path, path: Path): String = {
    prefix.relativize(path).toString.replace('\\', '/')
  }

}
