package ca.uwaterloo.flix.tools

import java.io.PrintWriter
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import java.util.zip.{ZipEntry, ZipFile, ZipInputStream, ZipOutputStream}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util._

import scala.collection.mutable

/**
  * An interface to manage flix packages.
  */
object Packager {

  // TODO: Every operation should return a value that determines whether it was successful.

  /**
    * Initializes a new flix project at the given path `p`.
    *
    * The project must not already exist.
    */
  def init(p: Path, o: Options)(implicit tc: TerminalContext): Unit = {
    //
    // Check that the current working directory is usable.
    //
    if (!Files.isDirectory(p) || !Files.isReadable(p) || !Files.isWritable(p)) {
      throw new RuntimeException(s"The directory: '$p' is not accessible. Aborting.")
    }

    //
    // Compute the name of the package based on the directory name.
    //
    val packageName = getPackageName(p)

    //
    // Compute all the directories and files we intend to create.
    //
    val buildDirectory = getBuildDirectory(p)
    val sourceDirectory = getSourceDirectory(p)
    val testDirectory = getTestDirectory(p)

    val packageFile = getPackageFile(p)
    val historyFile = getHistoryFile(p)
    val licenseFile = getLicenseFile(p)
    val readmeFile = getReadmeFile(p)
    val mainSourceFile = getMainSourceFile(p)
    val mainTestFile = getMainTestFile(p)

    //
    // Check that the project directories and files do not already exist.
    //
    val allPaths = List(
      sourceDirectory, testDirectory, packageFile, historyFile, licenseFile, readmeFile, mainSourceFile, mainTestFile
    )
    val pathExists = allPaths.find(f => Files.exists(f))
    if (pathExists.nonEmpty) {
      throw new RuntimeException(s"The path: '${pathExists.get}' already exists. Aborting.")
    }

    //
    // Create the project directories and files.
    //
    newDirectory(buildDirectory)
    newDirectory(sourceDirectory)
    newDirectory(testDirectory)

    newFile(packageFile) {
      s"""(package
         |  (name     "$packageName")
         |  (version  "0.1.0")
         |)
         |""".stripMargin
    }

    newFile(historyFile) {
      """### v0.1.0
        |   Initial release.
        |""".stripMargin
    }

    newFile(licenseFile) {
      """Enter license information here.
        |""".stripMargin
    }

    newFile(readmeFile) {
      s"""# $packageName
         |
         |Enter some useful information.
         |
         |""".stripMargin
    }

    newFile(mainSourceFile) {
      """// The main entry point.
        |def main(_args: Array[String]): Int32 & Impure =
        |  Console.printLine("Hello World!");
        |  0 // exit code
        |""".stripMargin
    }

    newFile(mainTestFile) {
      """@test
        |def test01(): Bool = 1 + 1 == 2
        |""".stripMargin
    }
  }

  /**
    * Type checks the source files for the given project path `p`.
    */
  def check(p: Path, o: Options)(implicit tc: TerminalContext): Unit = {
    // Check that the path is a project path.
    if (!isProjectPath(p))
      throw new RuntimeException(s"The path '$p' does not appear to be a flix project.")

    // Configure a new Flix object.
    val flix = new Flix()
    flix.setOptions(o)

    // Add all source files.
    for (sourceFile <- getAllFiles(getSourceDirectory(p))) {
      if (sourceFile.getFileName.toString.endsWith(".flix")) {
        flix.addPath(sourceFile)
      }
    }

    // Add all test files.
    for (testFile <- getAllFiles(getTestDirectory(p))) {
      if (testFile.getFileName.toString.endsWith(".flix")) {
        flix.addPath(testFile)
      }
    }

    flix.check() match {
      case Validation.Success(_) => ()
      case Validation.Failure(errors) =>
        implicit val _ = TerminalContext.AnsiTerminal
        errors.foreach(e => println(e.message.fmt))
    }
  }

  /**
    * Builds (compiles) the source files for the given project path `p`.
    */
  def build(p: Path, o: Options, loadClasses: Boolean = true)(implicit tc: TerminalContext): Option[CompilationResult] = {
    // Check that the path is a project path.
    if (!isProjectPath(p))
      throw new RuntimeException(s"The path '$p' does not appear to be a flix project.")

    // Configure a new Flix object.
    val flix = new Flix()
    flix.setOptions(o.copy(targetDirectory = getBuildDirectory(p), loadClassFiles = loadClasses, writeClassFiles = true))

    // Copy all class files from the Flix runtime jar.
    copyRuntimeClassFiles(p)

    // Add all source files.
    for (sourceFile <- getAllFiles(getSourceDirectory(p))) {
      if (sourceFile.getFileName.toString.endsWith(".flix")) {
        flix.addPath(sourceFile)
      }
    }

    // Add all test files.
    for (testFile <- getAllFiles(getTestDirectory(p))) {
      if (testFile.getFileName.toString.endsWith(".flix")) {
        flix.addPath(testFile)
      }
    }

    flix.compile() match {
      case Validation.Success(r) => Some(r)
      case Validation.Failure(errors) =>
        implicit val _ = TerminalContext.AnsiTerminal
        errors.foreach(e => println(e.message.fmt))
        None
    }
  }

  /**
    * Builds a jar package for the given project path `p`.
    */
  def buildJar(p: Path, o: Options)(implicit tc: TerminalContext): Unit = {
    // Check that the path is a project path.
    if (!isProjectPath(p))
      throw new RuntimeException(s"The path '$p' does not appear to be a flix project.")

    // The path to the jar file.
    val jarFile = getJarFile(p)

    // Check whether it is safe to write to the file.
    if (Files.exists(jarFile) && !isJarFile(jarFile)) {
      throw new RuntimeException(s"The path '$jarFile' exists and is not a jar-file. Refusing to overwrite.")
    }

    // Construct a new zip file.
    val zip = new ZipOutputStream(Files.newOutputStream(jarFile))

    // META-INF/MANIFEST.MF
    val manifest =
      """Manifest-Version: 1.0
        |Main-Class: Main
        |""".stripMargin

    // Add manifest file.
    addToZip(zip, "META-INF/MANIFEST.MF", manifest.getBytes)

    // Add all class files.
    for (buildFile <- getAllFiles(getBuildDirectory(p))) {
      val fileName = getBuildDirectory(p).relativize(buildFile).toString
      val fileNameWithSlashes = fileName.replace('\\', '/')
      addToZip(zip, fileNameWithSlashes, buildFile)
    }

    // Close the zip file.
    zip.finish()
  }

  /**
    * Builds a flix package for the given project path `p`.
    */
  def buildPkg(p: Path, o: Options)(implicit tc: TerminalContext): Unit = {
    // Check that the path is a project path.
    if (!isProjectPath(p))
      throw new RuntimeException(s"The path '$p' does not appear to be a flix project.")

    // The path to the fpkg file.
    val pkgFile = getPkgFile(p)

    // Check whether it is safe to write to the file.
    if (Files.exists(pkgFile) && !isPkgFile(pkgFile)) {
      throw new RuntimeException(s"The path '$pkgFile' exists and is not a fpkg-file. Refusing to overwrite.")
    }

    // Construct a new zip file.
    val zip = new ZipOutputStream(Files.newOutputStream(pkgFile))

    // Add required resources.
    addToZip(zip, "HISTORY.md", getHistoryFile(p))
    addToZip(zip, "LICENSE.md", getLicenseFile(p))
    addToZip(zip, "README.md", getReadmeFile(p))
    addToZip(zip, "package.json", getPackageFile(p))

    // Add all source files.
    for (sourceFile <- getAllFiles(getSourceDirectory(p))) {
      val name = p.relativize(sourceFile).toString
      addToZip(zip, name, sourceFile)
    }

    // Add all test files.
    for (testFile <- getAllFiles(getTestDirectory(p))) {
      val name = p.relativize(testFile).toString
      addToZip(zip, name, testFile)
    }

    // Close the zip file.
    zip.finish()
  }

  /**
    * Runs the main function in flix package for the given project path `p`.
    */
  def run(p: Path, o: Options)(implicit tc: TerminalContext): Unit = {
    for {
      compilationResult <- build(p, o)
      main <- compilationResult.getMain
    } yield {
      val exitCode = main(Array.empty)
      println(s"Main exited with status code $exitCode.")
    }
  }

  /**
    * Runs all benchmarks in the flix package for the given project path `p`.
    */
  def benchmark(p: Path, o: Options)(implicit tc: TerminalContext): Unit = {
    build(p, o) match {
      case None => // nop
      case Some(compilationResult) =>
        Benchmarker.benchmark(compilationResult, new PrintWriter(System.out, true))
    }
  }

  /**
    * Runs all tests in the flix package for the given project path `p`.
    */
  def test(p: Path, o: Options)(implicit tc: TerminalContext): Tester.OverallTestResult = {
    build(p, o) match {
      case None => Tester.OverallTestResult.NoTests
      case Some(compilationResult) =>
        val results = Tester.test(compilationResult)
        Console.println(results.output.fmt)
        results.overallResult
    }
  }

  /**
    * Returns a list of sources extracted from the given flix package at path `p`.
    */
  def unpack(p: Path)(implicit flix: Flix): List[Source] = {
    // Check that the path is a flix package.
    if (!isPkgFile(p))
      throw new RuntimeException(s"The path '$p' is not a flix package.")

    // Open the zip file.
    val zip = new ZipFile(p.toFile)

    // Collect all source and test files.
    val result = mutable.ListBuffer.empty[Source]
    val iterator = zip.entries()
    while (iterator.hasMoreElements) {
      val entry = iterator.nextElement()
      val name = entry.getName
      if (name.endsWith(".flix")) {
        val bytes = StreamOps.readAllBytes(zip.getInputStream(entry))
        val array = new String(bytes, flix.defaultCharset).toCharArray
        result += Source(name, array)
      }
    }

    result.toList
  }

  /**
    * Copies all Flix runtime class files into the build directory.
    */
  private def copyRuntimeClassFiles(p: Path): Unit = {
    // Retrieve the Flix runtime JAR file.
    val is = LocalResource.getInputStream("/src/resources/runtime/flix-runtime.jar")
    val zip = new ZipInputStream(is)

    // Iterate through its directories and classes.
    var entry = zip.getNextEntry
    while (entry != null) {
      // Check if the entry is a directory or a file.
      if (entry.isDirectory) {
        // Case 1: The entry is a directory. Recreate the directory (and its parent directories) inside the build directory.
        val directoryPath = getBuildDirectory(p).resolve(entry.getName).normalize()
        Files.createDirectories(directoryPath)
      } else {
        // Case 2: The entry is a file. Verify that it is a class file.
        val classFilePath = getBuildDirectory(p).resolve(entry.getName).normalize()
        if (classFilePath.toString.endsWith(".class")) {
          // The entry is a class file. Write its content to the build directory.
          StreamOps.writeAll(zip, classFilePath)
        }
      }

      // Done with this entry.
      zip.closeEntry()

      // Ready to process the next entry.
      entry = zip.getNextEntry
    }
  }

  /**
    * Returns `true` if the given path `p` appears to be a flix project path.
    */
  private def isProjectPath(p: Path): Boolean =
    Files.exists(getSourceDirectory(p)) &&
      Files.exists(getTestDirectory(p)) &&
      Files.exists(getHistoryFile(p)) &&
      Files.exists(getLicenseFile(p)) &&
      Files.exists(getReadmeFile(p)) &&
      Files.exists(getPackageFile(p))

  /**
    * Returns the package name based on the given path `p`.
    */
  private def getPackageName(p: Path): String = p.toAbsolutePath.getParent.getFileName.toString

  /**
    * Returns the path to the pkg file based on the given path `p`.
    */
  private def getPkgFile(p: Path): Path = p.resolve(getPackageName(p) + ".fpkg").normalize()

  /**
    * Returns the path to the jar file based on the given path `p`.
    */
  private def getJarFile(p: Path): Path = p.resolve(getPackageName(p) + ".jar").normalize()

  /**
    * Returns the path to the build directory relative to the given path `p`.
    */
  private def getBuildDirectory(p: Path): Path = p.resolve("./build/").normalize()

  /**
    * Returns the path to the source directory relative to the given path `p`.
    */
  private def getSourceDirectory(p: Path): Path = p.resolve("./src/").normalize()

  /**
    * Returns the path to the test directory relative to the given path `p`.
    */
  private def getTestDirectory(p: Path): Path = p.resolve("./test/").normalize()

  /**
    * Returns the path to the package file relative to the given path `p`.
    */
  private def getPackageFile(p: Path): Path = p.resolve("./package.sn").normalize()

  /**
    * Returns the path to the HISTORY file relative to the given path `p`.
    */
  private def getHistoryFile(p: Path): Path = p.resolve("./HISTORY.md").normalize()

  /**
    * Returns the path to the LICENSE file relative to the given path `p`.
    */
  private def getLicenseFile(p: Path): Path = p.resolve("./LICENSE.md").normalize()

  /**
    * Returns the path to the README file relative to the given path `p`.
    */
  private def getReadmeFile(p: Path): Path = p.resolve("./README.md").normalize()

  /**
    * Returns the path to the main source file relative to the given path `p`.
    */
  private def getMainSourceFile(p: Path): Path = getSourceDirectory(p).resolve("./Main.flix").normalize()

  /**
    * Returns the path to the main test file relative to the given path `p`.
    */
  private def getMainTestFile(p: Path): Path = getTestDirectory(p).resolve("./TestMain.flix").normalize()

  /**
    * Creates a new directory at the given path `p`.
    *
    * The directory must not already exist.
    */
  private def newDirectory(p: Path): Unit = {
    if (Files.exists(p)) throw InternalCompilerException(s"Path '$p' already exists.")

    Files.createDirectories(p)
  }

  /**
    * Creates a new text file at the given path `p` with the given content `s`.
    *
    * The file must not already exist.
    */
  private def newFile(p: Path)(s: String): Unit = {
    if (Files.exists(p)) throw InternalCompilerException(s"Path '$p' already exists.")

    val writer = Files.newBufferedWriter(p)
    writer.write(s)
    writer.close()
  }

  /**
    * Adds an entry to the given zip file.
    */
  private def addToZip(zip: ZipOutputStream, name: String, p: Path): Unit = {
    addToZip(zip, name, Files.readAllBytes(p))
  }

  /**
    * Adds an entry to the given zip file.
    */
  private def addToZip(zip: ZipOutputStream, name: String, d: Array[Byte]): Unit = {
    val entry = new ZipEntry(name)
    zip.putNextEntry(entry)
    zip.write(d)
    zip.closeEntry()
  }

  /**
    * Returns all files in the given path `p`.
    */
  private def getAllFiles(p: Path): List[Path] = {
    val visitor = new FileVisitor
    Files.walkFileTree(p, visitor)
    visitor.result.toList
  }

  /**
    * Returns `true` if the given path `p` is a jar-file.
    */
  private def isJarFile(p: Path): Boolean = isZipArchive(p)

  /**
    * Returns `true` if the given path `p` is a fpkg-file.
    */
  private def isPkgFile(p: Path): Boolean = isZipArchive(p)

  /**
    * Returns `true` if the given path `p` is a zip-archive.
    */
  private def isZipArchive(p: Path): Boolean = {
    if (Files.exists(p) && Files.isReadable(p) && Files.isRegularFile(p)) {
      // Read the first four bytes of the file.
      val is = Files.newInputStream(p)
      val b1 = is.read()
      val b2 = is.read()
      val b3 = is.read()
      val b4 = is.read()
      is.close()

      // Check if the four first bytes match 0x50, 0x4b, 0x03, 0x04
      return b1 == 0x50 && b2 == 0x4b && b3 == 0x03 && b4 == 0x04
    }
    false
  }

  private class FileVisitor extends SimpleFileVisitor[Path] {
    val result: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      result += file
      FileVisitResult.CONTINUE
    }
  }

}
