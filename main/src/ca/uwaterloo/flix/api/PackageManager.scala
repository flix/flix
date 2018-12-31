package ca.uwaterloo.flix.api

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.util.zip.{ZipEntry, ZipOutputStream}

import ca.uwaterloo.flix.runtime.{CompilationResult, Tester}
import ca.uwaterloo.flix.util.vt.TerminalContext

import scala.collection.mutable
import ca.uwaterloo.flix.util.{InternalCompilerException, Options, Validation}

object PackageManager {

  /**
    * Initializes a new flix project in the given path `p`. 
    *
    * The project must not already exist.
    */
  def init(p: Path, o: Options): Unit = {
    //
    // Check that the current working directory is usable.
    //
    if (!Files.isDirectory(p)) {
      throw new RuntimeException(s"The directory: '$p' is not accessible. Aborting.")
    }
    if (!Files.isReadable(p)) {
      throw new RuntimeException(s"The directory: '$p' is not readable. Aborting.")
    }
    if (!Files.isWritable(p)) {
      throw new RuntimeException(s"The directory: '$p' is not writable. Aborting.")
    }

    //
    // Compute the name of the package based on the directory name.
    //
    val packageName = getPackageName(p)

    //
    // Compute all the directories and files we intend to create.
    //
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
    newDirectory(sourceDirectory)
    newDirectory(testDirectory)

    newFile(packageFile) {
      s"""{
         |  "package": "$packageName",
         |  "version": "0.1.0"
         |}
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
        |def main(): Int = 123
        |""".stripMargin
    }

    newFile(mainTestFile) {
      """@test
        |def testMain01(): Bool = main() == 123
        |""".stripMargin
    }
  }

  /**
    * Builds (compiles) the source files for the given project path `p`.
    */
  def build(p: Path, o: Options): Option[CompilationResult] = {
    // Check that the path is a project path.
    if (!isProjectPath(p))
      throw new RuntimeException(s"The path '$p' does not appear to be a flix project.")

    // Configure a new Flix object.
    val flix = new Flix()
    flix.setOptions(o.copy(writeClassFiles = true))

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
  def buildJar(p: Path, o: Options): Unit = {
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
      """.stripMargin

    // Add manifest file.
    addToZip(zip, "META-INF/MANIFEST.MF", manifest.getBytes)

    // Add all class files.
    for (buildFile <- getAllFiles(getBuildDirectory(p))) {
      val name = getBuildDirectory(p).relativize(buildFile).toString
      addToZip(zip, name, buildFile)
    }

    // Close the zip file.
    zip.finish()
  }

  /**
    * Builds a flix package for the given project path `p`.
    */
  def buildPkg(p: Path, o: Options): Unit = {
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
  def run(p: Path, o: Options): Unit = {
    build(p, o) match {
      case None => // nop
      case Some(compilationResult) =>
        val result = compilationResult.evalToString("main")
        Console.println(result)
    }
  }

  /**
    * Runs all tests in the flix package for the given project path `p`.
    */
  def test(p: Path, o: Options): Unit = {
    build(p, o) match {
      case None => // nop
      case Some(compilationResult) =>
        implicit val _ = TerminalContext.AnsiTerminal
        val results = Tester.test(compilationResult)
        Console.println(results.output.fmt)
    }
  }

  /**
    * Returns `true` if the given path `p` appears to be a flix project path.
    */
  private def isProjectPath(p: Path): Boolean =
    Files.exists(getPackageFile(p)) && Files.exists(getSourceDirectory(p))

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
  private def getBuildDirectory(p: Path): Path = p.resolve("./target/flix/").normalize()

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
  private def getPackageFile(p: Path): Path = p.resolve("./package.json").normalize()

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

    Files.createDirectory(p)
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

  class FileVisitor extends SimpleFileVisitor[Path] {
    val result: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      result += file
      FileVisitResult.CONTINUE
    }
  }

}
