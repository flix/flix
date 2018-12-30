package ca.uwaterloo.flix.api

import java.io.{BufferedOutputStream, BufferedReader, FileOutputStream, FileReader}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.util.jar.{Attributes, JarOutputStream}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.collection.mutable

import ca.uwaterloo.flix.util.{InternalCompilerException, StreamOps}

object PackageManager {

  /**
    * Initializes a new flix project in the given path `p`. 
    *
    * The project must not already exist.
    */
  def init(p: Path): Unit = {
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
    * Builds a jar package for the given project path `p`.
    */
  def buildJar(p: Path): Unit = {
    // TODO: Check that this is flix project

    val jarFile = p.resolve(getPackageName(p) + ".jar").normalize()

    if (Files.exists(jarFile)) {
      throw new RuntimeException()
    }

    val manifest = new java.util.jar.Manifest

    val fos = new FileOutputStream(jarFile.toFile)
    val jos = new JarOutputStream(fos, manifest)
    val bos = new BufferedOutputStream(jos)
    for (f <- Nil) {
    }
  }

  /**
    * Builds a flix package for the given project path `p`.
    */
  def buildPkg(p: Path): Unit = {
    // Check that the path is a project path.
    if (!isProjectPath(p))
      throw new RuntimeException(s"The path '$p' does not appear to be a flix project.")

    // The path to the fpkg file.
    val pkgFile = getPkgFile(p)

    // Check that the pkg file does not already exist.
    if (Files.exists(pkgFile)) {
      throw new RuntimeException(s"The path '$pkgFile' already exists. Aborting.")
    }

    // Construct a new zip file.
    val zip = new ZipOutputStream(Files.newOutputStream(pkgFile))

    // Add required resources.
    addEntry(zip, "HISTORY.md", getHistoryFile(p))
    addEntry(zip, "LICENSE.md", getLicenseFile(p))
    addEntry(zip, "README.md", getReadmeFile(p))
    addEntry(zip, "package.json", getPackageFile(p))

    // Add all source files.
    for (sourceFile <- getAllFiles(getSourceDirectory(p))) {
      val name = p.relativize(sourceFile).toString
      addEntry(zip, name, sourceFile)
    }

    // Add all test files.
    for (testFile <- getAllFiles(getTestDirectory(p))) {
      val name = p.relativize(testFile).toString
      addEntry(zip, name, testFile)
    }

    // Close the zip file.
    zip.finish()
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
    * Returns the path to the flix package based on the given path `p`.
    */
  private def getPkgFile(p: Path): Path = p.resolve(getPackageName(p) + ".fpkg").normalize()

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
    * Adds the given path `p` to the given `zip` file with the given `name`.
    */
  private def addEntry(zip: ZipOutputStream, name: String, p: Path): Unit = {
    val entry = new ZipEntry(name)
    zip.putNextEntry(entry)
    zip.write(Files.readAllBytes(p))
    zip.closeEntry()
  }

  /**
    * Returns all files in the source files.
    */
  private def getAllFiles(p: Path): List[Path] = {
    val visitor = new FileVisitor
    Files.walkFileTree(p, visitor)
    visitor.result.toList
  }

  class FileVisitor extends SimpleFileVisitor[Path] {
    val result: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      result += file
      FileVisitResult.CONTINUE
    }
  }

}
