package ca.uwaterloo.flix.api

import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.util.InternalCompilerException

object PackageManager {

  /**
    * Initializes a new flix project in the given path `p`. 
    *
    * The project must not already exist.
    */
  def init(p: Path): Int = {

    //
    // Check that the current working directory is usable.
    //
    if (!Files.isDirectory(p)) {
      Console.println(s"The directory: '$p' is not accessible. Aborting.")
      return 1
    }
    if (!Files.isReadable(p)) {
      Console.println(s"The directory: '$p' is not readable. Aborting.")
      return 1
    }
    if (!Files.isWritable(p)) {
      Console.println(s"The directory: '$p' is not writable. Aborting.")
      return 1
    }

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
    if (Files.exists(sourceDirectory)) {
      Console.println(s"The path: '$sourceDirectory' already exists. Aborting.")
      return 1
    }

    //
    // Create the project directories and files.
    //
    newDirectory(sourceDirectory)


    newFile(mainSourceFile) {
      """// Main entry point.
        |def main(): Int = 123
      """.stripMargin
    }

    // Return success.
    return 0
  }

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

}
