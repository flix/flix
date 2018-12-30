package ca.uwaterloo.flix.api

import java.nio.file.{Files, Path}

object PackageManager {

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
    val mainSourceFile = getMainSourceFile(p)

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
    Files.createDirectory(sourceDirectory)
    createFileWithString(mainSourceFile) {
      """// Main entry point.
        |def main(): Int = 123
      """.stripMargin
    }

    // TODO: Create a manifest file.
    // TODO: Create a dep file.
    // TODO: Create a main file.
    // TODO: src and test folders.

    // Return success.
    return 0
  }

  private def getSourceDirectory(cwd: Path): Path = cwd.resolve("./src/").normalize()

  private def getTargetDirectory(cwd: Path): Path = cwd.resolve("./target/").normalize()

  private def getMainSourceFile(cwd: Path): Path = getSourceDirectory(cwd).resolve("./main.flix").normalize()

  private def createFileWithString(p: Path)(s: String): Unit = {
    val writer = Files.newBufferedWriter(p)
    writer.write(s)
    writer.close()
  }

}
