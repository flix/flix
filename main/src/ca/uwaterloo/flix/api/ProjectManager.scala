package ca.uwaterloo.flix.api

import java.nio.file.{Files, Path}

object ProjectManager {

  def init(cwd: Path): Int = {

    //
    // Check that the current working directory is usable.
    //
    if (!Files.isDirectory(cwd)) {
      Console.println(s"The directory: '$cwd' is not accessible. Aborting.")
      return 1
    }
    if (!Files.isReadable(cwd)) {
      Console.println(s"The directory: '$cwd' is not readable. Aborting.")
      return 1
    }
    if (!Files.isWritable(cwd)) {
      Console.println(s"The directory: '$cwd' is not writable. Aborting.")
      return 1
    }

    //
    // Compute all the directories and files we intend to create.
    //
    val sourceDirectory = getSourceDirectory(cwd)

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

    //    val writer = Files.newBufferedWriter(p)

    // TODO: Create a manifest file.
    // TODO: Create a dep file.
    // TODO: Create a main file.
    // TODO: src and test folders.

    Console.println(getMainSourceFile(cwd))


    return 0
  }

  private def getSourceDirectory(cwd: Path): Path = cwd.resolve("./src/").normalize()

  private def getTargetDirectory(cwd: Path): Path = cwd.resolve("./target/").normalize()

  private def getMainSourceFile(cwd: Path): Path = getSourceDirectory(cwd).resolve("./main.flix").normalize()


}
