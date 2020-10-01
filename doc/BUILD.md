# Build Instructions

## Building with Intellij IDEA

Flix can be built from within the IntelliJ IDEA IDE.

1. Clone the Flix GitHub repository.
2. Create a fresh project on top of the cloned directory.
   (Do NOT accept "import as gradle project")
3. Ensure that `main/src` is marked as a *source* directory.
4. Ensure that `main/test` is marked as a *test* directory.
5. Ensure that every jar in `lib` is added as a library (right-click, "Add as library ...")
6. Open `Main.scala` and right-click on `def main` and select `Run 'Main'`

## Building with Gradle

Flix can also be built with Gradle, but the main developers prefer to build with Intellij IDEA.

The Gradle build is used for continuous integration on GitHub.
