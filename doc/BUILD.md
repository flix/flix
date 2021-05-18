# Building and running the compiler

## Building with IntelliJ IDEA

Flix can be built and run from within the IntelliJ IDEA IDE which is useful for working on the compiler and the standard library.

### Setup an IDEA project

1. Clone the Flix GitHub repository.
2. Create a fresh project on top of the cloned directory. Choose Scala project and then "IDEA-based".
   (Do NOT accept "import as Gradle project")
3. Ensure that `main/src` is marked as a *source* directory (right-click -> Mark Dircetory as -> Sources Root).
4. Ensure that `main/test` is marked as a *test* directory (right-click -> Mark Dircetory as -> Test Sources Root).
5. Ensure that every jar in `lib` is added as a library (right-click, "Add as library ...")

### Running the Flix compiler
Open `Main.scala`, right-click on `def main` and select `Run 'Main'`. This runs the Flix REPL.

To compile and run a Flix file with a `main` function, right-click on the main function in `Main.scala` and choose "Modify Run Configuration".
Here enter the file name in the "Program arguments" field.

## Building with Gradle

Flix can also be built with Gradle, but the main developers prefer to build with IntelliJ IDEA.

The Gradle build is used for continuous integration on GitHub.

## Troubleshooting

### Out of memory

If you get a `java.lang.OutOfMemoryError: Java heap space` error, find the setting to increase the heap size. For IDEA projects, this is under `Settings -> Build, Execution, Deployment -> Compiler`. Directly on "Compiler" there is a field "Shared build processes heap" and under `Scala Compiler -> Scala Compiler` there is "Maximum heap size". Try out which one you need to increase. 4GB should be enough in each case.
