# Building and running the compiler

## Building with IntelliJ IDEA

Flix can be built and run from within the IntelliJ IDEA IDE which is useful for working on the compiler and the standard library.

### Setup an IDEA project

1. Open IDEA and choose "Get from VCS"
2. IDEA should automatically detect that this is a Gradle project
3. In "Settings |Build, Execution, Deployment | Build Tools | Gradle":
    1. Set "Build and run using:" to "IntelliJ IDEA"
    2. Set "Run tests using:" to "IntelliJ IDEA"

### Running the Flix compiler

Traverse to `main/src/ca/uwaterloo/flix/Main.scala`

Open `Main.scala`, right-click on `def main` and select `Run 'Main'`. This runs the Flix REPL.

To compile and run a Flix file with a `main` function, right-click on the main function in `Main.scala` and choose "Modify Run Configuration".
Here enter the file name in the "Program arguments" field.

### Running tests

Open `TestAll.scala`, right click on `class TestAll` and select `Run 'TestAll'`

### Formatting

Settings > Editor > Code Style > Scala > ScalaDoc > check "add additional space for leading asterisk"

## Building with Gradle

Flix can also be built with Gradle, but the main developers prefer to build with IntelliJ IDEA.

The Gradle build is used for continuous integration on GitHub.

## Troubleshooting

### Out of memory

If you get a `java.lang.OutOfMemoryError: Java heap space` error, find the setting to increase the heap size. For IDEA projects, this is under `Settings -> Build, Execution, Deployment -> Compiler`. Directly on "Compiler" there is a field "Shared build processes heap" and under `Scala Compiler -> Scala Compiler` there is "Maximum heap size". Try out which one you need to increase. 4GB should be enough in each case.

### Problems with imports from libraries

Check that all jar files in the `lib` folder are added as library (see above).
When a new library is added or an existing one is updated, it has to be added (again) this way.

### Build problems in IntelliJ

Sometimes it helps running `Build -> Rebuild Project` which will fix any cache issues.
