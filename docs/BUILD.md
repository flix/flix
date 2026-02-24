# Building and Running the Flix compiler

## Building with IntelliJ IDEA

Flix can be built and run from within the IntelliJ IDEA IDE which is useful for
working on the compiler and the standard library.

### Setup IDEA project

1. Open IDEA and choose "Get from VCS"
2. In "Settings | Build, Execution, Deployment | Build Tools":
    1. Set "Build and run using:" to "IntelliJ IDEA"
    2. Set "Run tests using:" to "IntelliJ IDEA"

### Running the Flix compiler

Traverse to `main/src/ca/uwaterloo/flix/Main.scala`

Open `Main.scala`, right-click on `def main` and select `Run 'Main'`. This runs
the Flix REPL.

To compile and run a Flix file with a `main` function, right-click on the main
function in `Main.scala` and choose "Modify Run Configuration".

Here enter the file name in the "Program arguments" field.

### Running tests

Open `TestAll.scala`, right click on `class TestAll` and select `Run 'TestAll'`

### Testing VSCode

Create a `.env` file in the project root with:

```
VSCODE_PATH=/path/to/vscode/project
```

Then run `./mill flix.vscode` to build a Flix `jar` and copy it to the target
directory.

Open the directory in VSCode: `File -> Open Folder ...` and test!

### Formatting

Go to `Settings > Editor > Code Style > Scala > ScalaDoc` and check "add
additional space for leading asterisk".

## Building with Mill

Flix can also be built with Mill, but the main developers prefer to build with
IntelliJ IDEA.

The Mill build is used for continuous integration on GitHub.

Common commands:

- `./mill flix.compile` — compile the compiler
- `./mill flix.test` — run all tests
- `./mill flix.assembly` — build a fat JAR
- `./mill flix.testPackageManager` — run package manager tests
- `./mill flix.testFuzzerSuite` — run fuzzer tests
- `./mill flix.testIDECompletion` — run IDE completion tests

The Mill version is pinned in `.mill-version` and JVM options are configured in
`.mill-jvm-opts`.

## Troubleshooting

### Out of memory

If you get a `java.lang.OutOfMemoryError: Java heap space` error, find the
setting to increase the heap size. For IDEA projects, this is under `Settings ->
Build, Execution, Deployment -> Compiler`. Directly on "Compiler" there is a
field "Shared build processes heap" and under `Scala Compiler -> Scala Compiler`
there is "Maximum heap size". Try out which one you need to increase. 4GB should
be enough in each case.

For Mill builds, JVM options are configured in `.mill-jvm-opts` (currently
`-Xmx4g`).

### Build problems in IntelliJ

Sometimes it helps running `Build -> Rebuild Project` which will fix any cache
issues.
