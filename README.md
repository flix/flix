# The Flix Programming Language

[![Build Status](https://travis-ci.org/flix/flix.svg?branch=master)](https://travis-ci.org/flix/flix)
[![CircleCI](https://circleci.com/gh/flix/flix.svg?style=svg)](https://circleci.com/gh/flix/flix)

Main repository for the source code of the Flix compiler and run-time.

[See the official Flix website for more information.](https://flix.github.io/)

### Reporting Bugs & Feature Requests

You are most welcome to report bugs or request features on this GitHub page.

Please include information such as the Flix version and JVM version you are using.

## Building Flix

### Building with SBT

Flix can be built with the Scala Simple Build Tool (SBT). Ensure SBT is installed and on your path.

The available tasks are:

- `compile`: compiles all sources files.
- `clean`: cleans all build files.
- `run`: runs Flix (with no input).
- `test`: runs all test cases.

### Building with Apache Ant

Flix can be built with Apache Ant. Ensure that Ant is installed and on your path.

The available Ant targets are:

- `build-all`: alias for `build-src, build-jar`.
- `build-src`: compiles all source files.
- `build-jar`: builds the `flix.jar` file.
- `clean`: cleans all build files.
- `rebuild`: alias for `clean, build-all`.
- `main`: runs Flix (with no input).
- `test`: runs all test cases.

The recommended way to checkout and build Flix is with the commands:

```bash
git clone https://github.com/flix/flix.git
cd flix
ant build-all
ant main
ant test
```

### Building with Intellij IDEA

Flix can be built from within the IntelliJ IDEA IDE.

1. Clone the Flix GitHub repository.
2. Create a fresh project on top of the cloned directory.
3. Ensure that `main/src` is marked as a *source* directory.
4. Ensure that `main/test` is marked as a *test* directory.
5. Ensure that every jar in `lib` is added as a library (right-click, "Add as library ...")
6. Open`main/src/ca/uwaterloo/flix/Main.scala` and right-click on `def main` and select `Run 'Main'`
