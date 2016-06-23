# The Flix Programming Language

Main repository for the source code of the Flix compiler and run-time.

[See the official Flix website for more information.](https://flix.github.io/)

### Reporting Bugs & Feature Requests

You are most welcome to report bugs or request features on this GitHub page.

Please include information such as the Flix version and JVM version you are using.

### Building with Apache Ant

Flix comes with an Apache Ant build script: build.xml. The build script includes the following targets:

- `build-all`: alias for `build-src, build-jar`.
- `build-jar`: builds the `flix.jar` file.
- `build-src`: compiles all source files.
- `clean`: cleans all build files.
- `rebuild`: alias for `clean, build-all`.
- `run-main`: runs flix (with no input).
- `run-tests`: runs all test cases.

The recommended way to checkout and build Flix is with the commands:

```bash
git clone https://github.com/flix/flix.git
cd flix
ant build-all
ant run-tests
ant run-main
```
