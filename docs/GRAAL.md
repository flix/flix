# Building a Native Image with GraalVM

Flix can be compiled to a native executable with
[GraalVM Native Image](https://www.graalvm.org/latest/reference-manual/native-image/).

The native executable starts in about 10 ms instead of about 200 ms and
type checks faster, but it cannot run compiled Flix programs. See
[Limitations](#limitations).

This is an experimental feature. The JAR remains the supported distribution.

## Prerequisites

### macOS

```bash
xcode-select --install
brew install --cask graalvm-jdk
export JAVA_HOME=$(/usr/libexec/java_home -v 25)
export PATH="$JAVA_HOME/bin:$PATH"
```

### Linux

```bash
sdk install java 25-graalce
sdk use java 25-graalce
```

Native Image invokes the system linker, so the usual build tools must be
installed:

```bash
sudo apt install build-essential zlib1g-dev     # Debian, Ubuntu
sudo dnf install gcc glibc-devel zlib-devel libstdc++-static   # Fedora, RHEL
```

## Building from a JAR

The Native Image configuration ships inside the Flix JAR, so no flags are
needed:

```bash
native-image -jar flix.jar -o flix
```

This takes a few minutes and needs several GB of RAM.

## Building from the repository

```bash
GRAALVM_HOME=$JAVA_HOME ./mill flix.nativeImage
```

The executable is written to `out/flix/nativeImage.dest/native-executable`.

Set `NATIVE_IMAGE_QUICK=1` to build with `-Ob` (quick build mode), which is
faster but produces a slower executable.

Mill reads `GRAALVM_HOME` from its daemon's environment. If a daemon is already
running without it, run `./mill shutdown` first.

## Limitations

The commands that compile a project work as usual:

- `init` — create a new project
- `check` — check the project for errors
- `build` — compile the project
- `build-jar` and `build-fatjar` — build a JAR
- `doc` — generate API documentation
- `format` — format the source code

The `run`, `test`, and `repl` commands do not work. They load the compiled
program into the running JVM, which a native image cannot do. Use
`build-fatjar` and run the result with `java -jar`, or use the JAR
distribution of Flix.
