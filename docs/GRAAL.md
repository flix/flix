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

If you download GraalVM through a browser instead, macOS quarantines it and
Gatekeeper refuses to run it. Remove the flag with:

```bash
sudo xattr -r -d com.apple.quarantine /path/to/graalvm
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

These commands print an explanatory message and exit:

- `run`, `test`, and `repl`
- `lsp` (`lsp-vscode` works)
- `--listen`
- passing a `.flix` file directly

They load the compiled program into the running JVM, which a native image
cannot do. Use `build-fatjar` and run the result with `java -jar`, or use the
JAR distribution of Flix.

Everything else works, including `check`, `build`, `build-jar`,
`build-fatjar`, `doc`, and `format`.

## Portability

A native executable runs only on the operating system and CPU architecture it
was built on, and on Linux it links against the glibc of the build machine, so
it may not start on an older distribution. Build on the oldest system you
intend to support.
