/*
 * Copyright 2021 Ramiro Calle
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
eff Env {

    ///
    /// Returns the arguments passed to the program via the command line.
    ///
    def getArgs(): List[String]

    ///
    /// Returns an map of the current system environment.
    ///
    def getEnv(): Map[String, String]

    ///
    /// Returns the value of the specified environment variable.
    ///
    def getVar(name: String): Option[String]

    ///
    /// Returns the system property by name.
    ///
    def getProp(name: String): Option[String]

    ///
    /// Returns the operating system name
    ///
    def getOsName(): Option[String]

    ///
    /// Returns the operating system architecture
    ///
    def getOsArch(): Option[String]

    ///
    /// Returns the operating system version
    ///
    def getOsVersion(): Option[String]

    ///
    /// Returns the file separator
    ///
    def getFileSeparator(): String

    ///
    /// Returns the path separator
    ///
    def getPathSeparator(): String

    ///
    /// Returns the system line separator
    ///
    def getLineSeparator(): String

    ///
    /// Returns the user's current working directory
    ///
    def getCurrentWorkingDirectory(): Option[String]

    ///
    /// Returns the default temp file path
    ///
    def getTemporaryDirectory(): Option[String]

    ///
    /// Returns the user's account name
    ///
    def getUserName(): Option[String]

    ///
    /// Returns the user's home directory
    ///
    def getUserHomeDirectory(): Option[String]

    ///
    /// Returns the number of virtual processors available to the JVM.
    ///
    /// The returned value is never less than one.
    ///
    def getVirtualProcessors(): Int32

}

pub mod Env {

    ///
    /// Handles the `Env` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Env` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Env) + IO = x ->
        run {
            f(x)
        } with handler Env {
            def getArgs(k) =
                region rc {
                    k(Array.toList(%%ENV_GET_ARGS%%(rc)))
                }

            def getEnv(k) = region rc {
                let pairs = %%ENV_GET_ENV_PAIRS%%(rc);
                k(getEnvFromPairs(pairs, 0, Map.empty()))
            }

            def getVar(name, k) =
                k(Object.toOption(%%ENV_GET_VAR%%(name)))

            def getProp(name, k) = k(Object.toOption(%%ENV_GET_PROP%%(name)))

            def getOsName(k) = k(Object.toOption(%%ENV_GET_PROP%%("os.name")))

            def getOsArch(k) = k(Object.toOption(%%ENV_GET_PROP%%("os.arch")))

            def getOsVersion(k) = k(Object.toOption(%%ENV_GET_PROP%%("os.version")))

            def getFileSeparator(k) = k(%%FILE_SEPARATOR%%(()))

            def getPathSeparator(k) = k(%%PATH_SEPARATOR%%(()))

            def getLineSeparator(k) = k(%%LINE_SEPARATOR%%(()))

            def getCurrentWorkingDirectory(k) = k(Object.toOption(%%ENV_GET_PROP%%("user.dir")))

            def getTemporaryDirectory(k) = k(Object.toOption(%%ENV_GET_PROP%%("java.io.tmpdir")))

            def getUserName(k) = k(Object.toOption(%%ENV_GET_PROP%%("user.name")))

            def getUserHomeDirectory(k) = k(Object.toOption(%%ENV_GET_PROP%%("user.home")))

            def getVirtualProcessors(k) = k(%%ENV_VIRTUAL_PROCESSORS%%(()))
        }

    ///
    /// Runs the `Env` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Env` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Env) + IO = handle(f)()

    def getEnvFromPairs(pairs: Array[String, r], i: Int32, m: Map[String, String]): Map[String, String] \ r =
        if (i + 1 < Array.length(pairs))
            let k = Array.get(i, pairs);
            let v = Array.get(i + 1, pairs);
            getEnvFromPairs(pairs, i + 2, Map.insert(k, v, m))
        else
            m

}
