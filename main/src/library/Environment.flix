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
namespace Environment {

    ///
    /// Returns an map of the current system environment.
    ///
    pub def getEnv(): Map[String, String] =
        import static java.lang.System.getenv(): ##java.util.Map & Impure;
        import java.util.Map.entrySet(): ##java.util.Set & Impure;
        import java.util.Set.iterator(): ##java.util.Iterator & Impure;
        try {
            let iter = getenv() |> entrySet |> iterator;
            getEnvHelper(iter, Map.empty())
        } catch {
            case _:##java.lang.Exception => Map.empty()
        } as & Pure

    ///
    /// Returns the value of the specified environment variable.
    ///
    pub def getVar(name: String): Option[String] =
        import static java.lang.System.getenv(String): String & Pure;
        try {
            let result = getenv(name);
            Object.toOption(result)
        } catch {
            case _:##java.lang.Exception => None
        }

    ///
    /// Returns the system property by name.
    ///
    pub def getProp(name: String): Option[String] =
        import static java.lang.System.getProperty(String): String & Pure;
        try {
            let result = getProperty(name);
            Object.toOption(result)
        } catch {
            case _:##java.lang.Exception => None
        }

    ///
    /// Returns the arguments passed to the program via the command line.
    ///
    @Experimental
    pub def getArgs(): Array[String] & Impure = []

    ///
    /// Returns the operating system name
    ///
    pub def getOsName(): Option[String] = getProp("os.name")

    ///
    ///  Returns the operating system architecture
    ///
    pub def getOsArch(): Option[String] = getProp("os.arch")

    ///
    ///  Returns the operating system version
    ///
    pub def getOsVersion(): Option[String] = getProp("os.version")

    ///
    ///  Returns the file separator
    ///
    pub def getFileSeparator(): String =
        import static get java.io.File.separator: String & Pure as sep;
        sep()

    ///
    /// Returns the path separator
    ///
    pub def getPathSeparator(): String =
        import static get java.io.File.pathSeparator: String & Pure as pathSep;
        pathSep()

    ///
    /// Returns the system line separator
    ///
    pub def getLineSeparator(): String =
        import static java.lang.System.lineSeparator(): String & Pure;
        lineSeparator()

    ///
    /// Returns the user's current working directory
    ///
    pub def getCurrentWorkingDirectory(): Option[String] = getProp("user.dir")

    ///
    /// Returns the default temp file path
    ///
    pub def getTemporaryDirectory(): Option[String] = getProp("java.io.tmpdir")

    ///
    /// Returns the user's account name
    ///
    pub def getUserName(): Option[String] = getProp("user.name")

    ///
    /// Returns the user's home directory
    ///
    pub def getUserHomeDirectory(): Option[String] = getProp("user.home")

    ///
    /// Returns the number of virtual processors available to the JVM.
    ///
    /// The returned value is never less than one.
    ///
    pub def getVirtualProcessors(): Int32 =
        import static java.lang.Runtime.getRuntime(): ##java.lang.Runtime & Pure;
        import java.lang.Runtime.availableProcessors(): Int32 & Pure;
        (getRuntime() |> availableProcessors)

    def getEnvHelper(iter: ##java.util.Iterator, m: Map[String, String]): Map[String, String] & Impure =
        import java.util.Iterator.hasNext(): Bool & Impure;
        import java.util.Iterator.next(): ##java.lang.Object & Impure;
        import java.util.Map$Entry.getKey(): ##java.lang.Object & Impure;
        import java.util.Map$Entry.getValue(): ##java.lang.Object & Impure;
        if (hasNext(iter))
            let entry = next(iter) as ##java.util.Map$Entry;
            let key = getKey(entry) as String;
            let value = getValue(entry) as String;
            getEnvHelper(iter, Map.insert(key, value, m))
        else
            m

}
