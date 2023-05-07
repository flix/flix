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
mod Environment {

    ///
    /// Returns the arguments passed to the program via the command line.
    ///
    pub def getArgs(): List[String] = region rc {
        import static dev.flix.runtime.Global.getArgs(): Array[String, rc] \ Write(rc);
        let _ = rc; // Avoids redundancy error.
        Array.toList(getArgs())
    }

    ///
    /// Returns an map of the current system environment.
    ///
    pub def getEnv(): Map[String, String] = region rc {
        import static java.lang.System.getenv(): ##java.util.Map \ Write(rc);
        import java.util.Map.entrySet(): ##java.util.Set \ { Read(rc), Write(rc) };
        import java.util.Set.iterator(): ##java.util.Iterator \ { Read(rc), Write(rc) };
        let _ = rc; // Avoids redundancy error.
        try {
            let iter = getenv() |> entrySet |> iterator;
            getEnvHelper(iter, Map.empty())
        } catch {
            case _:##java.lang.Exception => Map.empty()
        }
    }

    ///
    /// Returns the value of the specified environment variable.
    ///
    pub def getVar(name: String): Option[String] =
        import static java.lang.System.getenv(String): String \ {};
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
        import static java.lang.System.getProperty(String): String \ {};
        try {
            let result = getProperty(name);
            Object.toOption(result)
        } catch {
            case _:##java.lang.Exception => None
        }

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
        import static get java.io.File.separator: String \ {} as sep;
        sep()

    ///
    /// Returns the path separator
    ///
    pub def getPathSeparator(): String =
        import static get java.io.File.pathSeparator: String \ {} as pathSep;
        pathSep()

    ///
    /// Returns the system line separator
    ///
    pub def getLineSeparator(): String =
        import static java.lang.System.lineSeparator(): String \ {};
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
        import static java.lang.Runtime.getRuntime(): ##java.lang.Runtime \ {};
        import java.lang.Runtime.availableProcessors(): Int32 \ {};
        (getRuntime() |> availableProcessors)

    def getEnvHelper(iter: ##java.util.Iterator, m: Map[String, String]): Map[String, String] =
        import java.util.Iterator.hasNext(): Bool \ {};
        import java.util.Iterator.next(): ##java.lang.Object \ {};
        import java.util.Map$Entry.getKey(): ##java.lang.Object \ {};
        import java.util.Map$Entry.getValue(): ##java.lang.Object \ {};
        if (hasNext(iter))
            let e = unchecked_cast(next(iter) as ##java.util.Map$Entry);
            let k = unchecked_cast(getKey(e) as String);
            let v = unchecked_cast(getValue(e) as String);
            getEnvHelper(iter, Map.insert(k, v, m))
        else
            m

}
