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
        import java.lang.System:getenv();
        import java.util.Map.entrySet();
        import java.util.Set.iterator();
        try {
            let iter = getenv().entrySet().iterator();
            getEnvHelper(iter, Map.empty())
        } catch {
            case _:##java.lang.Exception => Map.empty()
        } as & Pure

    ///
    /// Gets the value of the specified environment variable.
    ///
    pub def getVar(name: String): Option[String] =
        import java.lang.System:getenv(String);
        try {
            let result = getenv(name) as & Pure;
            Object.toOption(result)
        } catch {
            case _:##java.lang.Exception => None
        }

    ///
    /// Gets the system property by name.
    ///
    pub def getProp(name: String): Option[String] =
        import java.lang.System:getProperty(String);
        try {
            let result = getProperty(name) as & Pure;
            Object.toOption(result)
        } catch {
            case _:##java.lang.Exception => None
        }

    ///
    /// Operating system name
    ///
    pub def getOsName(): Option[String] = getProp("os.name")

    ///
    /// Operating system architecture
    ///
    pub def getOsArch(): Option[String] = getProp("os.arch")

    ///
    /// Operating system version
    ///
    pub def getOsVersion(): Option[String] = getProp("os.version")

    ///
    /// File separator
    ///
    pub def getFileSeparator(): String =
        import get java.io.File:separator as sep;
        sep() as String & Pure

    ///
    /// Path separator
    ///
    pub def getPathSeparator(): String =
        import get java.io.File:pathSeparator as pathSep;
        pathSep() as String & Pure

    ///
    /// Line separator
    ///
    pub def getLineSeparator(): String =
        import java.lang.System:lineSeparator();
        lineSeparator() as String & Pure

    ///
    /// User's current working directory
    ///
    pub def getCurrentWorkingDirectory(): Option[String] = getProp("user.dir")

    ///
    /// Default temp file path
    ///
    pub def getTemporaryDirectory(): Option[String] = getProp("java.io.tmpdir")

    ///
    /// User's account name
    ///
    pub def getUserName(): Option[String] = getProp("user.name")

    ///
    /// User's home directory
    ///
    pub def getUserHomeDirectory(): Option[String] = getProp("user.home")

    ///
    /// Returns the number of virtual processors available to the JVM.
    ///
    /// The returned value is never less than one.
    ///
    pub def getVirtualProcessors(): Int32 =
        import java.lang.Runtime:getRuntime();
        import java.lang.Runtime.availableProcessors();
        getRuntime().availableProcessors() as & Pure

    def getEnvHelper(iter: ##java.util.Iterator, m: Map[String, String]): Map[String, String] & Impure =
        import java.util.Iterator.hasNext();
        import java.util.Iterator.next();
        import java.util.Map$Entry.getKey();
        import java.util.Map$Entry.getValue();
        if (iter.hasNext())
            let entry = iter.next() as ##java.util.Map$Entry;
            let key = entry.getKey() as String;
            let value = entry.getValue() as String;
            getEnvHelper(iter, Map.insert(key, value, m))
        else
            m

}
