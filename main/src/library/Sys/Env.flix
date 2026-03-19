/*
 *  Copyright 2021 Ramiro Calle
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

pub mod Sys.Env {

    use Sys.Env

    import dev.flix.runtime.Global
    import java.lang.Exception
    import java.lang.Runtime
    import java.lang.System
    import java.io.{File => JFile}
    import java.util.{Iterator => JIterator}
    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.util.{Map$Entry => Entry}

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
                k(Array.toList(Global.getArgs()))

            def getEnv(k) = region rc {
                try {
                    let _ = rc;
                    let iter = unsafe IO as rc { System.getenv().entrySet().iterator() };
                    k(getEnvHelper(iter, Map.empty()))
                } catch {
                    case _: Exception => k(Map.empty())
                }
            }

            def getVar(name, k) =
                try {
                    let result = System.getenv(name);
                    k(Object.toOption(result))
                } catch {
                    case _: Exception => k(None)
                }

            def getProp(name, k) = k(getPropHelper(name))

            def getOsName(k) = k(getPropHelper("os.name"))

            def getOsArch(k) = k(getPropHelper("os.arch"))

            def getOsVersion(k) = k(getPropHelper("os.version"))

            def getFileSeparator(k) = k(JFile.separator)

            def getPathSeparator(k) = k(JFile.pathSeparator)

            def getLineSeparator(k) = k(System.lineSeparator())

            def getCurrentWorkingDirectory(k) = k(getPropHelper("user.dir"))

            def getTemporaryDirectory(k) = k(getPropHelper("java.io.tmpdir"))

            def getUserName(k) = k(getPropHelper("user.name"))

            def getUserHomeDirectory(k) = k(getPropHelper("user.home"))

            def getVirtualProcessors(k) = k(Runtime.getRuntime().availableProcessors())
        }

    ///
    /// Runs the `Env` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Env` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Env) + IO = handle(f)()

    def getPropHelper(name: String): Option[String] \ IO =
            try {
                let result = System.getProperty(name);
                Object.toOption(result)
            } catch {
                case _: Exception => None
            }

    def getEnvHelper(iter: JIterator, m: Map[String, String]): Map[String, String] \ IO =
        if (iter.hasNext())
            let e = unchecked_cast(iter.next() as Entry);
            let k = unchecked_cast(e.getKey() as String);
            let v = unchecked_cast(e.getValue() as String);
            getEnvHelper(iter, Map.insert(k, v, m))
        else
            m

}
