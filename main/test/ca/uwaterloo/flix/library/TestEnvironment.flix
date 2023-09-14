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
mod TestEnvironment {

    @Test
    def testGetEnv01(): Bool =
        not Map.isEmpty(Environment.getEnv())

    @Test
    def testGetVar01(): Bool =
        not Option.isEmpty(Environment.getVar("PATH"))

    @Test
    def testGetVar02(): Bool =
        Option.isEmpty(Environment.getVar("UNKNOWN"))

    @Test
    def testGetProp02(): Bool =
        Option.isEmpty(Environment.getProp("UNKNOWN"))

    @Test
    def testGetOsName(): Bool =
        not Option.isEmpty(Environment.getOsName())

    @Test
    def testGetOsArch(): Bool =
        not Option.isEmpty(Environment.getOsArch())

    @Test
    def testGetOsVersion(): Bool =
        not Option.isEmpty(Environment.getOsVersion())

    @Test
    def testGetFileSeparator(): Bool =
        not String.isEmpty(Environment.getFileSeparator())

    @Test
    def testGetPathSeparator(): Bool =
        not String.isEmpty(Environment.getPathSeparator())

    @Test
    def testGetLineSeparator(): Bool =
        not String.isEmpty(Environment.getLineSeparator())

    @Test
    def testGetCurrentWorkingDirectory(): Bool =
        not Option.isEmpty(Environment.getCurrentWorkingDirectory())

    @Test
    def testGetTemporaryDirectory(): Bool =
        not Option.isEmpty(Environment.getTemporaryDirectory())

    @Test
    def testGetUserName(): Bool =
        not Option.isEmpty(Environment.getUserName())

    @Test
    def testGetUserHomeDirectory(): Bool =
        not Option.isEmpty(Environment.getUserHomeDirectory())

    @Test
    def testGetVirtualProcessors(): Bool =
        Environment.getVirtualProcessors() > 0

    @Test
    def testGetArgs(): List[String] = Environment.getArgs()

}
