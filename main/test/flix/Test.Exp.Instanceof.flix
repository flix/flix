/*
 * Copyright 2023 Stephen Tetley
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

mod Test.Exp.Instanceof {

    @test
    def testInstanceof01(): Bool = "Hello World" instanceof ##java.lang.String

    @test
    def testInstanceof02(): Bool = "Hello World" instanceof ##java.lang.CharSequence

    @test
    def testInstanceof03(): Bool = "Hello World" instanceof ##java.lang.Object

    @test
    def testInstanceof04(): Bool = 100ii instanceof ##java.math.BigInteger

    @test
    def testInstanceof05(): Bool = 100ii instanceof ##java.lang.Number

    @test
    def testInstanceof06(): Bool = 100ii instanceof ##java.lang.Object

    @test
    def testInstanceof07(): Bool =
        import static get java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: ##java.nio.file.StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof ##java.nio.file.StandardOpenOption

    @test
    def testInstanceof08(): Bool =
        import static get java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: ##java.nio.file.StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof ##java.nio.file.OpenOption

    @test
    def testInstanceof09(): Bool =
        import static get java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: ##java.nio.file.StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof ##java.lang.Enum

    @test
    def testInstanceof10(): Bool =
        import static get java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: ##java.nio.file.StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof ##java.lang.Object

    @test
    def testInstanceof11(): Bool = (100ii instanceof ##java.lang.Long) == false

    @test
    def testInstanceof12(): Bool = ("Hello World" instanceof ##java.lang.StringBuilder) == false

}
