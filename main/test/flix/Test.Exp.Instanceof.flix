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

    import java.lang.{CharSequence, Enum, Long, Object, String => JString, StringBuilder => JStringBuilder}
    import java.math.{BigInteger, Number}
    import java.nio.file.{OpenOption, StandardOpenOption}

    @test
    def testInstanceof01(): Bool = "Hello World" instanceof JString

    @test
    def testInstanceof02(): Bool = "Hello World" instanceof CharSequence

    @test
    def testInstanceof03(): Bool = "Hello World" instanceof Object

    @test
    def testInstanceof04(): Bool = 100ii instanceof BigInteger

    @test
    def testInstanceof05(): Bool = 100ii instanceof Number

    @test
    def testInstanceof06(): Bool = 100ii instanceof Object

    @test
    def testInstanceof07(): Bool =
        import static java_get_field java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof StandardOpenOption

    @test
    def testInstanceof08(): Bool =
        import static java_get_field java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof OpenOption

    @test
    def testInstanceof09(): Bool =
        import static java_get_field java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof Enum

    @test
    def testInstanceof10(): Bool =
        import static java_get_field java.nio.file.StandardOpenOption.DELETE_ON_CLOSE: StandardOpenOption \ {} as get_DELETE_ON_CLOSE;
        get_DELETE_ON_CLOSE() instanceof Object

    @test
    def testInstanceof11(): Bool = (100ii instanceof Long) == false

    @test
    def testInstanceof12(): Bool = ("Hello World" instanceof StringBuilder) == false

    @test
    def testInstanceof13(): Bool \ IO = {
        let arr = Array#{"hi"} @ Static;
        Array.get(0, arr) instanceof String
    }

}
