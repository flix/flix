/*
 * Copyright 2023 Jakob Schneider Villumsen
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

mod Test.Exp.UncheckedTypeCast {

    import java.lang.Integer
    import java.lang.Object
    import java.io.Serializable
    import java.lang.StringBuilder

    @test
    def testUncheckedTypeCast01(): Object = unchecked_cast(null as Object)

    @test
    def testUncheckedTypeCast02(): String = unchecked_cast(null as String)

    @test
    def testUncheckedTypeCast03(): Integer = unchecked_cast(null as Integer)

    @test
    def testUncheckedTypeCast04(): Serializable = unchecked_cast(null as Serializable)

    @test
    def testUncheckedTypeCast05(): Serializable = region rc {
        let _ = rc;
        unchecked_cast(unsafe new StringBuilder() as Serializable)
    }

    @test
    def testUncheckedTypeCast06(): Object = region rc {
        let _ = rc;
        unchecked_cast(unsafe new StringBuilder() as Object)
    }

}
