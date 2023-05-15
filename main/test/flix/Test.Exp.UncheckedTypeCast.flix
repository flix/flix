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

    @test
    def testUncheckedTypeCast01(): ##java.lang.Object = unchecked_cast(null as ##java.lang.Object)

    @test
    def testUncheckedTypeCast02(): ##java.lang.String = unchecked_cast(null as ##java.lang.String)

    @test
    def testUncheckedTypeCast03(): ##java.lang.Integer = unchecked_cast(null as ##java.lang.Integer)

    @test
    def testUncheckedTypeCast04(): ##java.io.Serializable = unchecked_cast(null as ##java.io.Serializable)

    @test
    def testUncheckedTypeCast05(): ##java.io.Serializable = region rh {
        let _ = rh;
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ rh as mkSb;
        unchecked_cast(mkSb() as ##java.io.Serializable)
    }

    @test
    def testUncheckedTypeCast06(): ##java.lang.Object = region rh {
        let _ = rh;
        import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ rh as mkSb;
        unchecked_cast(mkSb() as ##java.lang.Object)
    }

}
