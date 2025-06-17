/*
 * Copyright 2025 Jakob Schneider Villumsen
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
mod Test.Exp.ExtMatch {

    @Test
    def testExtMatch01BigInt(): Bool = {
        ematch xvar A(1ii) {
            case A(x) => x == 1ii
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02BigInt(): Bool = {
        ematch xvar A(1ii) {
            case A(x) => x == 1ii
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03BigInt(): Bool = {
        ematch xvar B(1ii) {
            case B(x) => x == 1ii
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04BigInt(): Bool = {
        ematch xvar B(1ii) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Bool(): Bool = {
        ematch xvar A(true) {
            case A(x) => x
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Bool(): Bool = {
        ematch xvar A(true) {
            case A(x) => x
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Bool(): Bool = {
        ematch xvar B(true) {
            case B(x) => x
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Bool(): Bool = {
        ematch xvar B(false) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Char(): Bool = {
        ematch xvar A('a') {
            case A(x) => x == 'a'
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Char(): Bool = {
        ematch xvar A('a') {
            case A(x) => x == 'a'
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Char(): Bool = {
        ematch xvar B('a') {
            case B(x) => x == 'a'
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Char(): Bool = {
        ematch xvar B('a') {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Float32(): Bool = {
        ematch xvar A(1.0f32) {
            case A(x) => x == 1.0f32
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Float32(): Bool = {
        ematch xvar A(1.0f32) {
            case A(x) => x == 1.0f32
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Float32(): Bool = {
        ematch xvar B(1.0f32) {
            case B(x) => x == 1.0f32
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Float32(): Bool = {
        ematch xvar B(1.0f32) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Float64(): Bool = {
        ematch xvar A(1.0f64) {
            case A(x) => x == 1.0f64
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Float64(): Bool = {
        ematch xvar A(1.0f64) {
            case A(x) => x == 1.0f64
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Float64(): Bool = {
        ematch xvar B(1.0f64) {
            case B(x) => x == 1.0f64
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Float64(): Bool = {
        ematch xvar B(1.0f64) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int8(): Bool = {
        ematch xvar A(1i8) {
            case A(x) => x == 1i8
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Int8(): Bool = {
        ematch xvar A(1i8) {
            case A(x) => x == 1i8
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int8(): Bool = {
        ematch xvar B(1i8) {
            case B(x) => x == 1i8
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Int8(): Bool = {
        ematch xvar B(1i8) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int16(): Bool = {
        ematch xvar A(1i16) {
            case A(x) => x == 1i16
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Int16(): Bool = {
        ematch xvar A(1i16) {
            case A(x) => x == 1i16
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int16(): Bool = {
        ematch xvar B(1i16) {
            case B(x) => x == 1i16
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Int16(): Bool = {
        ematch xvar B(1i16) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int32(): Bool = {
        ematch xvar A(1) {
            case A(x) => x == 1
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Int32(): Bool = {
        ematch xvar A(1) {
            case A(x) => x == 1
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int32(): Bool = {
        ematch xvar B(1) {
            case B(x) => x == 1
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Int32(): Bool = {
        ematch xvar B(1) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int64(): Bool = {
        ematch xvar A(1i64) {
            case A(x) => x == 1i64
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02Int64(): Bool = {
        ematch xvar A(1i64) {
            case A(x) => x == 1i64
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int64(): Bool = {
        ematch xvar B(1i64) {
            case B(x) => x == 1i64
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04Int64(): Bool = {
        ematch xvar B(1i64) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01String(): Bool = {
        ematch xvar A("a") {
            case A(x) => x == "a"
            case A(_) => false
        }
    }

    @Test
    def testExtMatch02String(): Bool = {
        ematch xvar A("a") {
            case A(x) => x == "a"
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03String(): Bool = {
        ematch xvar B("a") {
            case B(x) => x == "a"
            case B(_) => false
        }
    }

    @Test
    def testExtMatch04String(): Bool = {
        ematch xvar B("a") {
            case A(_) => false
            case B(_) => true
        }
    }
}
