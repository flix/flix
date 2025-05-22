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
            case B(x) => x == 1
        }
    }
}
