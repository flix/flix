/*
 * Copyright 2022 Stephen Tetley
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

namespace TestBigDecimal {

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = BigDecimal.min(1.0ff, 2.0ff) == 1.0ff

    @test
    def min02(): Bool = BigDecimal.min(2.0ff, -1.0ff) == -1.0ff

    @test
    def min03(): Bool = BigDecimal.min(-33.0ff, -66.0ff) == -66.0ff

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = BigDecimal.max(48.0ff, 49.0ff) == 49.0ff

    @test
    def max02(): Bool = BigDecimal.max(4.0ff, -16.0ff) == 4.0ff

    @test
    def max03(): Bool = BigDecimal.max(-34.0ff, -16.0ff) == -16.0ff

}
