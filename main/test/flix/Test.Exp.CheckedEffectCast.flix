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

 mod Test/Exp/CheckedEffectCast {

    @test
    def testCheckedEffectCast01(): Unit \ IO = checked_ecast(())

    @test
    def testCheckedEffectCast02(): Bool \ IO = checked_ecast(true)

    @test
    def testCheckedEffectCast03(): Bool \ IO = checked_ecast(not false)

    @test
    def testCheckedEffectCast04(): Char \ IO = checked_ecast('a')

    @test
    def testCheckedEffectCast05(): Char \ IO = checked_ecast('b')

    @test
    def testCheckedEffectCast06(): Char \ IO = checked_ecast('Z')

    @test
    def testCheckedEffectCast07(): Char \ IO = checked_ecast('0')

    @test
    def testCheckedEffectCast08(): Float32 \ IO = checked_ecast(0.0f32)

    @test
    def testCheckedEffectCast09(): Float32 \ IO = checked_ecast(123.4f32)

    @test
    def testCheckedEffectCast10(): Float64 \ IO = checked_ecast(0.0f64)

    @test
    def testCheckedEffectCast11(): Float64 \ IO = checked_ecast(123.4f64)

 }
