/*
 * Copyright 2019 Magnus Madsen
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

@test
def testNew01(): Channel[Bool] = Channel.new()

@test
def testNew02(): Channel[Int] = Channel.new()

@test
def testNew03(): Channel[Str] = Channel.new()

@test
def testUnbuffered01(): Channel[Bool] = Channel.unbuffered()

@test
def testUnbuffered02(): Channel[Int] = Channel.unbuffered()

@test
def testUnbuffered03(): Channel[Str] = Channel.unbuffered()

@test
def testBuffered01(): Channel[Bool] = Channel.buffered(123)

@test
def testBuffered02(): Channel[Int] = Channel.buffered(123)

@test
def testBuffered03(): Channel[Str] = Channel.buffered(123)

@test
def testGetPut01(): Bool =
    let c = Channel.buffered(1);
    Channel.put(c, true);
    Channel.get(c)

@test
def testGetPut02(): Int =
    let c = Channel.buffered(1);
    Channel.put(c, 123);
    Channel.get(c)

@test
def testGetPut03(): Str =
    let c = Channel.buffered(1);
    Channel.put(c, "Hello World!");
    Channel.get(c)
