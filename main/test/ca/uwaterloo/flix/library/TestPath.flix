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
def testNew01(): Path = Path.new("hello.txt")

@test
def testNew02(): Path = Path.new("hello/world.txt")

@test
def testNew03(): Path = Path.new("/tmp/hello.txt")

@test
def testExists01(): Bool = !Path.exists(Path.new("hello.txt"))

@test
def testExists02(): Bool = !Path.exists(Path.new("hello/world.txt"))

@test
def testExists03(): Bool = !Path.exists(Path.new("/tmp/hello.txt"))

@test
def testReadLines01(): List[Str] = Path.readLines(Path.new("README.md"))
