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

use Core/Io/File.{canonical, exists, File, getChildren, getName, getParent, isDirectory, isExecutable, isFile, isReadable, isSymbolicLink, isWritable, new, newFromFragments, readLines, resolve};

@test
def new01(): File & Impure = new("hello.txt")

@test
def new02(): File & Impure = new("hello/world.txt")

@test
def new03(): File & Impure = new("/tmp/hello.txt")

@test
def exists01(): Bool & Impure = exists(new("hello.txt")) |> Result.isOk

@test
def exists02(): Bool & Impure = exists(new("./doc/")) |> Result.isOk

@test
def exists03(): Bool & Impure = exists(new("./doc/foo")) |> Result.isOk

@test
def getName01(): Bool & Impure = getName(new("hello.txt")) == Ok("hello.txt")

@test
def getName02(): Bool & Impure = getName(new("./doc/")) == Ok("doc")

@test
def getName03(): Bool & Impure = getName(new("./doc/foo")) == Ok("foo")

@test
def getParent01(): Bool & Impure = getParent(new("hello.txt")) |> Result.isOk

@test
def getParent02(): Bool & Impure = getParent(new("./doc/")) |> Result.isOk

@test
def getParent03(): Bool & Impure = getParent(new("./doc/foo")) |> Result.isOk

@test
def getChildren01(): Bool & Impure = getChildren(new("hello.txt")) |> Result.isOk

@test
def getChildren02(): Bool & Impure = getChildren(new("./doc/")) |> Result.isOk

@test
def getChildren03(): Bool & Impure = getChildren(new("./doc/foo")) |> Result.isOk

@test
def isDirectory01(): Bool & Impure = isDirectory(new("hello.txt")) |> Result.isOk

@test
def isDirectory02(): Bool & Impure = isDirectory(new("./doc/")) |> Result.isOk

@test
def isDirectory03(): Bool & Impure = isDirectory(new("./doc/foo")) |> Result.isOk

@test
def isExecutable01(): Bool & Impure = isExecutable(new("hello.txt")) |> Result.isOk

@test
def isExecutable02(): Bool & Impure = isExecutable(new("./doc/")) |> Result.isOk

@test
def isExecutable03(): Bool & Impure = isExecutable(new("./doc/foo")) |> Result.isOk

@test
def isReadable01(): Bool & Impure = isReadable(new("hello.txt")) |> Result.isOk

@test
def isReadable02(): Bool & Impure = isReadable(new("./doc/")) |> Result.isOk

@test
def isReadable03(): Bool & Impure = isReadable(new("./doc/foo")) |> Result.isOk

@test
def isFile01(): Bool & Impure = isFile(new("hello.txt")) |> Result.isOk

@test
def isFile02(): Bool & Impure = isFile(new("./doc/")) |> Result.isOk

@test
def isFile03(): Bool & Impure = isFile(new("./doc/foo")) |> Result.isOk

@test
def isSymbolicLink01(): Bool & Impure = isSymbolicLink(new("hello.txt")) |> Result.isOk

@test
def isSymbolicLink02(): Bool & Impure = isSymbolicLink(new("./doc/")) |> Result.isOk

@test
def isSymbolicLink03(): Bool & Impure = isSymbolicLink(new("./doc/foo")) |> Result.isOk

@test
def isWritable01(): Bool & Impure = isWritable(new("hello.txt")) |> Result.isOk

@test
def isWritable02(): Bool & Impure = isWritable(new("./doc/")) |> Result.isOk

@test
def isWritable03(): Bool & Impure = isWritable(new("./doc/foo")) |> Result.isOk

@test
def testReadLines01(): Bool & Impure = readLines(new("README.md")) |> Result.isOk

@test
def testCanonical01(): Bool & Impure = canonical(new("./doc/..")) == canonical(new("."))

@test
def testCanonical02(): Bool & Impure = canonical(new("./doc/foo/..")) == canonical(new("./doc"))

@test
def testCanonical03(): Bool & Impure = canonical(new("./doc/foo/bar/../bar")) == canonical(new("./doc/foo/bar"))

@test
def testNewFromFragments01(): Bool & Impure = newFromFragments(Nel(".", "doc" :: Nil)) == new("./doc/")

@test
def testNewFromFragments02(): Bool & Impure = newFromFragments(Nel(".",  "doc" :: ".." :: Nil)) == new("./doc/..")

@test
def testNewFromFragments03(): Bool & Impure = newFromFragments(Nel(".", Nil)) == new(".")

@test
def testResolve01(): Bool & Impure = resolve(new("./foo"), "bar") == new("./foo/bar")

@test
def testResolve02(): Bool & Impure = resolve(new("./foo/.."), "foo") == new("./foo/../foo")

@test
def testResolve03(): Bool & Impure = resolve(new("."), "foo") == new("./foo")
