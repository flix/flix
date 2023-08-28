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

mod TestFile {

    use File.Mode.{ReadOnly, ReadWrite}
    use Applicative.{*>}
    use IOError.Generic


    def withReadOnlyFile(path: String, fn: File -> Result[IOError, a] \IO): Result[IOError, a] \ IO =
        match File.open(path, ReadOnly) {
            case Ok(f)    => match fn(f) {
                case Ok(ans)  => Functor.replace(File.close!(f), ans)
                case Err(err) => Err(err)
            }
            case Err(err) => Err(err)
    }

    /////////////////////////////////////////////////////////////////////////////
    // open / close!                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def openClose01(): Bool \ IO =
        match File.open("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin", ReadOnly) {
            case Ok(f) => match File.close!(f) {
                case Ok() => true
                case _    => false
            }
            case _     => false
        }

    @test
    def openClose02(): Bool \ IO =
        match File.open("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin", ReadOnly) {
            case Ok(f) => match File.close!(f) {
                case Ok() => true
                case _    => false
            }
            case _     => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> Result.flatMap(len -> if (len == 8i64) Ok() else Err(Generic("length error")), File.length(f)))
            |> Result.isOk

    @test
    def length02(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    f -> Result.flatMap(len -> if (len == 1i64) Ok() else Err(Generic("length error")), File.length(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // seek!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seek01(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> Result.flatMap(ch -> if (ch == 'A') Ok() else Err(Generic("seek! error")), File.seek!(0i64, f) *> File.readChar!(f)))
            |> Result.isOk

    @test
    def seek02(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> Result.flatMap(ch -> if (ch == 'B') Ok() else Err(Generic("seek! error")), File.seek!(2i64, f) *> File.readChar!(f)))
            |> Result.isOk

    @test
    def seek03(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> Result.flatMap(ch -> if (ch == 'C') Ok() else Err(Generic("seek! error")), File.seek!(4i64, f) *> File.readChar!(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readBool!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readBool01(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> Result.flatMap(b -> if (b == false) Ok() else Err(Generic("readBool! error")), File.readBool!(f)))
            |> Result.isOk

    @test
    def readBool02(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    f -> Result.flatMap(b -> if (b == true) Ok() else Err(Generic("readBool! error")), File.readBool!(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readChar!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readChar01(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> Result.flatMap(ch -> if (ch == 'A') Ok() else Err(Generic("readChar! error")), File.readChar!(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readInt8!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt801(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    f -> Result.flatMap(i -> if (i == -1i8) Ok() else Err(Generic("readInt8! error")), File.readInt8!(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readInt16!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt161(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/255-2bytes.bin",
                    f -> Result.flatMap(i -> if (i == 255i16) Ok() else Err(Generic("readInt16! error")), File.readInt16!(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readInt32!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt321(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/255-4bytes.bin",
                    f -> Result.flatMap(i -> if (i == 255) Ok() else Err(Generic("readInt32! error")), File.readInt32!(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readInt64!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt641(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/255-8bytes.bin",
                    f -> Result.flatMap(i -> if (i == 255i64) Ok() else Err(Generic("readInt64! error")), File.readInt64!(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readLine!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readLine01(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/multiline.txt",
                    f -> Result.flatMap(s -> if (s == "Sample text for TestFile.flix") Ok() else Err(Generic("readLine! error")), File.readLine!(f)))
            |> Result.isOk
}
