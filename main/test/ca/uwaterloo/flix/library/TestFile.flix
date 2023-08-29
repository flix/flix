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

    ///
    /// Helper - open an existing file for reading, run `reader`, use `test`
    /// to check what has been read and close the file.
    ///
    def testReadFunction(path: String, reader: File -> Result[IOError, a] \IO, test: a -> Bool \ ef): Result[IOError, Bool] \ {ef, IO} =
        match File.open(path, ReadOnly) {
            case Ok(f)    => match reader(f) {
                case Ok(a)    => Functor.replace(File.close!(f), test(a))
                case Err(err) => Err(err)
            }
            case Err(err) => Err(err)
    }

    ///
    /// Helper - open an existing file for reading, run `reader`, use `test`
    /// to check what has been read and close the file.
    ///
    def testWriteFunctionWithReadBack(path: String, writer: File -> Result[IOError, b] \IO, reader: File -> Result[IOError, a] \IO, test: a -> Bool \ ef): Result[IOError, Bool] \ {ef, IO} =
        match File.open(path, ReadWrite) {
            case Ok(f)    => match writer(f) {
                case Ok(_)    => match (File.seek!(0i64, f) *> reader(f)) {
                    case Ok(a)    => Functor.replace(File.close!(f), test(a))
                    case Err(err) => Err(err)
                }
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
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                        File.length,
                        len -> len == 8i64)
            |> Result.getWithDefault(false)

    @test
    def length02(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    File.length,
                    len -> len == 1i64)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // seek!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seek01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> File.seek!(0i64, f) *> File.readChar!(f),
                    ch -> ch == 'A')
            |> Result.getWithDefault(false)

    @test
    def seek02(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> File.seek!(2i64, f) *> File.readChar!(f),
                    ch -> ch == 'B')
            |> Result.getWithDefault(false)

    @test
    def seek03(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> File.seek!(4i64, f) *> File.readChar!(f),
                    ch -> ch == 'C')
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // skipBytes!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def skipBytes01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> File.skipBytes!(0, f) *> File.readChar!(f),
                    ch -> ch == 'A')
            |> Result.getWithDefault(false)

    @test
    def skipBytes02(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> File.skipBytes!(2, f) *> File.readChar!(f),
                    ch -> ch == 'B')
            |> Result.getWithDefault(false)

    @test
    def skipBytes03(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> File.skipBytes!(4, f) *> File.readChar!(f),
                    ch -> ch == 'C')
            |> Result.getWithDefault(false)

    @test
    def skipBytes04(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> File.skipBytes!(-1, f) *> File.readChar!(f),
                    ch -> ch == 'A')
            |> Result.getWithDefault(false)


    /////////////////////////////////////////////////////////////////////////////
    // setLength!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def setLength01(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.setLength!(10i64),
                    File.length,
                    len -> len == 10i64)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // read!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def read01(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 1);
        let expected = Array#{-1i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    File.read!(arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    @test
    def read02(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 8);
        let expected = Array#{0i8, 65i8, 0i8, 66i8, 0i8, 67i8, 0i8, 68i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.read!(arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readWith01(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 6);
        let expected = Array#{0i8, 65i8, 0i8, 66i8, 0i8, 0i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.readWith!(destOffset = 0, length = 4, arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    @test
    def readWith02(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 6);
        let expected = Array#{0i8, 0i8, 0i8, 65i8, 0i8, 66i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.readWith!(destOffset = 2, length = 4, arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readFully!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readFully01(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 1);
        let expected = Array#{-1i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    File.readFully!(arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    @test
    def readFully02(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 8);
        let expected = Array#{0i8, 65i8, 0i8, 66i8, 0i8, 67i8, 0i8, 68i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.readFully!(arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readFullyWith!                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readFullyWith01(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 6);
        let expected = Array#{0i8, 65i8, 0i8, 66i8, 0i8, 0i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.readFullyWith!(destOffset = 0, length = 4, arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    @test
    def readFullyWith02(): Bool \ IO = region rc {
        let arr: Array[Int8, rc] = Array.new(rc, 6);
        let expected = Array#{0i8, 0i8, 0i8, 65i8, 0i8, 66i8} @ rc;
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.readFullyWith!(destOffset = 2, length = 4, arr),
                    _ -> Array.sameElements(arr, expected))
            |> Result.getWithDefault(false)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readByte!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readByte01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/1-1byte.bin",
                    File.readByte!,
                    i -> i == 1)
            |> Result.getWithDefault(false)

    @test
    def readByte02(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    File.readByte!,
                    i -> i == 255)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readBool!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readBool01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.readBool!,
                    b -> b == false)
            |> Result.getWithDefault(false)

    @test
    def readBool02(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    File.readBool!,
                    b -> b == true)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readChar!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readChar01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    File.readChar!,
                    ch -> ch == 'A')
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readInt8!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt801(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    File.readInt8!,
                    i -> i == -1i8)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readInt16!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt161(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-2bytes.bin",
                    File.readInt16!,
                    i -> i == 255i16)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readInt32!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt321(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-4bytes.bin",
                    File.readInt32!,
                    i -> i == 255)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readInt64!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readInt641(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-8bytes.bin",
                    File.readInt64!,
                    i -> i == 255i64)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readFloat32!                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readFloat321(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/1-float.bin",
                    File.readFloat32!,
                    d -> d == 1.0f32)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readFloat64!                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readFloat641(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/1-double.bin",
                    File.readFloat64!,
                    d -> d == 1.0f64)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readUnsignedByte!                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readUnsignedByte01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/1-1byte.bin",
                    File.readUnsignedByte!,
                    i -> i == 1)
            |> Result.getWithDefault(false)

    @test
    def readUnsignedByte02(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/255-1byte.bin",
                    File.readUnsignedByte!,
                    i -> i == 255)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readUnsignedShort!                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readUnsignedShort01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/modified-utf8.txt",
                    File.readUnsignedShort!,
                    i -> i == 12)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readLine!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readLine01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/multiline.txt",
                    File.readLine!,
                    s -> s == "Sample text for TestFile.flix")
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // readModifiedUTF!                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readModifiedUTF01(): Bool \ IO =
        testReadFunction("./main/test/ca/uwaterloo/flix/library/testdata/modified-utf8.txt",
                    File.readModifiedUTF!,
                    s -> s == "Hello World!")
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // write!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def write01(): Bool \ IO = region rc {
        let src = Array#{0i8, 1i8, 2i8, 3i8} @ rc;
        let dest = Array.new(rc, 4);
        let expected = Array#{0i8, 1i8, 2i8, 3i8} @ rc;
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.write!(src),
                    File.read!(dest),
                    _ -> Array.sameElements(dest, expected))
            |> Result.getWithDefault(false)
    }

    /////////////////////////////////////////////////////////////////////////////
    // writeWith!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeWith01(): Bool \ IO = region rc {
        let src = Array#{0i8, 1i8, 2i8, 3i8} @ rc;
        let dest = Array.new(rc, 2);
        let expected = Array#{2i8, 3i8} @ rc;
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeWith!(sourceOffset = 2, length = 2, src),
                    File.read!(dest),
                    _ -> Array.sameElements(dest, expected))
            |> Result.getWithDefault(false)
    }

    /////////////////////////////////////////////////////////////////////////////
    // writeBool!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeBool01(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeBool!(true),
                    File.readBool!,
                    b -> b == true)
            |> Result.getWithDefault(false)


    @test
    def writeBool02(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeBool!(false),
                    File.readBool!,
                    b -> b == false)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeAsChar!                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeAsChar01(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeAsChar!(65),
                    File.readChar!,
                    ch -> ch == 'A')
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeAsByte!                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeAsByte01(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeAsByte!(100),
                    File.readByte!,
                    i -> i == 100)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeAsShort!                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeAsShort01(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeAsShort!(100),
                    File.readUnsignedShort!,
                    i -> i == 100)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeInt32!                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeInt3201(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeInt32!(100),
                    File.readInt32!,
                    i -> i == 100)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeInt64!                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeInt6401(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeInt64!(100i64),
                    File.readInt64!,
                    i -> i == 100i64)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeFloat32!                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeFloat3201(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeFloat32!(100.0f32),
                    File.readFloat32!,
                    d -> d == 100.0f32)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeFloat64!                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeFloat6401(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeFloat64!(100.0f64),
                    File.readFloat64!,
                    d -> d == 100.0f64)
            |> Result.getWithDefault(false)

    /////////////////////////////////////////////////////////////////////////////
    // writeModifiedUTF!                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def writeModifiedUTF01(): Bool \ IO =
        testWriteFunctionWithReadBack("./main/test/ca/uwaterloo/flix/library/testdata/outfile01.bin",
                    File.writeModifiedUTF!("Hello World!"),
                    File.readModifiedUTF!,
                    s -> s == "Hello World!")
            |> Result.getWithDefault(false)

}
