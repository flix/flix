/*
 * Copyright 2019 Stephen Tetley
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

mod TestStringBuilder {

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): StringBuilder[Static] \ IO =
        StringBuilder.empty(Static) // Must be Impure, otherwise the StringBuilder would escape its region


    /////////////////////////////////////////////////////////////////////////////
    // appendString!                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def appendString01!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString!("hello", sb);
        StringBuilder.toString(sb) == "hello"
    }

    @test
    def appendString02!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString!("hello", sb);
        StringBuilder.appendString!("!", sb);
        StringBuilder.toString(sb) == "hello!"
    }

    @test
    def appendString03!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString!("hello", sb);
        StringBuilder.appendString!(" ", sb);
        StringBuilder.appendString!("world!", sb);
        StringBuilder.toString(sb) == "hello world!"
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendCodePoint!                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def appendCodePoint01!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendCodePoint!(0x1f3dd, sb);
        StringBuilder.toString(sb) == CodePoint.toString(0x1f3dd)
    }

    @test
    def appendCodePoint02!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendCodePoint!(0x1f3dd, sb);
        StringBuilder.appendCodePoint!(0x0020, sb);
        StringBuilder.toString(sb) == "\uD83C\uDFDD "
    }

    @test
    def appendCodePoint03!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendCodePoint!(0x0020, sb);
        StringBuilder.appendCodePoint!(0x1f3dd, sb);
        StringBuilder.appendCodePoint!(0x0020, sb);
        StringBuilder.toString(sb) == " \uD83C\uDFDD "
    }


    /////////////////////////////////////////////////////////////////////////////
    // append! (Char)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def appendChar01!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append!('A', sb);
        StringBuilder.toString(sb) == "A"
    }

    @test
    def appendChar02!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append!('A', sb);
        StringBuilder.append!('B', sb);
        StringBuilder.toString(sb) == "AB"
    }

    @test
    def appendChar03!(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append!('A', sb);
        StringBuilder.append!('B', sb);
        StringBuilder.append!('C', sb);
        StringBuilder.toString(sb) == "ABC"
    }


/////////////////////////////////////////////////////////////////////////////
// append! (Int8)                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def appendInt801!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(0i8, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt802!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(1i8, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt803!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(-1i8, sb);
    StringBuilder.toString(sb) == "-1"
}

@test
def appendInt804!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(127i8, sb);
    StringBuilder.toString(sb) == "127"
}

@test
def appendInt805!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(-127i8, sb);
    StringBuilder.toString(sb) == "-127"
}

/////////////////////////////////////////////////////////////////////////////
// append! (Int16)                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendInt1601!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(0i16, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt1602!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(1i16, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt1603!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(-1i16, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// append! (Int32)                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendInt3201!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(0, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt3202!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(1, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt3203!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(-1, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// append! (Int64)                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendInt6401!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(0i64, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt6402!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(1i64, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt6403!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(-1i64, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// append! (BigInt)                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def appendBigInt01!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(0ii, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendBigInt02!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(1ii, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendBigInt03!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(-1ii, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// appendFloat32!                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def appendFloat3201!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(0.0f32, sb);
    StringBuilder.toString(sb) == "0.0"
}

@test
def appendFloat3202!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(Float32.nan(), sb);
    StringBuilder.toString(sb) == "NaN"
}

@test
def appendFloat3203!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(Float32.positiveInfinity(), sb);
    StringBuilder.toString(sb) == "Infinity"
}

@test
def appendFloat3204!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(Float32.negativeInfinity(), sb);
    StringBuilder.toString(sb) == "-Infinity"
}

/////////////////////////////////////////////////////////////////////////////
// appendFloat64!                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def appendFloat6401!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(0.0f64, sb);
    StringBuilder.toString(sb) == "0.0"
}

@test
def appendFloat6402!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(Float64.nan(), sb);
    StringBuilder.toString(sb) == "NaN"
}

@test
def appendFloat6403!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(Float64.positiveInfinity(), sb);
    StringBuilder.toString(sb) == "Infinity"
}

@test
def appendFloat6404!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!(Float64.negativeInfinity(), sb);
    StringBuilder.toString(sb) == "-Infinity"
}

/////////////////////////////////////////////////////////////////////////////
// appendLineSeparator!                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLineSeparator01!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLineSeparator!(sb);
    StringBuilder.toString(sb) == String.lineSeparator()
}

@test
def appendLineSeparator02!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!('A', sb);
    StringBuilder.appendLineSeparator!(sb);
    StringBuilder.append!('B', sb);
    let s1 = "A" + String.lineSeparator() + "B";
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// appendLine!                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLine01!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLine!("", sb);
    StringBuilder.toString(sb) == String.lineSeparator()
}

@test
def appendLine02!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLine!("A", sb);
    let s1 = "A" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLine03!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLine!("A", sb);
    StringBuilder.appendLine!("B", sb);
    let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLine04!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLine!("A", sb);
    StringBuilder.appendLine!("B", sb);
    StringBuilder.appendLine!("C", sb);
    let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator() + "C" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// appendLineWith!                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLineWith01!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLineWith!(Int32.toString, 1, sb);
    let s1 = "1" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLineWith02!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLineWith!(Int32.toString, 1, sb);
    StringBuilder.appendLineWith!(Int32.toString, 2, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLineWith03!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLineWith!(Int32.toString, 1, sb);
    StringBuilder.appendLineWith!(Int32.toString, 2, sb);
    StringBuilder.appendLineWith!(Int32.toString, 3, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLineWith04!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLineWith!(Int32.toString, 1, sb);
    StringBuilder.appendLineWith!(Int32.toString, 2, sb);
    StringBuilder.appendLineWith!(Int32.toString, 3, sb);
    StringBuilder.appendLineWith!(Int32.toString, 4, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator() + "4" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// appendLines                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLines01!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLines!(Array#{} @ rc, sb);
    StringBuilder.toString(sb) == ""
}

@test
def appendLines02!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLines!(Array#{"A"} @ rc, sb);
    let s1 = "A" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLines03!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLines!(Array#{"A", "B"} @ rc, sb);
    let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLines04!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLines!(Array#{"A", "B", "C"} @ rc, sb);
    let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator() + "C" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// appendLinesWith                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLinesWith01(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith(Int32.toString, Nil, sb);
    StringBuilder.toString(sb) == ""
}

@test
def appendLinesWith02(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith(Int32.toString, Set#{1}, sb);
    let s1 = "1" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith03(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith(Int32.toString, List.toChain(1 :: 2 :: Nil), sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith04(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith(Int32.toString, 1 :: 2 :: 3 :: Nil, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// appendLinesWith!                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLinesWith01!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith!(Int32.toString, ((Array#{} @ rc): Array[Int32, _]), sb);
    StringBuilder.toString(sb) == ""
}

@test
def appendLinesWith02!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith!(Int32.toString, Array#{1} @ rc, sb);
    let s1 = "1" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith03!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith!(Int32.toString, Array#{1, 2} @ rc, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith04!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendLinesWith!(Int32.toString, Array#{1, 2, 3} @ rc, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// intercalate!                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def intercalate01!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.intercalate!("", Array#{} @ rc, sb);
    StringBuilder.toString(sb) == ""
}

@test
def intercalate02!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.intercalate!("::", Array#{} @ rc, sb);
    StringBuilder.toString(sb) == ""
}

@test
def intercalate03!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.intercalate!("", Array#{"abc", "def"} @ rc, sb);
    StringBuilder.toString(sb) == "abcdef"
}

@test
def intercalate04!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.intercalate!("::", Array#{"abc"} @ rc, sb);
    StringBuilder.toString(sb) == "abc"
}

@test
def intercalate05!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.intercalate!("::", Array#{"abc", "def"} @ rc, sb);
    StringBuilder.toString(sb) == "abc::def"
}

@test
def intercalate06!(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.intercalate!("::", Array#{"abc", "def", "ghi"} @ rc, sb);
    StringBuilder.toString(sb) == "abc::def::ghi"
}


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.iterator(rc, sb) |> Iterator.toList == Nil
    }

    @test
    def iterator02(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString!("abc", sb);
        StringBuilder.iterator(rc, sb) |> Iterator.toList == 'a' :: 'b' :: 'c' :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def enumerator01(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.enumerator(rc, sb) |> Iterator.toList == Nil
    }

    @test
    def enumerator02(): Bool = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString!("abc", sb);
        StringBuilder.enumerator(rc, sb) |> Iterator.toList == (0, 'a') :: (1, 'b') :: (2, 'c') :: Nil
    }


/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def length01(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.length(sb) == 0
}

@test
def length02(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!('A', sb);
    StringBuilder.length(sb) == 1
}

@test
def length03(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!('A', sb);
    StringBuilder.append!('B', sb);
    StringBuilder.length(sb) == 2
}

@test
def length04(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendString!("A", sb);
    StringBuilder.appendString!("B", sb);
    StringBuilder.length(sb) == 2
}

@test
def length05(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendString!("Hello", sb);
    StringBuilder.append!(' ', sb);
    StringBuilder.appendString!("World", sb);
    StringBuilder.append!('!', sb);
    StringBuilder.length(sb) == 12
}

/////////////////////////////////////////////////////////////////////////////
// size                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def size01(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.size(sb) == 0
}

@test
def size02(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!('A', sb);
    StringBuilder.size(sb) == 1
}

@test
def size03(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!('A', sb);
    StringBuilder.append!('B', sb);
    StringBuilder.size(sb) == 2
}

@test
def size04(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendString!("A", sb);
    StringBuilder.appendString!("B", sb);
    StringBuilder.size(sb) == 2
}

@test
def size05(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendString!("Hello", sb);
    StringBuilder.append!(' ', sb);
    StringBuilder.appendString!("World", sb);
    StringBuilder.append!('!', sb);
    StringBuilder.size(sb) == 12
}


/////////////////////////////////////////////////////////////////////////////
// setLength                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def setLength!01(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!("1234567890", sb);
    StringBuilder.setLength!(4, sb);
    Assert.eq("1234", StringBuilder.toString(sb))
}

@test
def setLength!02(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.append!("ABC", sb);
    StringBuilder.setLength!(0, sb);
    Assert.eq("", StringBuilder.toString(sb))
}

/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toString01(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.toString(sb) == ""
}

@test
def toString02(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendString!("", sb);
    StringBuilder.toString(sb) == ""
}

@test
def toString03(): Bool = region rc {
    let sb = StringBuilder.empty(rc);
    StringBuilder.appendString!("abcdefghijklmnopqrstuvwxyz", sb);
    StringBuilder.toString(sb) == "abcdefghijklmnopqrstuvwxyz"
}

}
