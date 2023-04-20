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
    def new01(): StringBuilder[Static] \ IO =
        StringBuilder.new(Static) // Must be Impure, otherwise the StringBuilder would escape its region


    /////////////////////////////////////////////////////////////////////////////
    // appendString!                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def appendString01!(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.appendString!("hello", sb);
        StringBuilder.toString(sb) == "hello"
    }

    @test
    def appendString02!(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.appendString!("hello", sb);
        StringBuilder.appendString!("!", sb);
        StringBuilder.toString(sb) == "hello!"
    }

    @test
    def appendString03!(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.appendString!("hello", sb);
        StringBuilder.appendString!(" ", sb);
        StringBuilder.appendString!("world!", sb);
        StringBuilder.toString(sb) == "hello world!"
    }


    /////////////////////////////////////////////////////////////////////////////
    // append! (Char)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def appendChar01!(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.append!('A', sb);
        StringBuilder.toString(sb) == "A"
    }

    @test
    def appendChar02!(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.append!('A', sb);
        StringBuilder.append!('B', sb);
        StringBuilder.toString(sb) == "AB"
    }

    @test
    def appendChar03!(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.append!('A', sb);
        StringBuilder.append!('B', sb);
        StringBuilder.append!('C', sb);
        StringBuilder.toString(sb) == "ABC"
    }


/////////////////////////////////////////////////////////////////////////////
// append! (Int8)                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def appendInt801!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(0i8, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt802!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(1i8, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt803!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(-1i8, sb);
    StringBuilder.toString(sb) == "-1"
}

@test
def appendInt804!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(127i8, sb);
    StringBuilder.toString(sb) == "127"
}

@test
def appendInt805!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(-127i8, sb);
    StringBuilder.toString(sb) == "-127"
}

/////////////////////////////////////////////////////////////////////////////
// append! (Int16)                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendInt1601!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(0i16, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt1602!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(1i16, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt1603!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(-1i16, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// append! (Int32)                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendInt3201!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(0, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt3202!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(1, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt3203!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(-1, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// append! (Int64)                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendInt6401!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(0i64, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendInt6402!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(1i64, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendInt6403!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(-1i64, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// append! (BigInt)                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def appendBigInt01!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(0ii, sb);
    StringBuilder.toString(sb) == "0"
}

@test
def appendBigInt02!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(1ii, sb);
    StringBuilder.toString(sb) == "1"
}

@test
def appendBigInt03!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(-1ii, sb);
    StringBuilder.toString(sb) == "-1"
}

/////////////////////////////////////////////////////////////////////////////
// appendFloat32!                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def appendFloat3201!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(0.0f32, sb);
    StringBuilder.toString(sb) == "0.0"
}

@test
def appendFloat3202!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(Float32.nan(), sb);
    StringBuilder.toString(sb) == "NaN"
}

@test
def appendFloat3203!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(Float32.positiveInfinity(), sb);
    StringBuilder.toString(sb) == "Infinity"
}

@test
def appendFloat3204!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(Float32.negativeInfinity(), sb);
    StringBuilder.toString(sb) == "-Infinity"
}

/////////////////////////////////////////////////////////////////////////////
// appendFloat64!                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def appendFloat6401!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(0.0f64, sb);
    StringBuilder.toString(sb) == "0.0"
}

@test
def appendFloat6402!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(Float64.nan(), sb);
    StringBuilder.toString(sb) == "NaN"
}

@test
def appendFloat6403!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(Float64.positiveInfinity(), sb);
    StringBuilder.toString(sb) == "Infinity"
}

@test
def appendFloat6404!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!(Float64.negativeInfinity(), sb);
    StringBuilder.toString(sb) == "-Infinity"
}

/////////////////////////////////////////////////////////////////////////////
// appendLineSeparator!                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLineSeparator01!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLineSeparator!(sb);
    StringBuilder.toString(sb) == String.lineSeparator()
}

@test
def appendLineSeparator02!(): Bool = region r {
    let sb = StringBuilder.new(r);
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
def appendLine01!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLine!("", sb);
    StringBuilder.toString(sb) == String.lineSeparator()
}

@test
def appendLine02!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLine!("A", sb);
    let s1 = "A" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLine03!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLine!("A", sb);
    StringBuilder.appendLine!("B", sb);
    let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLine04!(): Bool = region r {
    let sb = StringBuilder.new(r);
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
def appendLineWith01!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLineWith!(Int32.toString, 1, sb);
    let s1 = "1" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLineWith02!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLineWith!(Int32.toString, 1, sb);
    StringBuilder.appendLineWith!(Int32.toString, 2, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLineWith03!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLineWith!(Int32.toString, 1, sb);
    StringBuilder.appendLineWith!(Int32.toString, 2, sb);
    StringBuilder.appendLineWith!(Int32.toString, 3, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLineWith04!(): Bool = region r {
    let sb = StringBuilder.new(r);
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
def appendLines01!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLines!(Array#{} @ r, sb);
    StringBuilder.toString(sb) == ""
}

@test
def appendLines02!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLines!(Array#{"A"} @ r, sb);
    let s1 = "A" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLines03!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLines!(Array#{"A", "B"} @ r, sb);
    let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLines04!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLines!(Array#{"A", "B", "C"} @ r, sb);
    let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator() + "C" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// appendLinesWith                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLinesWith01(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith(Int32.toString, Nil, sb);
    StringBuilder.toString(sb) == ""
}

@test
def appendLinesWith02(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith(Int32.toString, Set#{1}, sb);
    let s1 = "1" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith03(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith(Int32.toString, List.toChain(1 :: 2 :: Nil), sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith04(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith(Int32.toString, 1 :: 2 :: 3 :: Nil, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// appendLinesWith!                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def appendLinesWith01!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith!(Int32.toString, (Array#{} @ r): Array[Int32, _], sb);
    StringBuilder.toString(sb) == ""
}

@test
def appendLinesWith02!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith!(Int32.toString, Array#{1} @ r, sb);
    let s1 = "1" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith03!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith!(Int32.toString, Array#{1, 2} @ r, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

@test
def appendLinesWith04!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendLinesWith!(Int32.toString, Array#{1, 2, 3} @ r, sb);
    let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
    StringBuilder.toString(sb) == s1
}

/////////////////////////////////////////////////////////////////////////////
// intercalate!                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def intercalate01!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.intercalate!("", Array#{} @ r, sb);
    StringBuilder.toString(sb) == ""
}

@test
def intercalate02!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.intercalate!("::", Array#{} @ r, sb);
    StringBuilder.toString(sb) == ""
}

@test
def intercalate03!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.intercalate!("", Array#{"abc", "def"} @ r, sb);
    StringBuilder.toString(sb) == "abcdef"
}

@test
def intercalate04!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.intercalate!("::", Array#{"abc"} @ r, sb);
    StringBuilder.toString(sb) == "abc"
}

@test
def intercalate05!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.intercalate!("::", Array#{"abc", "def"} @ r, sb);
    StringBuilder.toString(sb) == "abc::def"
}

@test
def intercalate06!(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.intercalate!("::", Array#{"abc", "def", "ghi"} @ r, sb);
    StringBuilder.toString(sb) == "abc::def::ghi"
}


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.iterator(r, sb) |> Iterator.toList == Nil
    }

    @test
    def iterator02(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.appendString!("abc", sb);
        StringBuilder.iterator(r, sb) |> Iterator.toList == 'a' :: 'b' :: 'c' :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def enumerator01(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.enumerator(r, sb) |> Iterator.toList == Nil
    }

    @test
    def enumerator02(): Bool = region r {
        let sb = StringBuilder.new(r);
        StringBuilder.appendString!("abc", sb);
        StringBuilder.enumerator(r, sb) |> Iterator.toList == (0, 'a') :: (1, 'b') :: (2, 'c') :: Nil
    }


/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def length01(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.length(sb) == 0
}

@test
def length02(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!('A', sb);
    StringBuilder.length(sb) == 1
}

@test
def length03(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.append!('A', sb);
    StringBuilder.append!('B', sb);
    StringBuilder.length(sb) == 2
}

@test
def length04(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendString!("A", sb);
    StringBuilder.appendString!("B", sb);
    StringBuilder.length(sb) == 2
}

@test
def length05(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendString!("Hello", sb);
    StringBuilder.append!(' ', sb);
    StringBuilder.appendString!("World", sb);
    StringBuilder.append!('!', sb);
    StringBuilder.length(sb) == 12
}

/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toString01(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.toString(sb) == ""
}

@test
def toString02(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendString!("", sb);
    StringBuilder.toString(sb) == ""
}

@test
def toString03(): Bool = region r {
    let sb = StringBuilder.new(r);
    StringBuilder.appendString!("abcdefghijklmnopqrstuvwxyz", sb);
    StringBuilder.toString(sb) == "abcdefghijklmnopqrstuvwxyz"
}

}
