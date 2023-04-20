/*
 *  Copyright 2021 Nina Andrup Pedersen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

mod TestSemiGroup {

    use SemiGroup.{combine, combineN}

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineUnit01(): Bool = combine((), ()) == ()

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineInt801(): Bool = combine(0i8, 0i8) == 0i8

    @test
    def combineInt802(): Bool = combine(1i8, 0i8) == 1i8

    @test
    def combineInt803(): Bool = combine(0i8, 2i8) == 2i8

    @test
    def combineInt804(): Bool = combine(1i8, 2i8) == 3i8

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineInt1601(): Bool = combine(0i16, 0i16) == 0i16

    @test
    def combineInt1602(): Bool = combine(1i16, 0i16) == 1i16

    @test
    def combineInt1603(): Bool = combine(0i16, 2i16) == 2i16

    @test
    def combineInt1604(): Bool = combine(1i16, 2i16) == 3i16

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineInt3201(): Bool = combine(0i32, 0i32) == 0i32

    @test
    def combineInt3202(): Bool = combine(1i32, 0i32) == 1i32

    @test
    def combineInt3203(): Bool = combine(0i32, 2i32) == 2i32

    @test
    def combineInt3204(): Bool = combine(1i32, 2i32) == 3i32

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineInt6401(): Bool = combine(0i64, 0i64) == 0i64

    @test
    def combineInt6402(): Bool = combine(1i64, 0i64) == 1i64

    @test
    def combineInt6403(): Bool = combine(0i64, 2i64) == 2i64

    @test
    def combineInt6404(): Bool = combine(1i64, 2i64) == 3i64

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineBigInt01(): Bool = combine(0ii, 0ii) == 0ii

    @test
    def combineBigInt02(): Bool = combine(1ii, 0ii) == 1ii

    @test
    def combineBigInt03(): Bool = combine(0ii, 2ii) == 2ii

    @test
    def combineBigInt04(): Bool = combine(1ii, 2ii) == 3ii

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineFloat3201(): Bool = combine(0.0f32, 0.0f32) == 0.0f32

    @test
    def combineFloat3202(): Bool = combine(1.0f32, 0.0f32) == 1.0f32

    @test
    def combineFloat3203(): Bool = combine(0.0f32, 2.0f32) == 2.0f32

    @test
    def combineFloat3204(): Bool = combine(1.0f32, 2.0f32) == 3.0f32

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineFloat6401(): Bool = combine(0.0f64, 0.0f64) == 0.0f64

    @test
    def combineFloat6402(): Bool = combine(1.0f64, 0.0f64) == 1.0f64

    @test
    def combineFloat6403(): Bool = combine(0.0f64, 2.0f64) == 2.0f64

    @test
    def combineFloat6404(): Bool = combine(1.0f64, 2.0f64) == 3.0f64

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineBigDecimal01(): Bool = combine(0.0ff, 0.0ff) == 0.0ff

    @test
    def combineBigDecimal02(): Bool = combine(1.0ff, 0.0ff) == 1.0ff

    @test
    def combineBigDecimal03(): Bool = combine(0.0ff, 2.0ff) == 2.0ff

    @test
    def combineBigDecimal4(): Bool = combine(1.0ff, 2.0ff) == 3.0ff

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineString01(): Bool = combine("", "") == ""

    @test
    def combineString02(): Bool = combine("a", "") == "a"

    @test
    def combineString03(): Bool = combine("", "b") == "b"

    @test
    def combineString04(): Bool = combine("a", "b") == "ab"

    /////////////////////////////////////////////////////////////////////////////
    // Option                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineOption01(): Bool = combine(None, None) == None: Option[String]

    @test
    def combineOption02(): Bool = combine(Some("a"), None) == Some("a")

    @test
    def combineOption03(): Bool = combine(None, Some("b")) == Some("b")

    @test
    def combineOption04(): Bool = combine(Some("a"), Some("b")) == Some("ab")

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineList01(): Bool = combine(Nil, Nil) == Nil: List[Int32]

    @test
    def combineList02(): Bool = combine(1 :: Nil, Nil) == 1 :: Nil

    @test
    def combineList03(): Bool = combine(Nil, 2 :: Nil) == 2 :: Nil

    @test
    def combineList04(): Bool = combine(1 :: Nil, 2 :: Nil) == 1 :: 2 :: Nil

    @test
    def combineList05(): Bool = combine(1 :: 2 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineMap01(): Bool = combine(Map#{}, Map#{}) == Map#{}: Map[Int32, String]

    @test
    def combineMap02(): Bool = combine(Map#{1 => "A"}, Map#{}) == Map#{1 => "A"}

    @test
    def combineMap03(): Bool = combine(Map#{}, Map#{1 => "a"}) == Map#{1 => "a"}

    @test
    def combineMap04(): Bool = combine(Map#{1 => "A"}, Map#{1 => "a"}) == Map#{1 => "Aa"}

    @test
    def combineMap05(): Bool = combine(Map#{1 => "A", 2 => "B", 3 => "C"}, Map#{2 => "b", 3 => "c", 4 => "d"}) == Map#{1 => "A", 2 => "Bb", 3 => "Cc", 4 => "d"}

    /////////////////////////////////////////////////////////////////////////////
    // Validation                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineValidation01(): Bool = combine(Validation.Failure(Nec.singleton("e1")), Validation.Failure(Nec.singleton("e2"))): Validation[String, String] == Validation.Failure(Nec.append(Nec.singleton("e1"), Nec.singleton("e2")))

    @test
    def combineValidation02(): Bool = combine(Validation.Success("a"), Validation.Failure(Nec.singleton("e1"))): Validation[String, String] == Validation.Failure(Nec.singleton("e1"))

    @test
    def combineValidation03(): Bool = combine(Validation.Failure(Nec.singleton("e1")), Validation.Success("1")): Validation[String, String] == Validation.Failure(Nec.singleton("e1"))

    @test
    def combineValidation04(): Bool = combine(Validation.Success("a"), Validation.Success("1")): Validation[String, String] == Validation.Success("a1")

    /////////////////////////////////////////////////////////////////////////////
    // 2-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine2Tuple01(): Bool = combine(("", ""), ("", "")) == ("", "")

    @test
    def combine2Tuple02(): Bool = combine(("a", "b"), ("", "")) == ("a", "b")

    @test
    def combine2Tuple03(): Bool = combine(("", ""), ("1", "2")) == ("1", "2")

    @test
    def combine2Tuple04(): Bool = combine(("a", "b"), ("1", "2")) == ("a1", "b2")

    @test
    def combine2Tuple05(): Bool = combine(("a", ()), ("1", ())) == ("a1", ())

    /////////////////////////////////////////////////////////////////////////////
    // 3-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine3Tuple01(): Bool = combine(("", "", ""), ("", "", "")) == ("", "", "")

    @test
    def combine3Tuple02(): Bool = combine(("a", "b", "c"), ("", "", "")) == ("a", "b", "c")

    @test
    def combine3Tuple03(): Bool = combine(("", "", ""), ("1", "2", "3")) == ("1", "2", "3")

    @test
    def combine3Tuple04(): Bool = combine(("a", "b", "c"), ("1", "2", "3")) == ("a1", "b2", "c3")

    @test
    def combine3Tuple05(): Bool = combine(("a", (), "b"), ("1", (), "2")) == ("a1", (), "b2")

    /////////////////////////////////////////////////////////////////////////////
    // 4-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine4Tuple01(): Bool = combine(("", "", "", ""), ("", "", "", "")) == ("", "", "", "")

    @test
    def combine4Tuple02(): Bool = combine(("a", "b", "c", "d"), ("", "", "", "")) == ("a", "b", "c", "d")

    @test
    def combine4Tuple03(): Bool = combine(("", "", "", ""), ("1", "2", "3", "4")) == ("1", "2", "3", "4")

    @test
    def combine4Tuple04(): Bool = combine(("a", "b", "c", "d"), ("1", "2", "3", "4")) == ("a1", "b2", "c3", "d4")

    @test
    def combine4Tuple05(): Bool = combine(("a", (), "b", ()), ("1", (), "2", ())) == ("a1", (), "b2", ())

    /////////////////////////////////////////////////////////////////////////////
    // 5-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine5Tuple01(): Bool = combine(("", "", "", "", ""), ("", "", "", "", "")) == ("", "", "", "", "")

    @test
    def combine5Tuple02(): Bool = combine(("a", "b", "c", "d", "e"), ("", "", "", "", "")) == ("a", "b", "c", "d", "e")

    @test
    def combine5Tuple03(): Bool = combine(("", "", "", "", ""), ("1", "2", "3", "4", "5")) == ("1", "2", "3", "4", "5")

    @test
    def combine5Tuple04(): Bool = combine(("a", "b", "c", "d", "e"), ("1", "2", "3", "4", "5")) == ("a1", "b2", "c3", "d4", "e5")

    @test
    def combine5Tuple05(): Bool = combine(("a", (), "b", (), "c"), ("1", (), "2", (), "3")) == ("a1", (), "b2", (), "c3")

    /////////////////////////////////////////////////////////////////////////////
    // 6-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine6Tuple01(): Bool = combine(("", "", "", "", "", ""), ("", "", "", "", "", "")) == ("", "", "", "", "", "")

    @test
    def combine6Tuple02(): Bool = combine(("a", "b", "c", "d", "e", "f"), ("", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f")

    @test
    def combine6Tuple03(): Bool = combine(("", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6")) == ("1", "2", "3", "4", "5", "6")

    @test
    def combine6Tuple04(): Bool = combine(("a", "b", "c", "d", "e", "f"), ("1", "2", "3", "4", "5", "6")) == ("a1", "b2", "c3", "d4", "e5", "f6")

    @test
    def combine6Tuple05(): Bool = combine(("a", (), "b", (), "c", ()), ("1", (), "2", (), "3", ())) == ("a1", (), "b2", (), "c3", ())

    /////////////////////////////////////////////////////////////////////////////
    // 7-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine7Tuple01(): Bool = combine(("", "", "", "", "", "", ""), ("", "", "", "", "", "", "")) == ("", "", "", "", "", "", "")

    @test
    def combine7Tuple02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g"), ("", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g")

    @test
    def combine7Tuple03(): Bool = combine(("", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7")) == ("1", "2", "3", "4", "5", "6", "7")

    @test
    def combine7Tuple04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g"), ("1", "2", "3", "4", "5", "6", "7")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7")

    @test
    def combine7Tuple05(): Bool = combine(("a", (), "b", (), "c", (), "d"), ("1", (), "2", (), "3", (), "4")) == ("a1", (), "b2", (), "c3", (), "d4")

    /////////////////////////////////////////////////////////////////////////////
    // 8-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine8Tuple01(): Bool = combine(("", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "")) == ("", "", "", "", "", "", "", "")

    @test
    def combine8Tuple02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("", "", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g", "h")

    @test
    def combine8Tuple03(): Bool = combine(("", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8")) == ("1", "2", "3", "4", "5", "6", "7", "8")

    @test
    def combine8Tuple04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("1", "2", "3", "4", "5", "6", "7", "8")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8")

    @test
    def combine8Tuple05(): Bool = combine(("a", (), "b", (), "c", (), "d", ()), ("1", (), "2", (), "3", (), "4", ())) == ("a1", (), "b2", (), "c3", (), "d4", ())

    /////////////////////////////////////////////////////////////////////////////
    // 9-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine9Tuple01(): Bool = combine(("", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "")) == ("", "", "", "", "", "", "", "", "")

    @test
    def combine9Tuple02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("", "", "", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g", "h", "i")

    @test
    def combine9Tuple03(): Bool = combine(("", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9")) == ("1", "2", "3", "4", "5", "6", "7", "8", "9")

    @test
    def combine9Tuple04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("1", "2", "3", "4", "5", "6", "7", "8", "9")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9")

    @test
    def combine9Tuple05(): Bool = combine(("a", (), "b", (), "c", (), "d", (), "e"), ("1", (), "2", (), "3", (), "4", (), "5")) == ("a1", (), "b2", (), "c3", (), "d4", (), "e5")

    /////////////////////////////////////////////////////////////////////////////
    // 10-Tuple                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combine10Tuple01(): Bool = combine(("", "", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "", "")) == ("", "", "", "", "", "", "", "", "", "")

    @test
    def combine10Tuple02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("", "", "", "", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")

    @test
    def combine10Tuple03(): Bool = combine(("", "", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")) == ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")

    @test
    def combine10Tuple04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9", "j0")

    @test
    def combine10Tuple05(): Bool = combine(("a", (), "b", (), "c", (), "d", (), "e", ()), ("1", (), "2", (), "3", (), "4", (), "5", ())) == ("a1", (), "b2", (), "c3", (), "d4", (), "e5", ())

    /////////////////////////////////////////////////////////////////////////////
    // combineN                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineN01(): Bool = combineN((), 0) == ()

    @test
    def combineN02(): Bool = combineN((), 3) == ()

    @test
    def combineN03(): Bool = combineN("", -1) == ""

    @test
    def combineN04(): Bool = combineN("", 5) == ""

    @test
    def combineN05(): Bool = combineN("a", 3) == "aaa"

    @test
    def combineN06(): Bool = combineN(Some("ab"), 5) == Some("ababababab")

    @test
    def combineN07(): Bool = combineN(1 :: Nil, 2) == 1 :: 1 :: Nil

    @test
    def combineN08(): Bool = combineN(Map#{1 => "a"}, 4) == Map#{1 => "aaaa"}

    /////////////////////////////////////////////////////////////////////////////
    // associative law                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def associativeTest01(): Bool = combine(combine("a", "b"), "c") == combine("a", combine("b", "c"))

    /////////////////////////////////////////////////////////////////////////////
    // Syntax                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def operator01(): Bool = (1 ++ 1) == 2

    @test
    def operator02(): Bool = ("abc" ++ "def") == "abcdef"

    @test
    def operator03(): Bool = (1 :: 2 :: Nil ++ 3 :: 4 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

}
