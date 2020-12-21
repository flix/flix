/*
 *  Copyright 2020 Stephen Tetley
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

namespace TestMonoid {

    use Monoid.{empty, combine};
    use Monoid.{combineAll};

    /////////////////////////////////////////////////////////////////////////////
    // combineAll                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineAll01(): Bool = combineAll(Nil: List[Unit]) == ()

    @test
    def combineAll02(): Bool = combineAll(() :: Nil) == ()

    @test
    def combineAll03(): Bool = combineAll(Nil: List[String]) == ""

    @test
    def combineAll04(): Bool = combineAll("a" :: Nil) == "a"

    @test
    def combineAll05(): Bool = combineAll("a" :: "b" :: Nil) == "ab"

    @test
    def combineAll06(): Bool = combineAll("a" :: "b" :: "cdefg" :: Nil) == "abcdefg"

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unitEmpty01(): Bool = empty() == ()

    @test
    def unitCombine01(): Bool = combine((), ()) == ()

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stringEmpty01(): Bool = empty() == ""

    @test
    def stringCombine01(): Bool = combine("", "") == ""

    @test
    def stringCombine02(): Bool = combine("a", "") == "a"

    @test
    def stringCombine03(): Bool = combine("", "b") == "b"

    @test
    def stringCombine04(): Bool = combine("a", "b") == "ab"

    /////////////////////////////////////////////////////////////////////////////
    // Option                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def optionEmpty01(): Bool = empty(): Option[Unit] == None

    @test
    def optionCombine01(): Bool = combine(None, None): Option[Unit] == None

    @test
    def optionCombine02(): Bool = combine(Some("a"), None) == Some("a")

    @test
    def optionCombine03(): Bool = combine(None, Some("1")) == Some("1")

    @test
    def optionCombine04(): Bool = combine(Some("a"), Some("1")) == Some("a1")

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 2                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple2Empty01(): Bool = empty() == ("", "")

    @test
    def tuple2Empty02(): Bool = empty() == ("", ())

    @test
    def tuple2Combine01(): Bool = combine(("", ""), ("", "")) == ("", "")

    @test
    def tuple2Combine02(): Bool = combine(("a", "b"), ("", "")) == ("a", "b")

    @test
    def tuple2Combine03(): Bool = combine(("", ""), ("1", "2")) == ("1", "2")

    @test
    def tuple2Combine04(): Bool = combine(("a", "b"), ("1", "2")) == ("a1", "b2")

    @test
    def tuple2Combine05(): Bool = combine(("a", ()), ("1", ())) == ("a1", ())

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 3                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple3Empty01(): Bool = empty() == ("", "", "")

    @test
    def tuple3Empty02(): Bool = empty() == ("", (), "")

    @test
    def tuple3Combine01(): Bool = combine(("", "", ""), ("", "", "")) == ("", "", "")

    @test
    def tuple3Combine02(): Bool = combine(("a", "b", "c"), ("", "", "")) == ("a", "b", "c")

    @test
    def tuple3Combine03(): Bool = combine(("", "", ""), ("1", "2", "3")) == ("1", "2", "3")

    @test
    def tuple3Combine04(): Bool = combine(("a", "b", "c"), ("1", "2", "3")) == ("a1", "b2", "c3")

    @test
    def tuple3Combine05(): Bool = combine(("a", (), "b"), ("1", (), "2")) == ("a1", (), "b2")

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 4                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple4Empty01(): Bool = empty() == ("", "", "", "")

    @test
    def tuple4Empty02(): Bool = empty() == ("", (), "", ())

    @test
    def tuple4Combine01(): Bool = combine(("", "", "", ""), ("", "", "", "")) == ("", "", "", "")

    @test
    def tuple4Combine02(): Bool = combine(("a", "b", "c", "d"), ("", "", "", "")) == ("a", "b", "c", "d")

    @test
    def tuple4Combine03(): Bool = combine(("", "", "", ""), ("1", "2", "3", "4")) == ("1", "2", "3", "4")

    @test
    def tuple4Combine04(): Bool = combine(("a", "b", "c", "d"), ("1", "2", "3", "4")) == ("a1", "b2", "c3", "d4")

    @test
    def tuple4Combine05(): Bool = combine(("a", (), "b", ()), ("1", (), "2", ())) == ("a1", (), "b2", ())

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 5                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple5Empty01(): Bool = empty() == ("", "", "", "", "")

    @test
    def tuple5Empty02(): Bool = empty() == ("", (), "", (), "")

    @test
    def tuple5Combine01(): Bool = combine(("", "", "", "", ""), ("", "", "", "", "")) == ("", "", "", "", "")

    @test
    def tuple5Combine02(): Bool = combine(("a", "b", "c", "d", "e"), ("", "", "", "", "")) == ("a", "b", "c", "d", "e")

    @test
    def tuple5Combine03(): Bool = combine(("", "", "", "", ""), ("1", "2", "3", "4", "5")) == ("1", "2", "3", "4", "5")

    @test
    def tuple5Combine04(): Bool = combine(("a", "b", "c", "d", "e"), ("1", "2", "3", "4", "5")) == ("a1", "b2", "c3", "d4", "e5")

    @test
    def tuple5Combine05(): Bool = combine(("a", (), "b", (), "c"), ("1", (), "2", (), "3")) == ("a1", (), "b2", (), "c3")

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 6                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple6Empty01(): Bool = empty() == ("", "", "", "", "", "")

    @test
    def tuple6Empty02(): Bool = empty() == ("", (), "", (), "", ())

    @test
    def tuple6Combine01(): Bool = combine(("", "", "", "", "", ""), ("", "", "", "", "", "")) == ("", "", "", "", "", "")

    @test
    def tuple6Combine02(): Bool = combine(("a", "b", "c", "d", "e", "f"), ("", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f")

    @test
    def tuple6Combine03(): Bool = combine(("", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6")) == ("1", "2", "3", "4", "5", "6")

    @test
    def tuple6Combine04(): Bool = combine(("a", "b", "c", "d", "e", "f"), ("1", "2", "3", "4", "5", "6")) == ("a1", "b2", "c3", "d4", "e5", "f6")

    @test
    def tuple6Combine05(): Bool = combine(("a", (), "b", (), "c", ()), ("1", (), "2", (), "3", ())) == ("a1", (), "b2", (), "c3", ())

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 7                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple7Empty01(): Bool = empty() == ("", "", "", "", "", "", "")

    @test
    def tuple7Empty02(): Bool = empty() == ("", (), "", (), "", (), "")

    @test
    def tuple7Combine01(): Bool = combine(("", "", "", "", "", "", ""), ("", "", "", "", "", "", "")) == ("", "", "", "", "", "", "")

    @test
    def tuple7Combine02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g"), ("", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g")

    @test
    def tuple7Combine03(): Bool = combine(("", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7")) == ("1", "2", "3", "4", "5", "6", "7")

    @test
    def tuple7Combine04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g"), ("1", "2", "3", "4", "5", "6", "7")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7")

    @test
    def tuple7Combine05(): Bool = combine(("a", (), "b", (), "c", (), "d"), ("1", (), "2", (), "3", (), "4")) == ("a1", (), "b2", (), "c3", (), "d4")

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 8                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple8Empty01(): Bool = empty() == ("", "", "", "", "", "", "", "")

    @test
    def tuple8Empty02(): Bool = empty() == ("", (), "", (), "", (), "", ())

    @test
    def tuple8Combine01(): Bool = combine(("", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "")) == ("", "", "", "", "", "", "", "")

    @test
    def tuple8Combine02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("", "", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g", "h")

    @test
    def tuple8Combine03(): Bool = combine(("", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8")) == ("1", "2", "3", "4", "5", "6", "7", "8")

    @test
    def tuple8Combine04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("1", "2", "3", "4", "5", "6", "7", "8")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8")

    @test
    def tuple8Combine05(): Bool = combine(("a", (), "b", (), "c", (), "d", ()), ("1", (), "2", (), "3", (), "4", ())) == ("a1", (), "b2", (), "c3", (), "d4", ())

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 9                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple9Empty01(): Bool = empty() == ("", "", "", "", "", "", "", "", "")

    @test
    def tuple9Empty02(): Bool = empty() == ("", (), "", (), "", (), "", (), "")

    @test
    def tuple9Combine01(): Bool = combine(("", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "")) == ("", "", "", "", "", "", "", "", "")

    @test
    def tuple9Combine02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("", "", "", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g", "h", "i")

    @test
    def tuple9Combine03(): Bool = combine(("", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9")) == ("1", "2", "3", "4", "5", "6", "7", "8", "9")

    @test
    def tuple9Combine04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("1", "2", "3", "4", "5", "6", "7", "8", "9")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9")

    @test
    def tuple9Combine05(): Bool = combine(("a", (), "b", (), "c", (), "d", (), "e"), ("1", (), "2", (), "3", (), "4", (), "5")) == ("a1", (), "b2", (), "c3", (), "d4", (), "e5")

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 10                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tuple10Empty01(): Bool = empty() == ("", "", "", "", "", "", "", "", "", "")

    @test
    def tuple10Empty02(): Bool = empty() == ("", (), "", (), "", (), "", (), "", ())

    @test
    def tuple10Combine01(): Bool = combine(("", "", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "", "")) == ("", "", "", "", "", "", "", "", "", "")

    @test
    def tuple10Combine02(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("", "", "", "", "", "", "", "", "", "")) == ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")

    @test
    def tuple10Combine03(): Bool = combine(("", "", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")) == ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")

    @test
    def tuple10Combine04(): Bool = combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")) == ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9", "j0")

    @test
    def tuple10Combine05(): Bool = combine(("a", (), "b", (), "c", (), "d", (), "e", ()), ("1", (), "2", (), "3", (), "4", (), "5", ())) == ("a1", (), "b2", (), "c3", (), "d4", (), "e5", ())

    /////////////////////////////////////////////////////////////////////////////
    // Validation                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def validationEmpty01(): Bool = empty(): Validation[String, String] == Success("")

    @test
    def validationCombine01(): Bool = combine(Failure(Nel.singleton("e1")), Failure(Nel.singleton("e2"))): Validation[String, String] == Failure(Nel.append(Nel.singleton("e1"), Nel.singleton("e2")))

    @test
    def validationCombine02(): Bool = combine(Success("a"), Failure(Nel.singleton("e1"))): Validation[String, String] == Failure(Nel.singleton("e1"))

    @test
    def validationCombine03(): Bool = combine(Failure(Nel.singleton("e1")), Success("1")): Validation[String, String] == Failure(Nel.singleton("e1"))

    @test
    def validationCombine04(): Bool = combine(Success("a"), Success("1")): Validation[String, String] == Success("a1")

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def listEmpty01(): Bool = empty(): List[Int32] == Nil

    @test
    def listCombine01(): Bool = combine(Nil, Nil): List[Int32] == Nil

    @test
    def listCombine02(): Bool = combine(1 :: Nil, Nil) == 1 :: Nil

    @test
    def listCombine03(): Bool = combine(Nil, 2 :: Nil) == 2 :: Nil

    @test
    def listCombine04(): Bool = combine(1 :: Nil, 2 :: Nil) == 1 :: 2 :: Nil

    @test
    def listCombine05(): Bool = combine(1 :: 2 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def setEmpty01(): Bool = empty(): Set[Int32] == Set#{}

    @test
    def setCombine01(): Bool = combine(Set#{}, Set#{}): Set[Int32] == Set#{}

    @test
    def setCombine02(): Bool = combine(Set#{1}, Set#{}) == Set#{1}

    @test
    def setCombine03(): Bool = combine(Set#{}, Set#{2}) == Set#{2}

    @test
    def setCombine04(): Bool = combine(Set#{1}, Set#{2}) == Set#{1, 2}

    @test
    def setCombine05(): Bool = combine(Set#{1, 2}, Set#{1, 2}) == Set#{1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapEmpty01(): Bool = empty(): Map[Int32, String] == Map#{}

    @test
    def mapCombine01(): Bool = combine(Map#{}, Map#{}): Map[Int32, String] == Map#{}

    @test
    def mapCombine02(): Bool = combine(Map#{1 -> "A"}, Map#{}) == Map#{1 -> "A"}

    @test
    def mapCombine03(): Bool = combine(Map#{}, Map#{1 -> "a"}) == Map#{1 -> "a"}

    @test
    def mapCombine04(): Bool = combine(Map#{1 -> "A"}, Map#{1 -> "a"}) == Map#{1 -> "Aa"}

    @test
    def mapCombine05(): Bool = combine(Map#{1 -> "A", 2 -> "B", 3 -> "C"}, Map#{2 -> "b", 3 -> "c", 4 -> "d"}) == Map#{1 -> "A", 2 -> "Bb", 3 -> "Cc", 4 -> "d"}

}