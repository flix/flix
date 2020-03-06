/*
 * Copyright 2017 Liam Palmer
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

namespace TestBigInt {

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = BigInt.min(1234ii, 12123123123124151532146469083259682341ii) == 1234ii

    @test
    def min02(): Bool = BigInt.min(22737461919238461234728137461283412ii, -191283756ii) == -191283756ii

    @test
    def min03(): Bool = BigInt.min(-33ii, -66ii) == -66ii

    @test
    def min04(): Bool = BigInt.min(-44ii, -922337203685477580889008ii) == -922337203685477580889008ii

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = BigInt.max(48ii, 49ii) == 49ii

    @test
    def max02(): Bool = BigInt.max(4ii, -169879871293847921347918234912394789123ii) == 4ii

    @test
    def max03(): Bool = BigInt.max(-3498457932459234592873452983453245ii, -16ii) == -16ii

    @test
    def max04(): Bool = BigInt.max(-34998734957235ii, 92233720368547758078990ii) == 92233720368547758078990ii

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def abs01(): Bool = BigInt.abs(0ii) == 0ii

    @test
    def abs02(): Bool = BigInt.abs(1ii) == 1ii

    @test
    def abs03(): Bool = BigInt.abs(-1ii) == 1ii

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def dist01(): Bool = BigInt.dist(1ii, -1ii) == 2ii

    @test
    def dist02(): Bool = BigInt.dist(-1ii, 1ii) == 2ii

    @test
    def dist03(): Bool = BigInt.dist(1ii, 1ii) == 0ii

    @test
    def dist04(): Bool = BigInt.dist(-1ii, -1ii) == 0ii

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def compare01(): Bool = BigInt.compare(1ii, -1ii) == 1

    @test
    def compare02(): Bool = BigInt.compare(-9ii, -11ii) == 1

    @test
    def compare03(): Bool = BigInt.compare(1ii, 1ii) == 0

    @test
    def compare04(): Bool = BigInt.compare(-1ii, -1ii) == 0

    @test
    def compare05(): Bool = BigInt.compare(-1ii, 1ii) == -1

    @test
    def compare06(): Bool = BigInt.compare(11ii, 21ii) == -1

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def signum01(): Bool = BigInt.signum(2ii) == 1

    @test
    def signum02(): Bool = BigInt.signum(3ii) == 1

    @test
    def signum03(): Bool = BigInt.signum(0ii) == 0

    @test
    def signum04(): Bool = BigInt.signum(-2ii) == -1

    @test
    def signum05(): Bool = BigInt.signum(-10ii) == -1

    /////////////////////////////////////////////////////////////////////////////
    // gcd                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def gcd01(): Bool = BigInt.gcd(9ii, 3ii) == 3ii

    @test
    def gcd02(): Bool = BigInt.gcd(21ii, 15ii) == 3ii

    @test
    def gcd03(): Bool = BigInt.gcd(9233ii, 0ii) == 9233ii

    @test
    def gcd04(): Bool = BigInt.gcd(0ii, 9233ii) == 9233ii

    @test
    def gcd05(): Bool = BigInt.gcd(9223ii, 33ii) == 1ii

    @test
    def gcd06(): Bool = BigInt.gcd(32462531054272512000001ii, 578837438475345ii) == 1ii

    @test
    def gcd07(): Bool = BigInt.gcd(32462531054272512000001ii, 578837982572398457234545ii) == 1ii

    @test
    def gcd08(): Bool = BigInt.gcd(32462531054272512000001ii, 92371823432134ii) == 1ii

    @test
    def gcd09(): Bool = BigInt.gcd(-9ii, -3ii) == 3ii

    @test
    def gcd010(): Bool = BigInt.gcd(21ii, -15ii) == 3ii

    @test
    def gcd011(): Bool = BigInt.gcd(-9233ii, 0ii) == 9233ii

    @test
    def gcd012(): Bool = BigInt.gcd(0ii, -9233ii) == 9233ii

    @test
    def gcd013(): Bool = BigInt.gcd(9223ii, -33ii) == 1ii

    @test
    def gcd014(): Bool = BigInt.gcd(-32462531054272512000001ii, 578837438475345ii) == 1ii

    @test
    def gcd015(): Bool = BigInt.gcd(-32462531054272512000001ii, 578837982572398457234545ii) == 1ii

    @test
    def gcd016(): Bool = BigInt.gcd(32462531054272512000001ii, -92371823432134ii) == 1ii

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def getBit01(): Bool = BigInt.getBit(1ii, 0) == 1

    @test
    def getBit02(): Bool = BigInt.getBit(1ii, 1) == 0

    @test
    def getBit03(): Bool = BigInt.getBit(8388608ii, 23) == 1

    @test
    def getBit04(): Bool = BigInt.getBit(8388608ii, 22) == 0

    @test
    def getBit05(): Bool = BigInt.getBit(8388608ii, 24) == 0

    @test
    def getBit06(): Bool = BigInt.getBit(9223372036854775808ii, 63) == 1

    @test
    def getBit07(): Bool = BigInt.getBit(9223372036854775808ii, 62) == 0

    @test
    def getBit08(): Bool = BigInt.getBit(9223372036854775808ii, 64) == 0

    @test
    def getBit09(): Bool = BigInt.getBit(154742504910672534362390528ii, 87) == 1

    @test
    def getBit10(): Bool = BigInt.getBit(154742504910672534362390528ii, 86) == 0

    @test
    def getBit11(): Bool = BigInt.getBit(154742504910672534362390528ii, 88) == 0

    @test
    def getBit12(): Bool = BigInt.getBit(0ii, 7) == 0

    @test
    def getBit13(): Bool = BigInt.getBit(-1ii, 17) == 1

    @test
    def getBit14(): Bool = BigInt.getBit(-17ii, 4) == 0

    @test
    def getBit15(): Bool = BigInt.getBit(-17ii, 3) == 1

    @test
    def getBit16(): Bool = BigInt.getBit(-17ii, 5) == 1

    @test
    def getBit17(): Bool = BigInt.getBit(-898274123413412341ii, 4) == 0

    @test
    def getBit18(): Bool = BigInt.getBit(-898274123413412341ii, 3) == 1

    @test
    def getBit19(): Bool = BigInt.getBit(-898274123413412341ii, 35) == 0

    @test
    def getBit20(): Bool = BigInt.getBit(-898274123413412341ii, 36) == 1

    @test
    def getBit21(): Bool = BigInt.getBit(-898274123413412341ii, 37) == 1

    @test
    def getBit22(): Bool = BigInt.getBit(-8982741234134123419879712341ii, 92) == 0

    @test
    def getBit23(): Bool = BigInt.getBit(-8982741234134123419879712341ii, 93) == 1

    @test
    def getBit24(): Bool = BigInt.getBit(-8982741234134123419879712341ii, 94) == 1

    @test
    def getBit25(): Bool = BigInt.getBit(-8982741234134123419879712341ii, 95) == 1

    @test
    def getBit26(): Bool = BigInt.getBit(-8982741234134123419879712341ii, 96) == 1

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def setBit01(): Bool = BigInt.setBit(0ii, 0) == 1ii

    @test
    def setBit02(): Bool = BigInt.setBit(0ii, 1) == 2ii

    @test
    def setBit03(): Bool = BigInt.setBit(0ii, 31) == 2147483648ii

    @test
    def setBit04(): Bool = BigInt.setBit(0ii, 32) == 4294967296ii

    @test
    def setBit05(): Bool = BigInt.setBit(2305843009213693952ii, 55) == 2341871806232657920ii

    @test
    def setBit06(): Bool = BigInt.setBit(2305843009213693952ii, 62) == 6917529027641081856ii

    @test
    def setBit07(): Bool = BigInt.setBit(-17ii, 4) == -1ii

    @test
    def setBit08(): Bool = BigInt.setBit(-1025ii, 10) == -1ii

    @test
    def setBit09(): Bool = BigInt.setBit(-17ii, 31) == -17ii

    @test
    def setBit10(): Bool = BigInt.setBit(-17ii, 32) == -17ii

    @test
    def setBit11(): Bool = BigInt.setBit(-17ii, 101) == -17ii

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clearBit01(): Bool = BigInt.clearBit(1ii, 0) == 0ii

    @test
    def clearBit02(): Bool = BigInt.clearBit(1ii, 1) == 1ii

    @test
    def clearBit03(): Bool = BigInt.clearBit(2ii, 1) == 0ii

    @test
    def clearBit04(): Bool = BigInt.clearBit(2ii, 2) == 2ii

    @test
    def clearBit05(): Bool = BigInt.clearBit(0ii, 31) == 0ii

    @test
    def clearBit06(): Bool = BigInt.clearBit(0ii, 32) == 0ii

    @test
    def clearBit07(): Bool = BigInt.clearBit(2147483648ii, 31) == 0ii

    @test
    def clearBit08(): Bool = BigInt.clearBit(618970019642690137449562112ii, 89) == 0ii

    @test
    def clearBit09(): Bool = BigInt.clearBit(-1ii, 0) == -2ii

    @test
    def clearBit10(): Bool = BigInt.clearBit(-1ii, 1) == -3ii

    @test
    def clearBit11(): Bool = BigInt.clearBit(-1ii, 5) == -33ii

    @test
    def clearBit12(): Bool = BigInt.clearBit(-1ii, 31) == -2147483649ii

    @test
    def clearBit13(): Bool = BigInt.clearBit(-1ii, 32) == -4294967297ii

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flipBit01(): Bool = BigInt.flipBit(1ii, 0) == 0ii

    @test
    def flipBit02(): Bool = BigInt.flipBit(1ii, 1) == 3ii

    @test
    def flipBit03(): Bool = BigInt.flipBit(2ii, 1) == 0ii

    @test
    def flipBit04(): Bool = BigInt.flipBit(2ii, 2) == 6ii

    @test
    def flipBit05(): Bool = BigInt.flipBit(0ii, 31) == 2147483648ii

    @test
    def flipBit06(): Bool = BigInt.flipBit(0ii, 32) == 4294967296ii

    @test
    def flipBit07(): Bool = BigInt.flipBit(2147483648ii, 31) == 0ii

    @test
    def flipBit08(): Bool = BigInt.flipBit(618970019642690137449562112ii, 89) == 0ii

    @test
    def flipBit09(): Bool = BigInt.flipBit(-1ii, 0) == -2ii

    @test
    def flipBit10(): Bool = BigInt.flipBit(-1ii, 1) == -3ii

    @test
    def flipBit11(): Bool = BigInt.flipBit(-1ii, 5) == -33ii

    @test
    def flipBit12(): Bool = BigInt.flipBit(-1ii, 31) == -2147483649ii

    @test
    def flipBit13(): Bool = BigInt.flipBit(-1ii, 32) == -4294967297ii

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = BigInt.toString(0ii) == "0"

    @test
    def toString02(): Bool = BigInt.toString(1ii) == "1"

    @test
    def toString03(): Bool = BigInt.toString(-1ii) == "-1"

    @test
    def toString04(): Bool = BigInt.toString(3498457932459234592873452983453245ii) == "3498457932459234592873452983453245"

    @test
    def toString05(): Bool = BigInt.toString(-3498457932459234592873452983453245ii) == "-3498457932459234592873452983453245"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = BigInt.fromString("Bad") |> Result.isErr

    @test
    def fromString02(): Bool = BigInt.fromString(" 1") == Ok(1ii)

    @test
    def fromString03(): Bool = BigInt.fromString("1 ") == Ok(1ii)

    @test
    def fromString04(): Bool = BigInt.fromString("0") == Ok(0ii)

    @test
    def fromString05(): Bool = BigInt.fromString("1") == Ok(1ii)

    @test
    def fromString06(): Bool = BigInt.fromString("-1") == Ok(-1ii)

    @test
    def fromString07(): Bool = BigInt.fromString("3498457932459234592873452983453245") == Ok(3498457932459234592873452983453245ii)

    @test
    def fromString08(): Bool = BigInt.fromString("-3498457932459234592873452983453245") == Ok(-3498457932459234592873452983453245ii)

}
