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

mod TestBigInt {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mul01(): Bool = mul(1ii, 0ii) == 0ii

    @test
    def mul02(): Bool = mul(0ii, 1ii) == 0ii

    @test
    def mul03(): Bool = mul(0ii, 0ii) == 0ii

    @test
    def mul04(): Bool = mul(2ii, -3498457932459234592873452983453245ii) == -6996915864918469185746905966906490ii

    @test
    def mul05(): Bool = mul(7ii, 4681ii) == 32767ii

    @test
    def mul06(): Bool = mul(77ii, 4812ii) == 370524ii

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
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def mod01(): Bool = (1200000000000000000000ii `BigInt.mod` 200000000000000000000ii) == 0ii

    @test
    def mod02(): Bool = (1200000000000000000000ii `BigInt.mod` 500000000000000000000ii) == 200000000000000000000ii

    @test
    def mod03(): Bool = (-1200000000000000000000ii `BigInt.mod` 500000000000000000000ii) == 300000000000000000000ii

    @test
    def mod04(): Bool = (1200000000000000000000ii `BigInt.mod` -500000000000000000000ii) == -300000000000000000000ii

    @test
    def mod05(): Bool = (-1200000000000000000000ii `BigInt.mod` -500000000000000000000ii) == -200000000000000000000ii

    @test
    def mod06(): Bool = (1ii `BigInt.mod` 0ii) == 0ii

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rem01(): Bool = (1200000000000000000000ii `BigInt.rem` 200000000000000000000ii) == 0ii

    @test
    def rem02(): Bool = (1200000000000000000000ii `BigInt.rem` 500000000000000000000ii) == 200000000000000000000ii

    @test
    def rem03(): Bool = (-1200000000000000000000ii `BigInt.rem` 500000000000000000000ii) == -200000000000000000000ii

    @test
    def rem04(): Bool = (1200000000000000000000ii `BigInt.rem` -500000000000000000000ii) == 200000000000000000000ii

    @test
    def rem05(): Bool = (-1200000000000000000000ii `BigInt.rem` -500000000000000000000ii) == -200000000000000000000ii

    @test
    def rem06(): Bool = (1ii `BigInt.rem` 0ii) == 0ii

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
    // leftShift
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseLeftShift01(): Bool = BigInt.leftShift(8ii, 0) == 8ii

    @test
    def testBitwiseLeftShift02(): Bool = BigInt.leftShift(8ii, 32) == 34359738368ii

    @test
    def testBitwiseLeftShift03(): Bool = BigInt.leftShift(8ii, 64) == 147573952589676412928ii


    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseRightShift04(): Bool = BigInt.rightShift(1200000000000000000000ii, 0) == 1200000000000000000000ii

    @test
    def testBitwiseRightShift05(): Bool = BigInt.rightShift(1200000000000000000000ii, 2) == 300000000000000000000ii

    @test
    def testBitwiseRightShift06(): Bool = BigInt.rightShift(1200000000000000000000ii, 72) == 0ii

    @test
    def testBitwiseRightShift07(): Bool = BigInt.rightShift(-1200000000000000000000ii, 2) == -300000000000000000000ii

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseAnd01(): Bool = BigInt.bitwiseAnd(0ii, 0ii) == 0ii

    @test
    def bitwiseAnd02(): Bool = BigInt.bitwiseAnd(127ii, 0ii) == 0ii

    @test
    def bitwiseAnd03(): Bool = BigInt.bitwiseAnd(-128ii, 0ii) == 0ii

    @test
    def bitwiseAnd04(): Bool = BigInt.bitwiseAnd(0ii, 1ii) == 0ii

    @test
    def bitwiseAnd05(): Bool = BigInt.bitwiseAnd(127ii, 1ii) == 1ii

    @test
    def bitwiseAnd06(): Bool = BigInt.bitwiseAnd(-128ii, 1ii) == 0ii

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseNot01(): Bool = BigInt.bitwiseNot(0ii) == -1ii

    @test
    def bitwiseNot02(): Bool = BigInt.bitwiseNot(1ii) == -2ii

    @test
    def bitwiseNot03(): Bool = BigInt.bitwiseNot(-1ii) == -0ii

    @test
    def bitwiseNot04(): Bool = BigInt.bitwiseNot(127ii) == -128ii

    @test
    def bitwiseNot05(): Bool = BigInt.bitwiseNot(-128ii) == 127ii

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseOr01(): Bool = BigInt.bitwiseOr(0ii, 0ii) == 0ii

    @test
    def bitwiseOr02(): Bool = BigInt.bitwiseOr(127ii, 0ii) == 127ii

    @test
    def bitwiseOr03(): Bool = BigInt.bitwiseOr(-128ii, 0ii) == -128ii

    @test
    def bitwiseOr04(): Bool = BigInt.bitwiseOr(0ii, 1ii) == 1ii

    @test
    def bitwiseOr05(): Bool = BigInt.bitwiseOr(127ii, 1ii) == 127ii

    @test
    def bitwiseOr06(): Bool = BigInt.bitwiseOr(-128ii, 1ii) == -127ii

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseXor01(): Bool = BigInt.bitwiseXor(0ii, 0ii) == 0ii

    @test
    def bitwiseXor02(): Bool = BigInt.bitwiseXor(127ii, 0ii) == 127ii

    @test
    def bitwiseXor03(): Bool = BigInt.bitwiseXor(-128ii, 0ii) == -128ii

    @test
    def bitwiseXor04(): Bool = BigInt.bitwiseXor(0ii, 1ii) == 1ii

    @test
    def bitwiseXor05(): Bool = BigInt.bitwiseXor(127ii, 1ii) == 126ii

    @test
    def bitwiseXor06(): Bool = BigInt.bitwiseXor(-128ii, 1ii) == -127ii

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
    def fromString01(): Bool = BigInt.fromString("Bad") |> Option.isEmpty

    @test
    def fromString02(): Bool = BigInt.fromString(" 1") == Some(1ii)

    @test
    def fromString03(): Bool = BigInt.fromString("1 ") == Some(1ii)

    @test
    def fromString04(): Bool = BigInt.fromString("0") == Some(0ii)

    @test
    def fromString05(): Bool = BigInt.fromString("1") == Some(1ii)

    @test
    def fromString06(): Bool = BigInt.fromString("-1") == Some(-1ii)

    @test
    def fromString07(): Bool = BigInt.fromString("3498457932459234592873452983453245") == Some(3498457932459234592873452983453245ii)

    @test
    def fromString08(): Bool = BigInt.fromString("-3498457932459234592873452983453245") == Some(-3498457932459234592873452983453245ii)

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = BigInt.tryToInt8(0ii) == Some(0i8)

    @test
    def tryToInt802(): Bool = BigInt.tryToInt8(1ii) == Some(1i8)

    @test
    def tryToInt803(): Bool = BigInt.tryToInt8(-1ii) == Some(-1i8)

    @test
    def tryToInt804(): Bool = BigInt.tryToInt8(127ii) == Some(127i8)

    @test
    def tryToInt805(): Bool = BigInt.tryToInt8(128ii) == None

    @test
    def tryToInt806(): Bool = BigInt.tryToInt8(-128ii) == Some(-128i8)

    @test
    def tryToInt807(): Bool = BigInt.tryToInt8(-129ii) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt1601(): Bool = BigInt.tryToInt16(0ii) == Some(0i16)

    @test
    def tryToInt1602(): Bool = BigInt.tryToInt16(1ii) == Some(1i16)

    @test
    def tryToInt1603(): Bool = BigInt.tryToInt16(-1ii) == Some(-1i16)

    @test
    def tryToInt1604(): Bool = BigInt.tryToInt16(32767ii) == Some(32767i16)

    @test
    def tryToInt1605(): Bool = BigInt.tryToInt16(32768ii) == None

    @test
    def tryToInt1606(): Bool = BigInt.tryToInt16(-32768ii) == Some(-32768i16)

    @test
    def tryToInt1607(): Bool = BigInt.tryToInt16(-32769ii) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt3201(): Bool = BigInt.tryToInt32(0ii) == Some(0)

    @test
    def tryToInt3202(): Bool = BigInt.tryToInt32(1ii) == Some(1)

    @test
    def tryToInt3203(): Bool = BigInt.tryToInt32(-1ii) == Some(-1)

    @test
    def tryToInt3204(): Bool = BigInt.tryToInt32(2147483647ii) == Some(2147483647)

    @test
    def tryToInt3205(): Bool = BigInt.tryToInt32(2147483648ii) == None

    @test
    def tryToInt3206(): Bool = BigInt.tryToInt32(-2147483648ii) == Some(-2147483648)

    @test
    def tryToInt3207(): Bool = BigInt.tryToInt32(-2147483649ii) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt6401(): Bool = BigInt.tryToInt64(0ii) == Some(0i64)

    @test
    def tryToInt6402(): Bool = BigInt.tryToInt64(1ii) == Some(1i64)

    @test
    def tryToInt6403(): Bool = BigInt.tryToInt64(-1ii) == Some(-1i64)

    @test
    def tryToInt6404(): Bool = BigInt.tryToInt64(9223372036854775807ii) == Some(9223372036854775807i64)

    @test
    def tryToInt6405(): Bool = BigInt.tryToInt64(9223372036854775808ii) == None

    @test
    def tryToInt6406(): Bool = BigInt.tryToInt64(-9223372036854775808ii) == Some(-9223372036854775808i64)

    @test
    def tryToInt6407(): Bool = BigInt.tryToInt64(-9223372036854775809ii) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToFloat3201(): Bool = BigInt.tryToFloat32(0ii) == Some(0.0f32)

    @test
    def tryToFloat3202(): Bool = BigInt.tryToFloat32(1ii) == Some(1.0f32)

    @test
    def tryToFloat3203(): Bool = BigInt.tryToFloat32(-1ii) == Some(-1.0f32)

    @test
    def tryToFloat3204(): Bool =
        let step = () -> {
            forM (
                max <- Float32.maxValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(max)
            ) yield res
        };
        step() != None

    @test
    def tryToFloat3205(): Bool =
        let step = () -> {
            forM (
                max <- Float32.maxValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(max + 1ii)
            ) yield res
        };
        step() == None

    @test
    def tryToFloat3206(): Bool =
        let step = () -> {
            forM (
                min <- Float32.minValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(min)
            ) yield res
        };
        step() != None

    @test
    def tryToFloat3207(): Bool =
        let step = () -> {
            forM (
                min <- Float32.minValue() |> Float32.tryToBigInt;
                res <- BigInt.tryToFloat32(min - 1ii)
            ) yield res
        };
        step() == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToFloat64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToFloat6401(): Bool = BigInt.tryToFloat64(0ii) == Some(0.0f64)

    @test
    def tryToFloat6402(): Bool = BigInt.tryToFloat64(1ii) == Some(1.0f64)

    @test
    def tryToFloat6403(): Bool = BigInt.tryToFloat64(-1ii) == Some(-1.0f64)

    @test
    def tryToFloat6404(): Bool =
        let step = () -> {
            forM (
                max <- Float64.maxValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(max)
            ) yield res
        };
        step() != None

    @test
    def tryToFloat6405(): Bool =
        let step = () -> {
            forM (
                max <- Float64.maxValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(max + 1ii)
            ) yield res
        };
        step() == None

    @test
    def tryToFloat6406(): Bool =
        let step = () -> {
            forM (
                min <- Float64.minValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(min)
            ) yield res
        };
        step() != None

    @test
    def tryToFloat6407(): Bool =
        let step = () -> {
            forM (
                min <- Float64.minValue() |> Float64.tryToBigInt;
                res <- BigInt.tryToFloat64(min - 1ii)
            ) yield res
        };
        step() == None

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigDecimal01(): Bool = BigInt.toBigDecimal(0ii) == 0.0ff

    @test
    def toBigDecimal02(): Bool = BigInt.toBigDecimal(1ii) == 1.0ff

    @test
    def toBigDecimal03(): Bool = BigInt.toBigDecimal(-1ii) == -1.0ff

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt801(): Bool = BigInt.clampToInt8(0ii, -100i8, 100i8) == 0i8

    @test
    def clampToInt802(): Bool = BigInt.clampToInt8(100ii, -100i8, 100i8) == 100i8

    @test
    def clampToInt803(): Bool = BigInt.clampToInt8(101ii, -100i8, 100i8) == 100i8

    @test
    def clampToInt804(): Bool = BigInt.clampToInt8(-100ii, -100i8, 100i8) == -100i8

    @test
    def clampToInt805(): Bool = BigInt.clampToInt8(-101ii, -100i8, 100i8) == -100i8

    @test
    def clampToInt806(): Bool = BigInt.clampToInt8(8982741234134123419879712341ii, -100i8, 100i8) == 100i8

    @test
    def clampToInt807(): Bool = BigInt.clampToInt8(-8982741234134123419879712341ii, -100i8, 100i8) == -100i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt1601(): Bool = BigInt.clampToInt16(0ii, -100i16, 100i16) == 0i16

    @test
    def clampToInt1602(): Bool = BigInt.clampToInt16(100ii, -100i16, 100i16) == 100i16

    @test
    def clampToInt1603(): Bool = BigInt.clampToInt16(101ii, -100i16, 100i16) == 100i16

    @test
    def clampToInt1604(): Bool = BigInt.clampToInt16(-100ii, -100i16, 100i16) == -100i16

    @test
    def clampToInt1605(): Bool = BigInt.clampToInt16(-101ii, -100i16, 100i16) == -100i16

    @test
    def clampToInt1606(): Bool = BigInt.clampToInt16(8982741234134123419879712341ii, -100i16, 100i16) == 100i16

    @test
    def clampToInt1607(): Bool = BigInt.clampToInt16(-8982741234134123419879712341ii, -100i16, 100i16) == -100i16

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt3201(): Bool = BigInt.clampToInt32(0ii, -100, 100) == 0

    @test
    def clampToInt3202(): Bool = BigInt.clampToInt32(100ii, -100, 100) == 100

    @test
    def clampToInt3203(): Bool = BigInt.clampToInt32(101ii, -100, 100) == 100

    @test
    def clampToInt3204(): Bool = BigInt.clampToInt32(-100ii, -100, 100) == -100

    @test
    def clampToInt3205(): Bool = BigInt.clampToInt32(-101ii, -100, 100) == -100

    @test
    def clampToInt3206(): Bool = BigInt.clampToInt32(8982741234134123419879712341ii, -100i32, 100i32) == 100i32

    @test
    def clampToInt3207(): Bool = BigInt.clampToInt32(-8982741234134123419879712341ii, -100i32, 100i32) == -100i32

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt6401(): Bool = BigInt.clampToInt64(0ii, -100i64, 100i64) == 0i64

    @test
    def clampToInt6402(): Bool = BigInt.clampToInt64(100ii, -100i64, 100i64) == 100i64

    @test
    def clampToInt6403(): Bool = BigInt.clampToInt64(101ii, -100i64, 100i64) == 100i64

    @test
    def clampToInt6404(): Bool = BigInt.clampToInt64(-100ii, -100i64, 100i64) == -100i64

    @test
    def clampToInt6405(): Bool = BigInt.clampToInt64(-101ii, -100i64, 100i64) == -100i64

    @test
    def clampToInt6406(): Bool = BigInt.clampToInt64(8982741234134123419879712341ii, -100i64, 100i64) == 100i64

    @test
    def clampToInt6407(): Bool = BigInt.clampToInt64(-8982741234134123419879712341ii, -100i64, 100i64) == -100i64

    /////////////////////////////////////////////////////////////////////////////
    // clampToFloat32                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToFloat3201(): Bool = BigInt.clampToFloat32(0ii, -100.0f32, 100.0f32) == 0.0f32

    @test
    def clampToFloat3202(): Bool = BigInt.clampToFloat32(100ii, -100.0f32, 100.0f32) == 100.0f32

    @test
    def clampToFloat3203(): Bool = BigInt.clampToFloat32(101ii, -100.0f32, 100.0f32) == 100.0f32

    @test
    def clampToFloat3204(): Bool = BigInt.clampToFloat32(-100ii, -100.0f32, 100.0f32) == -100.0f32

    @test
    def clampToFloat3205(): Bool = BigInt.clampToFloat32(-101ii, -100.0f32, 100.0f32) == -100.0f32

    @test
    def clampToFloat3206(): Bool = BigInt.clampToFloat32(8982741234134123419879712341ii, -100.0f32, 100.0f32) == 100.0f32

    @test
    def clampToFloat3207(): Bool = BigInt.clampToFloat32(-8982741234134123419879712341ii, -100.0f32, 100.0f32) == -100.0f32

    /////////////////////////////////////////////////////////////////////////////
    // clampToFloat64                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToFloat6401(): Bool = BigInt.clampToFloat64(0ii, -100.0f64, 100.0f64) == 0.0f64

    @test
    def clampToFloat6402(): Bool = BigInt.clampToFloat64(100ii, -100.0f64, 100.0f64) == 100.0f64

    @test
    def clampToFloat6403(): Bool = BigInt.clampToFloat64(101ii, -100.0f64, 100.0f64) == 100.0f64

    @test
    def clampToFloat6404(): Bool = BigInt.clampToFloat64(-100ii, -100.0f64, 100.0f64) == -100.0f64

    @test
    def clampToFloat6405(): Bool = BigInt.clampToFloat64(-101ii, -100.0f64, 100.0f64) == -100.0f64

    @test
    def clampToFloat6406(): Bool = BigInt.clampToFloat64(8982741234134123419879712341ii, -100.0f64, 100.0f64) == 100.0f64

    @test
    def clampToFloat6407(): Bool = BigInt.clampToFloat64(-8982741234134123419879712341ii, -100.0f64, 100.0f64) == -100.0f64

}
