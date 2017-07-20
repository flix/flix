@test
def binaryBitwiseAndTest01(): Bool = assertEq!(40000 &&& -1, 40000)

@test
def binaryBitwiseAndTest02(): Bool = assertEq!(40000 &&& 40000, 40000)

@test
def binaryBitwiseAndTest03(): Bool = assertEq!(40000 &&& 0, 0)

@test
def binaryBitwiseAndTest04(): Bool = assertEq!(-1 &&& -1, -1)

@test
def binaryBitwiseAndTest05(): Bool = assertEq!(40i8 &&& -1i8, 40i8)

@test
def binaryBitwiseAndTest06(): Bool = assertEq!(40i8 &&& 40i8, 40i8)

@test
def binaryBitwiseAndTest07(): Bool = assertEq!(40i8 &&& 0i8, 0i8)

@test
def binaryBitwiseAndTest08(): Bool = assertEq!(-1i8 &&& -1i8, -1i8)

@test
def binaryBitwiseAndTest09(): Bool = assertEq!(400i16 &&& -1i16, 400i16)

@test
def binaryBitwiseAndTest10(): Bool = assertEq!(400i16 &&& 400i16, 400i16)

@test
def binaryBitwiseAndTest11(): Bool = assertEq!(400i16 &&& 0i16, 0i16)

@test
def binaryBitwiseAndTest12(): Bool = assertEq!(-1i16 &&& -1i16, -1i16)

@test
def binaryBitwiseAndTest13(): Bool = assertEq!(40000i32 &&& -1i32, 40000i32)

@test
def binaryBitwiseAndTest14(): Bool = assertEq!(40000i32 &&& 40000i32, 40000i32)

@test
def binaryBitwiseAndTest15(): Bool = assertEq!(40000i32 &&& 0i32, 0i32)

@test
def binaryBitwiseAndTest16(): Bool = assertEq!(-1i32 &&& -1i32, -1i32)

@test
def binaryBitwiseAndTest17(): Bool = assertEq!(40000000000i64 &&& -1i64, 40000000000i64)

@test
def binaryBitwiseAndTest18(): Bool = assertEq!(40000000000i64 &&& 40000000000i64, 40000000000i64)

@test
def binaryBitwiseAndTest19(): Bool = assertEq!(40000000000i64 &&& 0i64, 0i64)

@test
def binaryBitwiseAndTest20(): Bool = assertEq!(-1i64 &&& -1i64, -1i64)

@test
def binaryBitwiseAndTest21(): Bool = assertEq!(40000000000000000000ii &&& -1ii, 40000000000000000000ii)

@test
def binaryBitwiseAndTest22(): Bool = assertEq!(40000000000000000000ii &&& 40000000000000000000ii, 40000000000000000000ii)

@test
def binaryBitwiseAndTest23(): Bool = assertEq!(40000000000000000000ii &&& 0ii, 0ii)

@test
def binaryBitwiseAndTest24(): Bool = assertEq!(-1ii &&& -1ii, -1ii)

@test
def binaryBitwiseOrTest01(): Bool = assertEq!(40000 ||| -1, -1)

@test
def binaryBitwiseOrTest02(): Bool = assertEq!(40000 ||| 40000, 40000)

@test
def binaryBitwiseOrTest03(): Bool = assertEq!(40000 ||| 0, 40000)

@test
def binaryBitwiseOrTest04(): Bool = assertEq!(-1 ||| -1, -1)

@test
def binaryBitwiseOrTest05(): Bool = assertEq!(40i8 ||| -1i8, -1i8)

@test
def binaryBitwiseOrTest06(): Bool = assertEq!(40i8 ||| 40i8, 40i8)

@test
def binaryBitwiseOrTest07(): Bool = assertEq!(40i8 ||| 0i8, 40i8)

@test
def binaryBitwiseOrTest08(): Bool = assertEq!(-1i8 ||| -1i8, -1i8)

@test
def binaryBitwiseOrTest09(): Bool = assertEq!(400i16 ||| -1i16, -1i16)

@test
def binaryBitwiseOrTest10(): Bool = assertEq!(400i16 ||| 400i16, 400i16)

@test
def binaryBitwiseOrTest11(): Bool = assertEq!(400i16 ||| 0i16, 400i16)

@test
def binaryBitwiseOrTest12(): Bool = assertEq!(-1i16 ||| -1i16, -1i16)

@test
def binaryBitwiseOrTest13(): Bool = assertEq!(40000i32 ||| -1i32, -1i32)

@test
def binaryBitwiseOrTest14(): Bool = assertEq!(40000i32 ||| 40000i32, 40000i32)

@test
def binaryBitwiseOrTest15(): Bool = assertEq!(40000i32 ||| 0i32, 40000i32)

@test
def binaryBitwiseOrTest16(): Bool = assertEq!(-1i32 ||| -1i32, -1i32)

@test
def binaryBitwiseOrTest17(): Bool = assertEq!(40000000000i64 ||| -1i64, -1i64)

@test
def binaryBitwiseOrTest18(): Bool = assertEq!(40000000000i64 ||| 40000000000i64, 40000000000i64)

@test
def binaryBitwiseOrTest19(): Bool = assertEq!(40000000000i64 ||| 0i64, 40000000000i64)

@test
def binaryBitwiseOrTest20(): Bool = assertEq!(-1i64 ||| -1i64, -1i64)

@test
def binaryBitwiseOrTest21(): Bool = assertEq!(40000000000000000000ii ||| -1ii, -1ii)

@test
def binaryBitwiseOrTest22(): Bool = assertEq!(40000000000000000000ii ||| 40000000000000000000ii, 40000000000000000000ii)

@test
def binaryBitwiseOrTest23(): Bool = assertEq!(40000000000000000000ii ||| 0ii, 40000000000000000000ii)

@test
def binaryBitwiseOrTest24(): Bool = assertEq!(-1ii ||| -1ii, -1ii)

@test
def binaryBitwiseXorTest01(): Bool = assertEq!(40000 ^^^ -1, -40001)

@test
def binaryBitwiseXorTest02(): Bool = assertEq!(40000 ^^^ 40000, 0)

@test
def binaryBitwiseXorTest03(): Bool = assertEq!(40000 ^^^ 0, 40000)

@test
def binaryBitwiseXorTest04(): Bool = assertEq!(-1 ^^^ -1, 0)

@test
def binaryBitwiseXorTest05(): Bool = assertEq!(40i8 ^^^ -1i8, -41i8)

@test
def binaryBitwiseXorTest06(): Bool = assertEq!(40i8 ^^^ 40i8, 0i8)

@test
def binaryBitwiseXorTest07(): Bool = assertEq!(40i8 ^^^ 0i8, 40i8)

@test
def binaryBitwiseXorTest08(): Bool = assertEq!(-1i8 ^^^ -1i8, 0i8)

@test
def binaryBitwiseXorTest09(): Bool = assertEq!(400i16 ^^^ -1i16, -401i16)

@test
def binaryBitwiseXorTest10(): Bool = assertEq!(400i16 ^^^ 400i16, 0i16)

@test
def binaryBitwiseXorTest11(): Bool = assertEq!(400i16 ^^^ 0i16, 400i16)

@test
def binaryBitwiseXorTest12(): Bool = assertEq!(-1i16 ^^^ -1i16, 0i16)

@test
def binaryBitwiseXorTest13(): Bool = assertEq!(40000i32 ^^^ -1i32, -40001i32)

@test
def binaryBitwiseXorTest14(): Bool = assertEq!(40000i32 ^^^ 40000i32, 0i32)

@test
def binaryBitwiseXorTest15(): Bool = assertEq!(40000i32 ^^^ 0i32, 40000i32)

@test
def binaryBitwiseXorTest16(): Bool = assertEq!(-1i32 ^^^ -1i32, 0i32)

@test
def binaryBitwiseXorTest17(): Bool = assertEq!(40000000000i64 ^^^ -1i64, -40000000001i64)

@test
def binaryBitwiseXorTest18(): Bool = assertEq!(40000000000i64 ^^^ 40000000000i64, 0i64)

@test
def binaryBitwiseXorTest19(): Bool = assertEq!(40000000000i64 ^^^ 0i64, 40000000000i64)

@test
def binaryBitwiseXorTest20(): Bool = assertEq!(-1i64 ^^^ -1i64, 0i64)

@test
def binaryBitwiseXorTest21(): Bool = assertEq!(40000000000000000000ii ^^^ -1ii, -40000000000000000001ii)

@test
def binaryBitwiseXorTest22(): Bool = assertEq!(40000000000000000000ii ^^^ 40000000000000000000ii, 0ii)

@test
def binaryBitwiseXorTest23(): Bool = assertEq!(40000000000000000000ii ^^^ 0ii, 40000000000000000000ii)

@test
def binaryBitwiseXorTest24(): Bool = assertEq!(-1ii ^^^ -1ii, 0ii)

@test
def binaryBitwiseLeftShift01(): Bool = assertEq!(8 <<< 0, 8)

@test
def binaryBitwiseLeftShift02(): Bool = assertEq!(8 <<< 16, 524288)

@test
def binaryBitwiseLeftShift03(): Bool = assertEq!(8 <<< 28, -2147483648)

@test
def binaryBitwiseLeftShift04(): Bool = assertEq!(8 <<< 29, 0)

@test
def binaryBitwiseLeftShift05(): Bool = assertEq!(8i8 <<< 0, 8i8)

@test
def binaryBitwiseLeftShift06(): Bool = assertEq!(8i8 <<< 2, 32i8)

@test
def binaryBitwiseLeftShift07(): Bool = assertEq!(8i8 <<< 4, -128i8)

@test
def binaryBitwiseLeftShift08(): Bool = assertEq!(8i8 <<< 5, 0i8)

@test
def binaryBitwiseLeftShift09(): Bool = assertEq!(8i16 <<< 0, 8i16)

@test
def binaryBitwiseLeftShift10(): Bool = assertEq!(8i16 <<< 8, 2048i16)

@test
def binaryBitwiseLeftShift11(): Bool = assertEq!(8i16 <<< 12, -32768i16)

@test
def binaryBitwiseLeftShift12(): Bool = assertEq!(8i16 <<< 13, 0i16)

@test
def binaryBitwiseLeftShift13(): Bool = assertEq!(8i32 <<< 0, 8i32)

@test
def binaryBitwiseLeftShift14(): Bool = assertEq!(8i32 <<< 16, 524288i32)

@test
def binaryBitwiseLeftShift15(): Bool = assertEq!(8i32 <<< 28, -2147483648i32)

@test
def binaryBitwiseLeftShift16(): Bool = assertEq!(8i32 <<< 29, 0i32)

@test
def binaryBitwiseLeftShift17(): Bool = assertEq!(8i64 <<< 0, 8i64)

@test
def binaryBitwiseLeftShift18(): Bool = assertEq!(8i64 <<< 32, 34359738368i64)

@test
def binaryBitwiseLeftShift19(): Bool = assertEq!(8i64 <<< 60, -9223372036854775808i64)

@test
def binaryBitwiseLeftShift20(): Bool = assertEq!(8i64 <<< 61, 0i64)

@test
def binaryBitwiseLeftShift21(): Bool = assertEq!(8ii <<< 0, 8ii)

@test
def binaryBitwiseLeftShift22(): Bool = assertEq!(8ii <<< 32, 34359738368ii)

@test
def binaryBitwiseLeftShift23(): Bool = assertEq!(8ii <<< 64, 147573952589676412928ii)

@test
def binaryBitwiseRightShift01(): Bool = assertEq!(120000 >>> 0, 120000)

@test
def binaryBitwiseRightShift02(): Bool = assertEq!(120000 >>> 2, 30000)

@test
def binaryBitwiseRightShift03(): Bool = assertEq!(120000 >>> 31, 0)

@test
def binaryBitwiseRightShift04(): Bool = assertEq!(-120000 >>> 2, -30000)

@test
def binaryBitwiseRightShift05(): Bool = assertEq!(120i8 >>> 0, 120i8)

@test
def binaryBitwiseRightShift06(): Bool = assertEq!(120i8 >>> 2, 30i8)

@test
def binaryBitwiseRightShift07(): Bool = assertEq!(120i8 >>> 7, 0i8)

@test
def binaryBitwiseRightShift08(): Bool = assertEq!(-120i8 >>> 2, -30i8)

@test
def binaryBitwiseRightShift09(): Bool = assertEq!(12000i16 >>> 0, 12000i16)

@test
def binaryBitwiseRightShift10(): Bool = assertEq!(12000i16 >>> 2, 3000i16)

@test
def binaryBitwiseRightShift11(): Bool = assertEq!(12000i16 >>> 15, 0i16)

@test
def binaryBitwiseRightShift12(): Bool = assertEq!(-12000i16 >>> 2, -3000i16)

@test
def binaryBitwiseRightShift13(): Bool = assertEq!(120000i32 >>> 0, 120000i32)

@test
def binaryBitwiseRightShift14(): Bool = assertEq!(120000i32 >>> 2, 30000i32)

@test
def binaryBitwiseRightShift15(): Bool = assertEq!(120000i32 >>> 31, 0i32)

@test
def binaryBitwiseRightShift16(): Bool = assertEq!(-120000i32 >>> 2, -30000i32)

@test
def binaryBitwiseRightShift17(): Bool = assertEq!(12000000000i64 >>> 0, 12000000000i64)

@test
def binaryBitwiseRightShift18(): Bool = assertEq!(12000000000i64 >>> 2, 3000000000i64)

@test
def binaryBitwiseRightShift19(): Bool = assertEq!(12000000000i64 >>> 63, 0i64)

@test
def binaryBitwiseRightShift20(): Bool = assertEq!(-12000000000i64 >>> 2, -3000000000i64)

@test
def binaryBitwiseRightShift21(): Bool = assertEq!(1200000000000000000000ii >>> 0, 1200000000000000000000ii)

@test
def binaryBitwiseRightShift22(): Bool = assertEq!(1200000000000000000000ii >>> 2, 300000000000000000000ii)

@test
def binaryBitwiseRightShift23(): Bool = assertEq!(1200000000000000000000ii >>> 72, 0ii)

@test
def binaryBitwiseRightShift24(): Bool = assertEq!(-1200000000000000000000ii >>> 2, -300000000000000000000ii)