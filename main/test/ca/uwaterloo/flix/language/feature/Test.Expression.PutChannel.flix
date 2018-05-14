/////////////////////////////////////////////////////////////////////////////
// Put channel expressions                                                 //
/////////////////////////////////////////////////////////////////////////////


//Unit
def f011(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def putChannel01(): Unit =
    let ch = channel Unit 1;
    let _ = spawn f011(ch);
    <- ch

@test
def testPutChannel01(): Bool = putChannel01() `assertEq!` ()

def f021(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def f022(ch1: Channel[Unit], ch2: Channel[Unit]): Channel[Unit] =
    ch1 <- <- ch2

def putChannel02(): Unit =
    let ch1 = channel Unit 1;
    let ch2 = channel Unit 1;
    let _ = spawn f021(ch1);
    let _ = spawn f022(ch1, ch2);
    <- ch2

@test
def testPutChannel02(): Bool = putChannel01() `assertEq!` ()

def f031(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def f032(ch1: Channel[Unit], ch2: Channel[Unit]): Channel[Unit] =
    ch1 <- <- ch2

def putChannel03(): Unit =
    let ch1 = channel Unit 1;
    let ch2 = channel Unit 1;
    let ch3 = channel Unit 1;
    let _ = spawn f031(ch1);
    let _ = spawn f031(ch1);
    let _ = spawn f032(ch2, ch1);
    let _ = spawn f032(ch2, ch1);
    let _ = spawn f032(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannel03(): Bool = putChannel01() `assertEq!` ()

def f041(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def f042(ch1: Channel[Unit], ch2: Channel[Unit], ch3: Channel[Unit]): Channel[Unit] =
    ch1 <- <- ch2 <- <- ch3

@test
def testputChannel04(): Unit =
    let ch1 = channel Unit 1;
    let ch2 = channel Unit 1;
    let ch3 = channel Unit 1;
    let _ = spawn f041(ch1);
    let _ = spawn f041(ch1);
    let _ = spawn f042(ch3, ch2, ch1);
    let _ = spawn f042(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannel04(): Bool = putChannel01() `assertEq!` ()

















/*
def putChannel06(): Unit =
    let ch = channel Unit 1;
    let _ = spmUnit(ch, (), ());
    let _ = sgUnit(ch);
    <- ch


//Bool
@test
def putChannelBool01(): Bool =
    let ch = channel Bool 1;
    let _ = spBool(ch, true);
    <- ch

@test
def putChannelBool02(): Bool =
    let ch1 = channel Bool 1;
    let ch2 = channel Bool 1;
    let _ = spBool(ch1, true);
    let _ = spgBool(ch2, ch1);
    <- ch2

@test
def putChannelBool03(): Bool =
    let ch1 = channel Bool 1;
    let ch2 = channel Bool 1;
    let ch3 = channel Bool 1;
    let _ = spBool(ch1, true);
    let _ = spBool(ch1, true);
    let _ = spgBool(ch2, ch1);
    let _ = spgBool(ch2, ch1);
    let _ = spgBool(ch3, ch2);
    let _ = sgBool(ch2);
    <- ch3

@test
def putChannelBool04(): Bool =
    let ch1 = channel Bool 1;
    let ch2 = channel Bool 1;
    let ch3 = channel Bool 1;
    let _ = spBool(ch1, true);
    let _ = spBool(ch1, true);
    let _ = spBool(ch1, true);
    let _ = spBool(ch1, true);
    let _ = spgBool(ch2, ch1);
    let _ = spgBool(ch2, ch1);
    let _ = spgBool(ch2, ch1);
    let _ = spgBool(ch3, ch2);
    let _ = spgBool(ch3, ch2);
    <- ch3

@test
def putChannelBool05(): Bool =
    let ch1 = channel Bool 1;
    let ch2 = channel Bool 1;
    let ch3 = channel Bool 1;
    let _ = spBool(ch1, true);
    let _ = spBool(ch1, true);
    let _ = sptgBool(ch3, ch2, ch1);
    let _ = sptgBool(ch3, ch2, ch1);
    let _ = sgBool(ch3);
    <- ch3

@test
def putChannelBool06(): Bool =
    let ch = channel Bool 1;
    let _ = spmBool(ch, true, true);
    let _ = sgBool(ch);
    <- ch

//Int8
@test
def putChannel01(): Int8 =
    let ch = channel Int8 1;
    let _ = spI8(ch, 1i8);
    <- ch

@test
def putChannelI802(): Int8 =
    let ch1 = channel Int8 1;
    let ch2 = channel Int8 1;
    let _ = spI8(ch1, 1i8);
    let _ = spgI8(ch2, ch1);
    <- ch2

@test
def putChannelI803(): Int8 =
    let ch1 = channel Int8 1;
    let ch2 = channel Int8 1;
    let ch3 = channel Int8 1;
    let _ = spI8(ch1, 1i8);
    let _ = spI8(ch1, 1i8);
    let _ = spgI8(ch2, ch1);
    let _ = spgI8(ch2, ch1);
    let _ = spgI8(ch3, ch2);
    let _ = sgI8(ch2);
    <- ch3

@test
def putChannelI804(): Int8 =
    let ch1 = channel Int8 1;
    let ch2 = channel Int8 1;
    let ch3 = channel Int8 1;
    let _ = spI8(ch1, 1i8);
    let _ = spI8(ch1, 1i8);
    let _ = spI8(ch1, 1i8);
    let _ = spI8(ch1, 1i8);
    let _ = spgI8(ch2, ch1);
    let _ = spgI8(ch2, ch1);
    let _ = spgI8(ch2, ch1);
    let _ = spgI8(ch3, ch2);
    let _ = spgI8(ch3, ch2);
    <- ch3

@test
def putChannelI805(): Int8 =
    let ch1 = channel Int8 1;
    let ch2 = channel Int8 1;
    let ch3 = channel Int8 1;
    let _ = spI8(ch1, 1i8);
    let _ = spI8(ch1, 1i8);
    let _ = sptgI8(ch3, ch2, ch1);
    let _ = sptgI8(ch3, ch2, ch1);
    let _ = sgI8(ch3);
    <- ch3

@test
def putChannelI806(): Int8 =
    let ch = channel Int8 1;
    let _ = spmI8(ch, 1i8, 1i8);
    let _ = sgI8(ch);
    <- ch


//Int16
@test
def putChannelI1601(): Int16 =
    let ch = channel Int16 1;
    let _ = spI16(ch, 1i16);
    <- ch

@test
def putChannelI1602(): Int16 =
    let ch1 = channel Int16 1;
    let ch2 = channel Int16 1;
    let _ = spI16(ch1, 1i16);
    let _ = spgI16(ch2, ch1);
    <- ch2

@test
def putChannelI1603(): Int16 =
    let ch1 = channel Int16 1;
    let ch2 = channel Int16 1;
    let ch3 = channel Int16 1;
    let _ = spI16(ch1, 1i16);
    let _ = spI16(ch1, 1i16);
    let _ = spgI16(ch2, ch1);
    let _ = spgI16(ch2, ch1);
    let _ = spgI16(ch3, ch2);
    let _ = sgI16(ch2);
    <- ch3

@test
def putChannelI1604(): Int16 =
    let ch1 = channel Int16 1;
    let ch2 = channel Int16 1;
    let ch3 = channel Int16 1;
    let _ = spI16(ch1, 1i16);
    let _ = spI16(ch1, 1i16);
    let _ = spI16(ch1, 1i16);
    let _ = spI16(ch1, 1i16);
    let _ = spgI16(ch2, ch1);
    let _ = spgI16(ch2, ch1);
    let _ = spgI16(ch2, ch1);
    let _ = spgI16(ch3, ch2);
    let _ = spgI16(ch3, ch2);
    <- ch3

@test
def putChannelI1605(): Int16 =
    let ch1 = channel Int16 1;
    let ch2 = channel Int16 1;
    let ch3 = channel Int16 1;
    let _ = spI16(ch1, 1i16);
    let _ = spI16(ch1, 1i16);
    let _ = sptgI16(ch3, ch2, ch1);
    let _ = sptgI16(ch3, ch2, ch1);
    let _ = sgI16(ch3);
    <- ch3

@test
def putChannelI1606(): Int16 =
    let ch = channel Int16 1;
    let _ = spmI16(ch, 1i16, 1i16);
    let _ = sgI16(ch);
    <- ch


@test
def putChannelI3201(): Int32 =
    let ch = channel Int32 1;
    let _ = spI32(ch, 1i32);
    <- ch

@test
def putChannelI3202(): Int32 =
    let ch1 = channel Int32 1;
    let ch2 = channel Int32 1;
    let _ = spI32(ch1, 1i32);
    let _ = spgI32(ch2, ch1);
    <- ch2

@test
def putChannelI3203(): Int32 =
    let ch1 = channel Int32 1;
    let ch2 = channel Int32 1;
    let ch3 = channel Int32 1;
    let _ = spI32(ch1, 1i32);
    let _ = spI32(ch1, 1i32);
    let _ = spgI32(ch2, ch1);
    let _ = spgI32(ch2, ch1);
    let _ = spgI32(ch3, ch2);
    let _ = sgI32(ch2);
    <- ch3

@test
def putChannelI3204(): Int32 =
    let ch1 = channel Int32 1;
    let ch2 = channel Int32 1;
    let ch3 = channel Int32 1;
    let _ = spI32(ch1, 1i32);
    let _ = spI32(ch1, 1i32);
    let _ = spI32(ch1, 1i32);
    let _ = spI32(ch1, 1i32);
    let _ = spgI32(ch2, ch1);
    let _ = spgI32(ch2, ch1);
    let _ = spgI32(ch2, ch1);
    let _ = spgI32(ch3, ch2);
    let _ = spgI32(ch3, ch2);
    <- ch3

@test
def putChannelI3205(): Int32 =
    let ch1 = channel Int32 1;
    let ch2 = channel Int32 1;
    let ch3 = channel Int32 1;
    let _ = spI32(ch1, 1i32);
    let _ = spI32(ch1, 1i32);
    let _ = sptgI32(ch3, ch2, ch1);
    let _ = sptgI32(ch3, ch2, ch1);
    let _ = sgI32(ch3);
    <- ch3

@test
def putChannelI3206(): Int32 =
    let ch = channel Int32 1;
    let _ = spmI32(ch, 1i32, 1i32);
    let _ = sgI32(ch);
    <- ch


//Int64
@test
def putChannelI6401(): Int64 =
    let ch = channel Int64 1;
    let _ = spI64(ch, 1i64);
    <- ch

@test
def putChannelI6402(): Int64 =
    let ch1 = channel Int64 1;
    let ch2 = channel Int64 1;
    let _ = spI64(ch1, 1i64);
    let _ = spgI64(ch2, ch1);
    <- ch2

@test
def putChannelI6403(): Int64 =
    let ch1 = channel Int64 1;
    let ch2 = channel Int64 1;
    let ch3 = channel Int64 1;
    let _ = spI64(ch1, 1i64);
    let _ = spI64(ch1, 1i64);
    let _ = spgI64(ch2, ch1);
    let _ = spgI64(ch2, ch1);
    let _ = spgI64(ch3, ch2);
    let _ = sgI64(ch2);
    <- ch3

@test
def putChannelI6404(): Int64 =
    let ch1 = channel Int64 1;
    let ch2 = channel Int64 1;
    let ch3 = channel Int64 1;
    let _ = spI64(ch1, 1i64);
    let _ = spI64(ch1, 1i64);
    let _ = spI64(ch1, 1i64);
    let _ = spI64(ch1, 1i64);
    let _ = spgI64(ch2, ch1);
    let _ = spgI64(ch2, ch1);
    let _ = spgI64(ch2, ch1);
    let _ = spgI64(ch3, ch2);
    let _ = spgI64(ch3, ch2);
    <- ch3

@test
def putChannelI6405(): Int64 =
    let ch1 = channel Int64 1;
    let ch2 = channel Int64 1;
    let ch3 = channel Int64 1;
    let _ = spI64(ch1, 1i64);
    let _ = spI64(ch1, 1i64);
    let _ = sptgI64(ch3, ch2, ch1);
    let _ = sptgI64(ch3, ch2, ch1);
    let _ = sgI64(ch3);
    <- ch3

@test
def putChannelI6406(): Int64 =
    let ch = channel Int64 1;
    let _ = spmI64(ch, 1i64, 1i64);
    let _ = sgI64(ch);
    <- ch


//BigInt
@test
def putChannelBI01(): BigInt =
    let ch = channel BigInt 1;
    let _ = spBI(ch, 1ii);
    <- ch

@test
def putChannelBI02(): BigInt =
    let ch1 = channel BigInt 1;
    let ch2 = channel BigInt 1;
    let _ = spBI(ch1, 1ii);
    let _ = spgBI(ch2, ch1);
    <- ch2

@test
def putChannelBI03(): BigInt =
    let ch1 = channel BigInt 1;
    let ch2 = channel BigInt 1;
    let ch3 = channel BigInt 1;
    let _ = spBI(ch1, 1ii);
    let _ = spBI(ch1, 1ii);
    let _ = spgBI(ch2, ch1);
    let _ = spgBI(ch2, ch1);
    let _ = spgBI(ch3, ch2);
    let _ = sgBI(ch2);
    <- ch3

@test
def putChannelBI04(): BigInt =
    let ch1 = channel BigInt 1;
    let ch2 = channel BigInt 1;
    let ch3 = channel BigInt 1;
    let _ = spBI(ch1, 1ii);
    let _ = spBI(ch1, 1ii);
    let _ = spBI(ch1, 1ii);
    let _ = spBI(ch1, 1ii);
    let _ = spgBI(ch2, ch1);
    let _ = spgBI(ch2, ch1);
    let _ = spgBI(ch2, ch1);
    let _ = spgBI(ch3, ch2);
    let _ = spgBI(ch3, ch2);
    <- ch3

@test
def putChannelBI05(): BigInt =
    let ch1 = channel BigInt 1;
    let ch2 = channel BigInt 1;
    let ch3 = channel BigInt 1;
    let _ = spBI(ch1, 1ii);
    let _ = spBI(ch1, 1ii);
    let _ = sptgBI(ch3, ch2, ch1);
    let _ = sptgBI(ch3, ch2, ch1);
    let _ = sgBI(ch3);
    <- ch3

@test
def putChannelBI06(): BigInt =
    let ch = channel BigInt 1;
    let _ = spmBI(ch, 1ii, 1ii);
    let _ = sgBI(ch);
    <- ch

//Char
@test
def putChannelChar01(): Char =
    let ch = channel Char 1;
    let _ = spChar(ch, 'a');
    <- ch

@test
def putChannelChar02(): Char =
    let ch1 = channel Char 1;
    let ch2 = channel Char 1;
    let _ = spChar(ch1, 'a');
    let _ = spgChar(ch2, ch1);
    <- ch2

@test
def putChannelChar03(): Char =
    let ch1 = channel Char 1;
    let ch2 = channel Char 1;
    let ch3 = channel Char 1;
    let _ = spChar(ch1, 'a');
    let _ = spChar(ch1, 'a');
    let _ = spgChar(ch2, ch1);
    let _ = spgChar(ch2, ch1);
    let _ = spgChar(ch3, ch2);
    let _ = sgChar(ch2);
    <- ch3

@test
def putChannelChar04(): Char =
    let ch1 = channel Char 1;
    let ch2 = channel Char 1;
    let ch3 = channel Char 1;
    let _ = spChar(ch1, 'a');
    let _ = spChar(ch1, 'a');
    let _ = spChar(ch1, 'a');
    let _ = spChar(ch1, 'a');
    let _ = spgChar(ch2, ch1);
    let _ = spgChar(ch2, ch1);
    let _ = spgChar(ch2, ch1);
    let _ = spgChar(ch3, ch2);
    let _ = spgChar(ch3, ch2);
    <- ch3

@test
def putChannelChar05(): Char =
    let ch1 = channel Char 1;
    let ch2 = channel Char 1;
    let ch3 = channel Char 1;
    let _ = spChar(ch1, 'a');
    let _ = spChar(ch1, 'a');
    let _ = sptgChar(ch3, ch2, ch1);
    let _ = sptgChar(ch3, ch2, ch1);
    let _ = sgChar(ch3);
    <- ch3

@test
def putChannelChar06(): Char =
    let ch = channel Char 1;
    let _ = spmChar(ch, 'a', 'a');
    let _ = sgChar(ch);
    <- ch


//Float32
@test
def putChannelF3201(): Float32 =
    let ch = channel Float32 1;
    let _ = spF32(ch, 1.0f32);
    <- ch

@test
def putChannelF3202(): Float32 =
    let ch1 = channel Float32 1;
    let ch2 = channel Float32 1;
    let _ = spF32(ch1, 1.0f32);
    let _ = spgF32(ch2, ch1);
    <- ch2

@test
def putChannelF3203(): Float32 =
    let ch1 = channel Float32 1;
    let ch2 = channel Float32 1;
    let ch3 = channel Float32 1;
    let _ = spF32(ch1, 1.0f32);
    let _ = spF32(ch1, 1.0f32);
    let _ = spgF32(ch2, ch1);
    let _ = spgF32(ch2, ch1);
    let _ = spgF32(ch3, ch2);
    let _ = sgF32(ch2);
    <- ch3

@test
def putChannelF3204(): Float32 =
    let ch1 = channel Float32 1;
    let ch2 = channel Float32 1;
    let ch3 = channel Float32 1;
    let _ = spF32(ch1, 1.0f32);
    let _ = spF32(ch1, 1.0f32);
    let _ = spF32(ch1, 1.0f32);
    let _ = spF32(ch1, 1.0f32);
    let _ = spgF32(ch2, ch1);
    let _ = spgF32(ch2, ch1);
    let _ = spgF32(ch2, ch1);
    let _ = spgF32(ch3, ch2);
    let _ = spgF32(ch3, ch2);
    <- ch3

@test
def putChannelF3205(): Float32 =
    let ch1 = channel Float32 1;
    let ch2 = channel Float32 1;
    let ch3 = channel Float32 1;
    let _ = spF32(ch1, 1.0f32);
    let _ = spF32(ch1, 1.0f32);
    let _ = sptgF32(ch3, ch2, ch1);
    let _ = sptgF32(ch3, ch2, ch1);
    let _ = sgF32(ch3);
    <- ch3

@test
def putChannelF3206(): Float32 =
    let ch = channel Float32 1;
    let _ = spmF32(ch, 1.0f32, 1.0f32);
    let _ = sgF32(ch);
    <- ch


//Float64
@test
def putChannelF6401(): Float64 =
    let ch = channel Float64 1;
    let _ = spF64(ch, 1.0f64);
    <- ch

@test
def putChannelF6402(): Float64 =
    let ch1 = channel Float64 1;
    let ch2 = channel Float64 1;
    let _ = spF64(ch1, 1.0f64);
    let _ = spgF64(ch2, ch1);
    <- ch2

@test
def putChannelF6403(): Float64 =
    let ch1 = channel Float64 1;
    let ch2 = channel Float64 1;
    let ch3 = channel Float64 1;
    let _ = spF64(ch1, 1.0f64);
    let _ = spF64(ch1, 1.0f64);
    let _ = spgF64(ch2, ch1);
    let _ = spgF64(ch2, ch1);
    let _ = spgF64(ch3, ch2);
    let _ = sgF64(ch2);
    <- ch3

@test
def putChannelF6404(): Float64 =
    let ch1 = channel Float64 1;
    let ch2 = channel Float64 1;
    let ch3 = channel Float64 1;
    let _ = spF64(ch1, 1.0f64);
    let _ = spF64(ch1, 1.0f64);
    let _ = spF64(ch1, 1.0f64);
    let _ = spF64(ch1, 1.0f64);
    let _ = spgF64(ch2, ch1);
    let _ = spgF64(ch2, ch1);
    let _ = spgF64(ch2, ch1);
    let _ = spgF64(ch3, ch2);
    let _ = spgF64(ch3, ch2);
    <- ch3

@test
def putChannelF6405(): Float64 =
    let ch1 = channel Float64 1;
    let ch2 = channel Float64 1;
    let ch3 = channel Float64 1;
    let _ = spF64(ch1, 1.0f64);
    let _ = spF64(ch1, 1.0f64);
    let _ = sptgF64(ch3, ch2, ch1);
    let _ = sptgF64(ch3, ch2, ch1);
    let _ = sgF64(ch3);
    <- ch3

@test
def putChannelF6406(): Float64 =
    let ch = channel Float64 1;
    let _ = spmF64(ch, 1.0f64, 1.0f64);
    let _ = sgF64(ch);
    <- ch


//Str
@test
def putChannelStr01(): Str =
    let ch = channel Str 1;
    let _ = spStr(ch, "a");
    <- ch

@test
def putChannelStr02(): Str =
    let ch1 = channel Str 1;
    let ch2 = channel Str 1;
    let _ = spStr(ch1, "a");
    let _ = spgStr(ch2, ch1);
    <- ch2

@test
def putChannelStr03(): Str =
    let ch1 = channel Str 1;
    let ch2 = channel Str 1;
    let ch3 = channel Str 1;
    let _ = spStr(ch1, "a");
    let _ = spStr(ch1, "a");
    let _ = spgStr(ch2, ch1);
    let _ = spgStr(ch2, ch1);
    let _ = spgStr(ch3, ch2);
    let _ = sgStr(ch2);
    <- ch3

@test
def putChannelStr04(): Str =
    let ch1 = channel Str 1;
    let ch2 = channel Str 1;
    let ch3 = channel Str 1;
    let _ = spStr(ch1, "a");
    let _ = spStr(ch1, "a");
    let _ = spStr(ch1, "a");
    let _ = spStr(ch1, "a");
    let _ = spgStr(ch2, ch1);
    let _ = spgStr(ch2, ch1);
    let _ = spgStr(ch2, ch1);
    let _ = spgStr(ch3, ch2);
    let _ = spgStr(ch3, ch2);
    <- ch3

@test
def putChannelStr05(): Str =
    let ch1 = channel Str 1;
    let ch2 = channel Str 1;
    let ch3 = channel Str 1;
    let _ = spStr(ch1, "a");
    let _ = spStr(ch1, "a");
    let _ = sptgStr(ch3, ch2, ch1);
    let _ = sptgStr(ch3, ch2, ch1);
    let _ = sgStr(ch3);
    <- ch3

@test
def putChannelStr06(): Str =
    let ch = channel Str 1;
    let _ = spmStr(ch, "a", "a");
    let _ = sgStr(ch);
    <- ch


//Channel Int
@test
def putChannelChInt01(): Channel[Int] =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Int 1;
    let _ = spChInt(ch1, ch2);
    <- ch1

@test
def putChannelChInt02(): Channel[Int] =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Channel[Int] 1;
    let ch3 = channel Int 1;
    let _ = spChInt(ch1, ch3);
    let _ = spgChInt(ch2, ch1);
    <- ch2

@test
def putChannelChInt03(): Channel[Int] =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Channel[Int] 1;
    let ch3 = channel Channel[Int] 1;
    let ch4 = channel Int 1;
    let ch5 = channel Int 1;
    let _ = spChInt(ch1, ch4);
    let _ = spChInt(ch1, ch5);
    let _ = spgChInt(ch2, ch1);
    let _ = spgChInt(ch2, ch1);
    let _ = spgChInt(ch3, ch2);
    let _ = sgChInt(ch2);
    <- ch3

@test
def putChannelChInt04(): Channel[Int] =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Channel[Int] 1;
    let ch3 = channel Channel[Int] 1;
    let ch4 = channel Int 1;
    let ch5 = channel Int 1;
    let ch6 = channel Int 1;
    let ch7 = channel Int 1;
    let _ = spChInt(ch1, ch4);
    let _ = spChInt(ch1, ch5);
    let _ = spChInt(ch1, ch6);
    let _ = spChInt(ch1, ch7);
    let _ = spgChInt(ch2, ch1);
    let _ = spgChInt(ch2, ch1);
    let _ = spgChInt(ch2, ch1);
    let _ = spgChInt(ch3, ch2);
    let _ = spgChInt(ch3, ch2);
    <- ch3

@test
def putChannelChInt05(): Channel[Int] =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Channel[Int] 1;
    let ch3 = channel Channel[Int] 1;
    let ch4 = channel Int 1;
    let ch5 = channel Int 1;
    let _ = spChInt(ch1, ch4);
    let _ = spChInt(ch1, ch5);
    let _ = sptgChInt(ch3, ch2, ch1);
    let _ = sptgChInt(ch3, ch2, ch1);
    let _ = sgChInt(ch3);
    <- ch3

@test
def putChannelChInt06(): Channel[Int] =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Int 1;
    let ch3 = channel Int 1;
    let _ = spmChInt(ch1, ch2, ch3);
    let _ = sgChInt(ch1);
    <- ch1


//List
@test
def putChannelList01(): List[Int] =
    let ch = channel List[Int] 1;
    let _ = spList(ch, 1 :: Nil);
    <- ch

@test
def putChannelList02(): List[Int] =
    let ch1 = channel List[Int] 1;
    let ch2 = channel List[Int] 1;
    let _ = spList(ch1, 1 :: Nil);
    let _ = spgList(ch2, ch1);
    <- ch2

@test
def putChannelList03(): List[Int] =
    let ch1 = channel List[Int] 1;
    let ch2 = channel List[Int] 1;
    let ch3 = channel List[Int] 1;
    let _ = spList(ch1, 1 :: Nil);
    let _ = spList(ch1, 1 :: Nil);
    let _ = spgList(ch2, ch1);
    let _ = spgList(ch2, ch1);
    let _ = spgList(ch3, ch2);
    let _ = sgList(ch2);
    <- ch3

@test
def putChannelList04(): List[Int] =
    let ch1 = channel List[Int] 1;
    let ch2 = channel List[Int] 1;
    let ch3 = channel List[Int] 1;
    let _ = spList(ch1, 1 :: Nil);
    let _ = spList(ch1, 1 :: Nil);
    let _ = spList(ch1, 1 :: Nil);
    let _ = spList(ch1, 1 :: Nil);
    let _ = spgList(ch2, ch1);
    let _ = spgList(ch2, ch1);
    let _ = spgList(ch2, ch1);
    let _ = spgList(ch3, ch2);
    let _ = spgList(ch3, ch2);
    <- ch3

@test
def putChannelList05(): List[Int] =
    let ch1 = channel List[Int] 1;
    let ch2 = channel List[Int] 1;
    let ch3 = channel List[Int] 1;
    let _ = spList(ch1, 1 :: Nil);
    let _ = spList(ch1, 1 :: Nil);
    let _ = sptgList(ch3, ch2, ch1);
    let _ = sptgList(ch3, ch2, ch1);
    let _ = sgList(ch3);
    <- ch3

@test
def putChannelList06(): List[Int] =
    let ch = channel List[Int] 1;
    let _ = spmList(ch, 1 :: Nil, 1 :: Nil);
    let _ = sgList(ch);
    <- ch

@test
def putChannel02(): Channel[Int8] =
    let ch = channel Int8 1;
    let _ = sp(ch, 2i8);
    <- ch


@test
def putChannel03(): Channel[Int16] =
    let ch = channel Int16 1;
    let _ = ch <- 2i16;
    ch

@test
def putChannel04(): Channel[Int64] = let ch = channel Int64 1; let _ = ch <- 2i64; ch

@test
def putChannel05(): Channel[BigInt] = let ch = channel BigInt 1; let _ = ch <- 2ii; ch

@test
def putChannel06(): Channel[Float32] = let ch = channel Float32 1; let _ = ch <- 2.0f32; ch

@test
def putChannel07(): Channel[Float64] = let ch = channel Float64 1; let _ = ch <- 2.0f64; ch

@test
def putChannel08(): Channel[Str] = let ch = channel Str 1; let _ = ch <- "asdf"; ch

@test
def putChannel09(): Channel[Char] = let ch = channel Char 1; let _ = ch <- 'c'; ch

@test
def putChannel10(): Channel[Bool] = let ch = channel Bool 1; let _ = ch <- true; ch

@test
def putChannel11(): Channel[Unit] = let ch = channel Unit 1; let _ = ch <- (); ch

@test
def putChannel12(): Channel[Int] = let ch =
    channel Int 1;
    let _ = ch <- 2;
    let _ = ch <- 3;
    let _ = ch <- 3;
    let _ = ch <- 5; ch

@test
def putChannel13(): Channel[Int] =
    let ch = channel Int 1;
    ch <- 1 <- 2 <- 3

@test
def putChannel14(): Unit =
    let ch = channel Int 1;
    let _ = ch <- 1 <- 2 <- 3;
    ()

@test
def putChannel15(): Channel[Str] =
    let ch = channel Str 1;
    ch <- "a" <- "b" <- "c"

@test
def putChannel16(): Unit =
    let ch = channel Str 1;
    let _ = ch <- "a" <- "b" <- "c";
    ()

@test
def putChannel17(): Channel[Unit] =
    let ch = channel Unit 1;
    ch <- () <- () <- ()

@test
def putChannel18(): Unit =
    let ch = channel Unit 1;
    let _ = ch <- () <- () <- ();
    ()

@test
def putChannel19(): Channel[Channel[Int]] =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Int 1;
    let ch3 = channel Int 1;
    ch1 <- ch2 <- ch3

@test
def putChannel20(): Unit =
    let ch1 = channel Channel[Int] 1;
    let ch2 = channel Int 1;
    let ch3 = channel Int 1;
    let _ = ch1 <- ch2 <- ch3;
    ()
/*

@test
def putChannel21(): Channel[Int] =
    let ch1 = channel Int 1;
    let ch2 = channel Int 1;
    ch1 <- <- ch2

@test
def putChannel22(): Unit =
    let ch1 = channel Int 1;
    let ch2 = channel Int 1;
    let _ = ch1 <- <- ch2;
    ()

// Int8
def spI8(to: Channel[Int8], v: Int8): Unit =
    spawn putI8(to, v)

def sgI8(from: Channel[Int8]): Unit =
    spawn getI8(from)

def spgI8(to: Channel[Int8], from: Channel[Int8]): Unit =
    spawn putGetI8(to, from)

def sptgI8(to: Channel[Int8], through: Channel[Int8], from: Channel[Int8]): Unit =
    spawn putThroughGetI8(to, through, from)

def spmI8(to: Channel[Int8], v1: Int8, v2: Int8): Unit =
    spawn putMultipleI8(to, v1, v2)


def putI8(to: Channel[Int8], v: Int8): Channel[Int8] =
    to <- v

def getI8(from: Channel[Int8]): Int8 =
    <- from

def putGetI8(to: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- from

def putThroughGetI8(to: Channel[Int8], through: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- through <- <- from

def putMultipleI8(to: Channel[Int8], v1: Int8, v2: Int8): Channel[Int8] =
    to <- v1 <- v2

def putThroughGet(to: Channel[Int8], through: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- through <- <- from

def putMultiple(to: Channel[Int8], v1: Int8, v2: Int8): Channel[Int8] =
    to <- v1 <- v2


// Int16
def spI16(to: Channel[Int16], v: Int16): Unit =
    spawn putI16(to, v)

def sgI16(from: Channel[Int16]): Unit =
    spawn getI16(from)

def spgI16(to: Channel[Int16], from: Channel[Int16]): Unit =
    spawn putGetI16(to, from)

def sptgI16(to: Channel[Int16], through: Channel[Int16], from: Channel[Int16]): Unit =
    spawn putThroughGetI16(to, through, from)

def spmI16(to: Channel[Int16], v1: Int16, v2: Int16): Unit =
    spawn putMultipleI16(to, v1, v2)


def putI16(to: Channel[Int16], v: Int16): Channel[Int16] =
    to <- v

def getI16(from: Channel[Int16]): Int16 =
    <- from

def putGetI16(to: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- from

def putThroughGetI16(to: Channel[Int16], through: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- through <- <- from

def putMultipleI16(to: Channel[Int16], v1: Int16, v2: Int16): Channel[Int16] =
    to <- v1 <- v2


// Int32
def spI32(to: Channel[Int32], v: Int32): Unit =
    spawn putI32(to, v)

def sgI32(from: Channel[Int32]): Unit =
    spawn getI32(from)

def spgI32(to: Channel[Int32], from: Channel[Int32]): Unit =
    spawn putGetI32(to, from)

def sptgI32(to: Channel[Int32], through: Channel[Int32], from: Channel[Int32]): Unit =
    spawn putThroughGetI32(to, through, from)

def spmI32(to: Channel[Int32], v1: Int32, v2: Int32): Unit =
    spawn putMultipleI32(to, v1, v2)


def putI32(to: Channel[Int32], v: Int32): Channel[Int32] =
    to <- v

def getI32(from: Channel[Int32]): Int32 =
    <- from

def putGetI32(to: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- from

def putThroughGetI32(to: Channel[Int32], through: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- through <- <- from

def putMultipleI32(to: Channel[Int32], v1: Int32, v2: Int32): Channel[Int32] =
    to <- v1 <- v2


// Int64
def spI64(to: Channel[Int64], v: Int64): Unit =
    spawn putI64(to, v)

def sgI64(from: Channel[Int64]): Unit =
    spawn getI64(from)

def spgI64(to: Channel[Int64], from: Channel[Int64]): Unit =
    spawn putGetI64(to, from)

def sptgI64(to: Channel[Int64], through: Channel[Int64], from: Channel[Int64]): Unit =
    spawn putThroughGetI64(to, through, from)

def spmI64(to: Channel[Int64], v1: Int64, v2: Int64): Unit =
    spawn putMultipleI64(to, v1, v2)


def putI64(to: Channel[Int64], v: Int64): Channel[Int64] =
    to <- v

def getI64(from: Channel[Int64]): Int64 =
    <- from

def putGetI64(to: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- from

def putThroughGetI64(to: Channel[Int64], through: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- through <- <- from

def putMultipleI64(to: Channel[Int64], v1: Int64, v2: Int64): Channel[Int64] =
    to <- v1 <- v2


// BigInt
def spBI(to: Channel[BigInt], v: BigInt): Unit =
    spawn putBI(to, v)

def sgBI(from: Channel[BigInt]): Unit =
    spawn getBI(from)

def spgBI(to: Channel[BigInt], from: Channel[BigInt]): Unit =
    spawn putGetBI(to, from)

def sptgBI(to: Channel[BigInt], through: Channel[BigInt], from: Channel[BigInt]): Unit =
    spawn putThroughGetBI(to, through, from)

def spmBI(to: Channel[BigInt], v1: BigInt, v2: BigInt): Unit =
    spawn putMultipleBI(to, v1, v2)


def putBI(to: Channel[BigInt], v: BigInt): Channel[BigInt] =
    to <- v

def getBI(from: Channel[BigInt]): BigInt =
    <- from

def putGetBI(to: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- from

def putThroughGetBI(to: Channel[BigInt], through: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- through <- <- from

def putMultipleBI(to: Channel[BigInt], v1: BigInt, v2: BigInt): Channel[BigInt] =
    to <- v1 <- v2


// Unit
def spUnit(to: Channel[Unit], v: Unit): Unit =
    spawn putUnit(to, v)

def sgUnit(from: Channel[Unit]): Unit =
    spawn getUnit(from)

def spgUnit(to: Channel[Unit], from: Channel[Unit]): Unit =
    spawn putGetUnit(to, from)

def sptgUnit(to: Channel[Unit], through: Channel[Unit], from: Channel[Unit]): Unit =
    spawn putThroughGetUnit(to, through, from)

def spmUnit(to: Channel[Unit], v1: Unit, v2: Unit): Unit =
    spawn putMultipleUnit(to, v1, v2)


def putUnit(to: Channel[Unit], v: Unit): Channel[Unit] =
    to <- v

def getUnit(from: Channel[Unit]): Unit =
    <- from

def putGetUnit(to: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- from

def putThroughGetUnit(to: Channel[Unit], through: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- through <- <- from

def putMultipleUnit(to: Channel[Unit], v1: Unit, v2: Unit): Channel[Unit] =
    to <- v1 <- v2


// Bool
def spBool(to: Channel[Bool], v: Bool): Unit =
    spawn putBool(to, v)

def sgBool(from: Channel[Bool]): Unit =
    spawn getBool(from)

def spgBool(to: Channel[Bool], from: Channel[Bool]): Unit =
    spawn putGetBool(to, from)

def sptgBool(to: Channel[Bool], through: Channel[Bool], from: Channel[Bool]): Unit =
    spawn putThroughGetBool(to, through, from)

def spmBool(to: Channel[Bool], v1: Bool, v2: Bool): Unit =
    spawn putMultipleBool(to, v1, v2)


def putBool(to: Channel[Bool], v: Bool): Channel[Bool] =
    to <- v

def getBool(from: Channel[Bool]): Bool =
    <- from

def putGetBool(to: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- from

def putThroughGetBool(to: Channel[Bool], through: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- through <- <- from

def putMultipleBool(to: Channel[Bool], v1: Bool, v2: Bool): Channel[Bool] =
    to <- v1 <- v2


// Char
def spChar(to: Channel[Char], v: Char): Unit =
    spawn putChar(to, v)

def sgChar(from: Channel[Char]): Unit =
    spawn getChar(from)

def spgChar(to: Channel[Char], from: Channel[Char]): Unit =
    spawn putGetChar(to, from)

def sptgChar(to: Channel[Char], through: Channel[Char], from: Channel[Char]): Unit =
    spawn putThroughGetChar(to, through, from)

def spmChar(to: Channel[Char], v1: Char, v2: Char): Unit =
    spawn putMultipleChar(to, v1, v2)


def putChar(to: Channel[Char], v: Char): Channel[Char] =
    to <- v

def getChar(from: Channel[Char]): Char =
    <- from

def putGetChar(to: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- from

def putThroughGetChar(to: Channel[Char], through: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- through <- <- from

def putMultipleChar(to: Channel[Char], v1: Char, v2: Char): Channel[Char] =
    to <- v1 <- v2


//Float32
def spF32(to: Channel[Float32], v: Float32): Unit =
    spawn putF32(to, v)

def sgF32(from: Channel[Float32]): Unit =
    spawn getF32(from)

def spgF32(to: Channel[Float32], from: Channel[Float32]): Unit =
    spawn putGetF32(to, from)

def sptgF32(to: Channel[Float32], through: Channel[Float32], from: Channel[Float32]): Unit =
    spawn putThroughGetF32(to, through, from)

def spmF32(to: Channel[Float32], v1: Float32, v2: Float32): Unit =
    spawn putMultipleF32(to, v1, v2)


def putF32(to: Channel[Float32], v: Float32): Channel[Float32] =
    to <- v

def getF32(from: Channel[Float32]): Float32 =
    <- from

def putGetF32(to: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- from

def putThroughGetF32(to: Channel[Float32], through: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- through <- <- from

def putMultipleF32(to: Channel[Float32], v1: Float32, v2: Float32): Channel[Float32] =
    to <- v1 <- v2


//Float64
def spF64(to: Channel[Float64], v: Float64): Unit =
    spawn putF64(to, v)

def sgF64(from: Channel[Float64]): Unit =
    spawn getF64(from)

def spgF64(to: Channel[Float64], from: Channel[Float64]): Unit =
    spawn putGetF64(to, from)

def sptgF64(to: Channel[Float64], through: Channel[Float64], from: Channel[Float64]): Unit =
    spawn putThroughGetF64(to, through, from)

def spmF64(to: Channel[Float64], v1: Float64, v2: Float64): Unit =
    spawn putMultipleF64(to, v1, v2)


def putF64(to: Channel[Float64], v: Float64): Channel[Float64] =
    to <- v

def getF64(from: Channel[Float64]): Float64 =
    <- from

def putGetF64(to: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- from

def putThroughGetF64(to: Channel[Float64], through: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- through <- <- from

def putMultipleF64(to: Channel[Float64], v1: Float64, v2: Float64): Channel[Float64] =
    to <- v1 <- v2


//Str
def spStr(to: Channel[Str], v: Str): Unit =
    spawn putStr(to, v)

def sgStr(from: Channel[Str]): Unit =
    spawn getStr(from)

def spgStr(to: Channel[Str], from: Channel[Str]): Unit =
    spawn putGetStr(to, from)

def sptgStr(to: Channel[Str], through: Channel[Str], from: Channel[Str]): Unit =
    spawn putThroughGetStr(to, through, from)

def spmStr(to: Channel[Str], v1: Str, v2: Str): Unit =
    spawn putMultipleStr(to, v1, v2)


def putStr(to: Channel[Str], v: Str): Channel[Str] =
    to <- v

def getStr(from: Channel[Str]): Str =
    <- from

def putGetStr(to: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- from

def putThroughGetStr(to: Channel[Str], through: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- through <- <- from

def putMultipleStr(to: Channel[Str], v1: Str, v2: Str): Channel[Str] =
    to <- v1 <- v2


//Channel Int
def spChInt(to: Channel[Channel[Int]], v: Channel[Int]): Unit =
    spawn putChInt(to, v)

def sgChInt(from: Channel[Channel[Int]]): Unit =
    spawn getChInt(from)

def spgChInt(to: Channel[Channel[Int]], from: Channel[Channel[Int]]): Unit =
    spawn putGetChInt(to, from)

def sptgChInt(to: Channel[Channel[Int]], through: Channel[Channel[Int]], from: Channel[Channel[Int]]): Unit =
    spawn putThroughGetChInt(to, through, from)

def spmChInt(to: Channel[Channel[Int]], v1: Channel[Int], v2: Channel[Int]): Unit =
    spawn putMultipleChInt(to, v1, v2)


def putChInt(to: Channel[Channel[Int]], v: Channel[Int]): Channel[Channel[Int]] =
    to <- v

def getChInt(from: Channel[Channel[Int]]): Channel[Int] =
    <- from

def putGetChInt(to: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- from

def putThroughGetChInt(to: Channel[Channel[Int]], through: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- through <- <- from

def putMultipleChInt(to: Channel[Channel[Int]], v1: Channel[Int], v2: Channel[Int]): Channel[Channel[Int]] =
    to <- v1 <- v2


//List
def spList(to: Channel[List[Int]], v: List[Int]): Unit =
    spawn putList(to, v)

def sgList(from: Channel[List[Int]]): Unit =
    spawn getList(from)

def spgList(to: Channel[List[Int]], from: Channel[List[Int]]): Unit =
    spawn putGetList(to, from)

def sptgList(to: Channel[List[Int]], through: Channel[List[Int]], from: Channel[List[Int]]): Unit =
    spawn putThroughGetList(to, through, from)

def spmList(to: Channel[List[Int]], v1: List[Int], v2: List[Int]): Unit =
    spawn putMultipleList(to, v1, v2)


def putList(to: Channel[List[Int]], v: List[Int]): Channel[List[Int]] =
    to <- v

def getList(from: Channel[List[Int]]): List[Int] =
    <- from

def putGetList(to: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- from

def putThroughGetList(to: Channel[List[Int]], through: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- through <- <- from

def putMultipleList(to: Channel[List[Int]], v1: List[Int], v2: List[Int]): Channel[List[Int]] =
    to <- v1 <- v2
*/
