/////////////////////////////////////////////////////////////////////////////
// Put channel expressions                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def putChannelI801(): Int8 =
    let ch = channel Int8;
    let _ = spI8(ch, 1i8);
    <- ch

@test
def putChannelI802(): Int8 =
    let ch1 = channel Int8;
    let ch2 = channel Int8;
    let _ = spI8(ch1, 1i8);
    let _ = spgI8(ch2, ch1);
    <- ch2

@test
def putChannelI803(): Int8 =
    let ch1 = channel Int8;
    let ch2 = channel Int8;
    let ch3 = channel Int8;
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
    let ch1 = channel Int8 0;
    let ch2 = channel Int8 1;
    let ch3 = channel Int8 0;
    let _ = spI8(ch1, 1i8);
    let _ = spI8(ch1, 1i8);
    let _ = sptgI8(ch3, ch2, ch1);
    let _ = sptgI8(ch3, ch2, ch1);
    let _ = sgI8(ch3);
    <- ch3

@test
def putChannelI806(): Int8 =
    let ch = channel Int8;
    let _ = spmI8(ch, 1i8, 1i8);
    let _ = sgI8(ch);
    <- ch
/*
@test
def putChannelI1601(): Int16 =
    let ch = channel Int16;
    let _ = spI16(ch, 1i16);
    <- ch

@test
def putChannelI1602(): Int16 =
    let ch1 = channel Int16;
    let ch2 = channel Int16;
    let _ = spI16(ch1, 1i16);
    let _ = spgI16(ch2, ch1);
    <- ch2

@test
def putChannelI1603(): Int16 =
    let ch1 = channel Int16;
    let ch2 = channel Int16;
    let ch3 = channel Int16;
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
def putChannelI3201(): Int32 =
    let ch = channel Int32;
    let _ = spI32(ch, 1i32);
    <- ch

@test
def putChannelI3202(): Int32 =
    let ch1 = channel Int32;
    let ch2 = channel Int32;
    let _ = spI32(ch1, 1i32);
    let _ = spgI32(ch2, ch1);
    <- ch2

@test
def putChannelI3203(): Int32 =
    let ch1 = channel Int32;
    let ch2 = channel Int32;
    let ch3 = channel Int32;
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
def putChannelI6401(): Int64 =
    let ch = channel Int64;
    let _ = spI64(ch, 1i64);
    <- ch

@test
def putChannelI6402(): Int64 =
    let ch1 = channel Int64;
    let ch2 = channel Int64;
    let _ = spI64(ch1, 1i64);
    let _ = spgI64(ch2, ch1);
    <- ch2

@test
def putChannelI6403(): Int64 =
    let ch1 = channel Int64;
    let ch2 = channel Int64;
    let ch3 = channel Int64;
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
def putChannelIBI01(): BigInt =
    let ch = channel BigInt;
    let _ = spBI(ch, 1ii);
    <- ch

@test
def putChannelIBI02(): BigInt =
    let ch1 = channel BigInt;
    let ch2 = channel BigInt;
    let _ = spBI(ch1, 1ii);
    let _ = spgBI(ch2, ch1);
    <- ch2

@test
def putChannelIBI03(): BigInt =
    let ch1 = channel BigInt;
    let ch2 = channel BigInt;
    let ch3 = channel BigInt;
    let _ = spBI(ch1, 1ii);
    let _ = spBI(ch1, 1ii);
    let _ = spgBI(ch2, ch1);
    let _ = spgBI(ch2, ch1);
    let _ = spgBI(ch3, ch2);
    let _ = sgBI(ch2);
    <- ch3

@test
def putChannelIBI04(): BigInt =
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
def putChannelUnit01(): Unit =
    let ch = channel Unit;
    let _ = spUnit(ch, ());
    <- ch

@test
def putChannelIUnit02(): Unit =
    let ch1 = channel Unit;
    let ch2 = channel Unit;
    let _ = spUnit(ch1, ());
    let _ = spgUnit(ch2, ch1);
    <- ch2

@test
def putChannelIUnit03(): Unit =
    let ch1 = channel Unit;
    let ch2 = channel Unit;
    let ch3 = channel Unit;
    let _ = spUnit(ch1, ());
    let _ = spUnit(ch1, ());
    let _ = spgUnit(ch2, ch1);
    let _ = spgUnit(ch2, ch1);
    let _ = spgUnit(ch3, ch2);
    let _ = sgUnit(ch2);
    <- ch3

@test
def putChannelIUnit04(): Unit =
    let ch1 = channel Unit 1;
    let ch2 = channel Unit 1;
    let ch3 = channel Unit 1;
    let _ = spUnit(ch1, ());
    let _ = spUnit(ch1, ());
    let _ = spUnit(ch1, ());
    let _ = spUnit(ch1, ());
    let _ = spgUnit(ch2, ch1);
    let _ = spgUnit(ch2, ch1);
    let _ = spgUnit(ch2, ch1);
    let _ = spgUnit(ch3, ch2);
    let _ = spgUnit(ch3, ch2);
    <- ch3

@test
def putChannelBool01(): Bool =
    let ch = channel Bool;
    let _ = spBool(ch, true);
    <- ch

@test
def putChannelBool02(): Bool =
    let ch1 = channel Bool;
    let ch2 = channel Bool;
    let _ = spBool(ch1, true);
    let _ = spgBool(ch2, ch1);
    <- ch2

@test
def putChannelBool03(): Bool =
    let ch1 = channel Bool;
    let ch2 = channel Bool;
    let ch3 = channel Bool;
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
*/
/*
@test
def putChannel02(): Channel[Int8] =
    let ch = channel Int8;
    let _ = sp(ch, 2i8);
    <- ch


@test
def putChannel03(): Channel[Int16] =
    let ch = channel Int16;
    let _ = ch <- 2i16;
    ch

@test
def putChannel04(): Channel[Int64] = let ch = channel Int64; let _ = ch <- 2i64; ch

@test
def putChannel05(): Channel[BigInt] = let ch = channel BigInt; let _ = ch <- 2ii; ch

@test
def putChannel06(): Channel[Float32] = let ch = channel Float32; let _ = ch <- 2.0f32; ch

@test
def putChannel07(): Channel[Float64] = let ch = channel Float64; let _ = ch <- 2.0f64; ch

@test
def putChannel08(): Channel[Str] = let ch = channel Str; let _ = ch <- "asdf"; ch

@test
def putChannel09(): Channel[Char] = let ch = channel Char; let _ = ch <- 'c'; ch

@test
def putChannel10(): Channel[Bool] = let ch = channel Bool; let _ = ch <- true; ch

@test
def putChannel11(): Channel[Unit] = let ch = channel Unit; let _ = ch <- (); ch

@test
def putChannel12(): Channel[Int] = let ch = channel Int 1; let _ = ch <- 2; let _ = ch <- 3; let _ = ch <- 3; let _ = ch <- 5; ch

@test
def putChannel13(): Channel[Int] =
    let ch = channel Int;
    ch <- 1 <- 2 <- 3

@test
def putChannel14(): Unit =
    let ch = channel Int;
    let _ = ch <- 1 <- 2 <- 3;
    ()

@test
def putChannel15(): Channel[Str] =
    let ch = channel Str;
    ch <- "a" <- "b" <- "c"

@test
def putChannel16(): Unit =
    let ch = channel Str;
    let _ = ch <- "a" <- "b" <- "c";
    ()

@test
def putChannel17(): Channel[Unit] =
    let ch = channel Unit;
    ch <- () <- () <- ()

@test
def putChannel18(): Unit =
    let ch = channel Unit;
    let _ = ch <- () <- () <- ();
    ()

@test
def putChannel19(): Channel[Channel[Int]] =
    let ch1 = channel Channel[Int];
    let ch2 = channel Int;
    let ch3 = channel Int;
    ch1 <- ch2 <- ch3

@test
def putChannel20(): Unit =
    let ch1 = channel Channel[Int];
    let ch2 = channel Int;
    let ch3 = channel Int;
    let _ = ch1 <- ch2 <- ch3;
    ()

/*


@test
def putChannel21(): Channel[Int] =
    let ch1 = channel Int;
    let ch2 = channel Int;
    ch1 <- <- ch2

@test
def putChannel22(): Unit =
    let ch1 = channel Int;
    let ch2 = channel Int;
    let _ = ch1 <- <- ch2;
    ()

*/

// Int8
def spI8(to: Channel[Int8], v: Int8): Unit =
    spawn putI8(to, v)

def sgI8(from: Channel[Int8]): Unit =
    spawn getI8(from)

def spgI8(to: Channel[Int8], from: Channel[Int8]): Unit =
    spawn putGetI8(to, from)

def sptgI8(to: Channel[Int8], through: Channel[Int8], from: Channel[Int8]): Unit =
    spawn putThroughGet(to, through, from)

def spmI8(to: Channel[Int8], v1: Int8, v2: Int8): Unit =
    spawn putMultiple(to, v1, v2)

def putI8(to: Channel[Int8], v: Int8): Channel[Int8] =
    to <- v

def getI8(from: Channel[Int8]): Int8 =
    <- from

def putGetI8(to: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- from

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

def putI16(to: Channel[Int16], v: Int16): Channel[Int16] =
    to <- v

def getI16(from: Channel[Int16]): Int16 =
    <- from

def putGetI16(to: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- from


// Int32
def spI32(to: Channel[Int32], v: Int32): Unit =
    spawn putI32(to, v)

def sgI32(from: Channel[Int32]): Unit =
    spawn getI32(from)

def spgI32(to: Channel[Int32], from: Channel[Int32]): Unit =
    spawn putGetI32(to, from)

def putI32(to: Channel[Int32], v: Int32): Channel[Int32] =
    to <- v

def getI32(from: Channel[Int32]): Int32 =
    <- from

def putGetI32(to: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- from


// Int64
def spI64(to: Channel[Int64], v: Int64): Unit =
    spawn putI64(to, v)

def sgI64(from: Channel[Int64]): Unit =
    spawn getI64(from)

def spgI64(to: Channel[Int64], from: Channel[Int64]): Unit =
    spawn putGetI64(to, from)


def putI64(to: Channel[Int64], v: Int64): Channel[Int64] =
    to <- v

def getI64(from: Channel[Int64]): Int64 =
    <- from

def putGetI64(to: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- from


// BigInt
def spBI(to: Channel[BigInt], v: BigInt): Unit =
    spawn putBI(to, v)

def sgBI(from: Channel[BigInt]): Unit =
    spawn getBI(from)

def spgBI(to: Channel[BigInt], from: Channel[BigInt]): Unit =
    spawn putGetBI(to, from)


def putBI(to: Channel[BigInt], v: BigInt): Channel[BigInt] =
    to <- v

def getBI(from: Channel[BigInt]): BigInt =
    <- from

def putGetBI(to: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- from


// Unit
def spUnit(to: Channel[Unit], v: Unit): Unit =
    spawn putUnit(to, v)

def sgUnit(from: Channel[Unit]): Unit =
    spawn getUnit(from)

def spgUnit(to: Channel[Unit], from: Channel[Unit]): Unit =
    spawn putGetUnit(to, from)


def putUnit(to: Channel[Unit], v: Unit): Channel[Unit] =
    to <- v

def getUnit(from: Channel[Unit]): Unit =
    <- from

def putGetUnit(to: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- from


// Bool
def spBool(to: Channel[Bool], v: Bool): Unit =
    spawn putBool(to, v)

def sgBool(from: Channel[Bool]): Unit =
    spawn getBool(from)

def spgBool(to: Channel[Bool], from: Channel[Bool]): Unit =
    spawn putGetBool(to, from)


def putBool(to: Channel[Bool], v: Bool): Channel[Bool] =
    to <- v

def getBool(from: Channel[Bool]): Bool =
    <- from

def putGetBool(to: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- from
