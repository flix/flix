/////////////////////////////////////////////////////////////////////////////
// Put channel expressions                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def putChannel01(): Channel[Int] = let ch = channel Int; let _ = ch <- 2; ch

@test
def putChannel02(): Channel[Int8] = let ch = channel Int8; let _ = ch <- 2i8; ch

@test
def putChannel03(): Channel[Int16] = let ch = channel Int16; let _ = ch <- 2i16; ch

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