//
// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
//
// Exercises:
// - Float32/Float64 parsing and special values (NaN/Inf)
// - float→int conversions (truncation + failure on NaN/Inf)
//
def assertTrue(cond: Bool, msg: String): Unit =
    if (cond) () else bug!(msg)

def assertEqInt32(actual: Int32, expected: Int32, msg: String): Unit =
    if (actual == expected) () else bug!("${msg}: expected=${expected}, got=${actual}")

def main(): Unit \ IO = {
    let nan32 = Float32.nan();
    let inf32 = Float32.positiveInfinity();
    let ninf32 = Float32.negativeInfinity();

    let _ = assertTrue(Float32.isNan(nan32), "Float32.isNan(nan)");
    let _ = assertTrue(nan32 != nan32, "nan != nan");
    let _ = assertTrue(Float32.isInfinite(inf32), "Float32.isInfinite(+inf)");
    let _ = assertTrue(Float32.isInfinite(ninf32), "Float32.isInfinite(-inf)");

    let _ = assertTrue(inf32 > 1.0f32, "+inf > 1.0");
    let _ = assertTrue(ninf32 < -1.0f32, "-inf < -1.0");

    let _ = match Float32.tryToInt32(1.9f32) {
        case Some(v) => { let _ = assertEqInt32(v, 1i32, "Float32.tryToInt32(1.9)"); () }
        case None    => bug!("expected Float32.tryToInt32(1.9) to succeed")
    };

    let _ = match Float32.tryToInt32(nan32) {
        case None    => ()
        case Some(_) => bug!("expected Float32.tryToInt32(nan) to fail")
    };

    let _ = match Float32.tryToInt32(inf32) {
        case None    => ()
        case Some(_) => bug!("expected Float32.tryToInt32(+inf) to fail")
    };

    let _ = match Float64.fromString(" 1.5 ") {
        case Some(v) => { let _ = assertTrue(v == 1.5f64, "Float64.fromString(1.5)"); () }
        case None    => bug!("expected Float64.fromString to succeed")
    };

    println("float-semantics: ok");
    ()
}
