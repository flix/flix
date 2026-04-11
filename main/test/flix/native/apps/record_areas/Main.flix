//
// A small "real program" fixture for the LLVM-native and LLVM-wasm backends.
//
// Exercises:
// - record construction + field selection
// - polymorphic record extension
//
def assertEqInt32(actual: Int32, expected: Int32, msg: String): Unit =
    if (actual == expected) () else bug!("${msg}: expected=${expected}, got=${actual}")

def area(r: {x = Int32, y = Int32}): Int32 = r#x * r#y

def polyArea(r: {x = Int32, y = Int32 | a}): Int32 = r#x * r#y

def main(): Unit \ IO = {
    let _ = assertEqInt32(area({x = 1, y = 2}), 2i32, "area({x=1,y=2})");
    let _ = assertEqInt32(area({y = 2, x = 3}), 6i32, "area({x=3,y=2})");

    let _ = assertEqInt32(polyArea({x = 1, y = 2}), 2i32, "polyArea({x=1,y=2})");
    let _ = assertEqInt32(polyArea({x = 2, y = 3, z = 4}), 6i32, "polyArea({x=2,y=3,z=4})");

    println("record-areas: ok");
    ()
}
