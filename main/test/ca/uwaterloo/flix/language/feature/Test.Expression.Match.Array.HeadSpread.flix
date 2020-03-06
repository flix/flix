@test
def matchUnitArrayHeadSpread1(): Bool & Impure = match [()] {
    case [xs.., ()] => true
    case _ => false
}

@test
def matchUnitArrayHeadSpread2(): Bool & Impure = match [(), ()] {
    case [xs.., (), ()] => true
    case _ => false
}

@test
def matchUnitArrayHeadSpread3(): Bool & Impure = match [[()], [()]] {
    case [xs.., [()], [()]] => true
    case _ => false
}

@test
def matchUnitArrayHeadSpread4(): Bool & Impure = match [(), (), (), ()] {
    case [xs.., (), ()] => true
    case _ => false
}

@test
def matchUnitArrayHeadSpread5(): Bool & Impure = match [()] {
    case [xs.., (), ()] => false
    case _ => true
}

@test
def matchUnitArrayHeadSpread6(): Bool & Impure = match [(), (), (), ()] {
    case [xs.., (), ()] => Array.length(xs) == 2
    case _ => false
}

@test
def matchUnitArrayHeadSpread7(): Bool & Impure = match [(), (), (), ()] {
    case [xs.., (), ()] => xs[0] == ()
    case _ => false
}

@test
def matchIntArrayHeadSpread1(): Bool & Impure = match [1] {
    case [xs.., 1] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread2(): Bool & Impure = match [1, 2] {
    case [xs.., 2] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread3(): Bool & Impure = match [1, 2] {
    case [xs.., 1, 2] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread4(): Bool & Impure = match [1, 2] {
    case [xs.., 1, _] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread5(): Bool & Impure = match [2, 1] {
    case [xs.., 1, 2] => false
    case _ => true
}

@test
def matchIntArrayHeadSpread6(): Bool & Impure = match [[1], [2]] {
    case [xs.., [1], [2]] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread7(): Bool & Impure = match [[1, 2, 3], [1, 2, 3]] {
    case [xs.., [1, 2, 3], [1, 2, 3]] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread8(): Bool & Impure = match [[[1]], [[2]]] {
    case [xs.., [[1]], [[2]]] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread9(): Bool & Impure = match [[1], [2], [3], [4]] {
    case [xs.., [3], [4]] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread10(): Bool & Impure = match [1, 2, 3, 4] {
    case [xs.., 3, 4] => true
    case _ => false
}

@test
def matchIntArrayHeadSpread11(): Bool & Impure = match [1] {
    case [xs.., 1, 2] => false
    case _ => true
}

@test
def matchIntArrayHeadSpread12(): Bool & Impure = match [1, 2, 3, 4] {
    case [xs.., 3, 4] => Array.length(xs) == 2
    case _ => false
}

@test
def matchIntArrayHeadSpread13(): Bool & Impure = match [1, 2, 3, 4] {
    case [xs.., 3, 4] => xs[1] == 2
    case _ => false
}

@test
def matchInt8ArrayHeadSpread1(): Bool & Impure = match [1i8] {
    case [xs.., 1i8] => true
    case _ => false
}

@test
def matchInt8ArrayHeadSpread2(): Bool & Impure = match [1i8] {
    case [xs.., 2i8] => false
    case _ => true
}

@test
def matchInt8ArrayHeadSpread3(): Bool & Impure = match [1i8, 2i8] {
    case [xs.., 1i8, 2i8] => true
    case _ => false
}

@test
def matchInt8ArrayHeadSpread4(): Bool & Impure = match [[1i8], [2i8]] {
    case [xs.., [1i8], [2i8]] => true
    case _ => false
}

@test
def matchInt8ArrayHeadSpread5(): Bool & Impure = match [1i8, 2i8, 3i8, 4i8] {
    case [xs.., 3i8, 4i8] => true
    case _ => false
}

@test
def matchInt8ArrayHeadSpread6(): Bool & Impure = match [2i8] {
    case [xs.., 1i8, 2i8] => false
    case _ => true
}

@test
def matchInt8ArrayHeadSpread7(): Bool & Impure = match [1i8, 2i8, 3i8, 4i8] {
    case [xs.., 3i8, 4i8] => Array.length(xs) == 2
    case _ => false
}

@test
def matchInt8ArrayHeadSpread8(): Bool & Impure = match [1i8, 2i8, 3i8, 4i8] {
    case [xs.., 3i8, 4i8] => xs[1] == 2i8
    case _ => false
}

@test
def matchInt16ArrayHeadSpread1(): Bool & Impure = match [1i16] {
    case [xs.., 1i16] => true
    case _ => false
}

@test
def matchInt16ArrayHeadSpread2(): Bool & Impure = match [1i16] {
    case [xs.., 2i16] => false
    case _ => true
}

@test
def matchInt16ArrayHeadSpread3(): Bool & Impure = match [1i16, 2i16] {
    case [xs.., 1i16, 2i16] => true
    case _ => false
}

@test
def matchInt16ArrayHeadSpread4(): Bool & Impure = match [[1i16], [2i16]] {
    case [xs.., [1i16], [2i16]] => true
    case _ => false
}

@test
def matchInt16ArrayHeadSpread5(): Bool & Impure = match [1i16, 2i16, 3i16, 4i16] {
    case [xs.., 3i16, 4i16] => true
    case _ => false
}

@test
def matchInt16ArrayHeadSpread6(): Bool & Impure = match [1i16] {
    case [xs.., 1i16, 2i16] => false
    case _ => true
}

@test
def matchInt16ArrayHeadSpread7(): Bool & Impure = match [1i16, 2i16, 3i16, 4i16] {
    case [xs.., 3i16, 4i16] => Array.length(xs) == 2
    case _ => false
}

@test
def matchInt16ArrayHeadSpread8(): Bool & Impure = match [1i16, 2i16, 3i16, 4i16] {
    case [xs.., 3i16, 4i16] => xs[1] == 2i16
    case _ => false
}

@test
def matchInt64ArrayHeadSpread1(): Bool & Impure = match [1i64] {
    case [xs.., 1i64] => true
    case _ => false
}

@test
def matchInt64ArrayHeadSpread2(): Bool & Impure = match [1i64] {
    case [xs.., 2i64] => false
    case _ => true
}

@test
def matchInt64ArrayHeadSpread3(): Bool & Impure = match [1i64, 2i64] {
    case [xs.., 1i64, 2i64] => true
    case _ => false
}

@test
def matchInt64ArrayHeadSpread4(): Bool & Impure = match [[1i64], [2i64]] {
    case [xs.., [1i64], [2i64]] => true
    case _ => false
}

@test
def matchInt64ArrayHeadSpread5(): Bool & Impure = match [1i64, 2i64, 3i64, 4i64] {
    case [xs.., 3i64, 4i64] => true
    case _ => false
}

@test
def matchInt64ArrayHeadSpread6(): Bool & Impure = match [1i64] {
    case [xs.., 1i64, 2i64] => false
    case _ => true
}

@test
def matchInt64ArrayHeadSpread7(): Bool & Impure = match [1i64, 2i64, 3i64, 4i64] {
    case [xs.., 3i64, 4i64] => Array.length(xs) == 2
    case _ => false
}

@test
def matchInt64ArrayHeadSpread8(): Bool & Impure = match [1i64, 2i64, 3i64, 4i64] {
    case [xs.., 3i64, 4i64] => xs[1] == 2i64
    case _ => false
}

@test
def matchBigIntArrayHeadSpread1(): Bool & Impure = match [1ii] {
    case [xs.., 1ii] => true
    case _ => false
}

@test
def matchBigIntArrayHeadSpread2(): Bool & Impure = match [1ii] {
    case [xs.., 2ii] => false
    case _ => true
}

@test
def matchBigIntArrayHeadSpread3(): Bool & Impure = match [1ii, 2ii] {
    case [xs.., 1ii, 2ii] => true
    case _ => false
}

@test
def matchBigIntArrayHeadSpread4(): Bool & Impure = match [[1ii], [2ii]] {
    case [xs.., [1ii], [2ii]] => true
    case _ => false
}

@test
def matchBigIntArrayHeadSpread5(): Bool & Impure = match [1ii, 2ii, 3ii, 4ii] {
    case [xs.., 3ii, 4ii] => true
    case _ => false
}

@test
def matchBigIntArrayHeadSpread6(): Bool & Impure = match [1ii, 2ii] {
    case [xs.., 1ii] => false
    case _ => true
}

@test
def matchBigIntArrayHeadSpread7(): Bool & Impure = match [1ii, 2ii, 3ii, 4ii] {
    case [xs.., 3ii, 4ii] => Array.length(xs) == 2
    case _ => false
}

@test
def matchBigIntArrayHeadSpread8(): Bool & Impure = match [1ii, 2ii, 3ii, 4ii] {
    case [xs.., 3ii, 4ii] => xs[1] == 2ii
    case _ => false
}

@test
def matchStrArrayHeadSpread1(): Bool & Impure = match ["one"] {
    case [xs.., "one"] => true
    case _ => false
}

@test
def matchStrArrayHeadSpread2(): Bool & Impure = match ["one"] {
    case [xs.., "one "] => false
    case _ => true
}

@test
def matchStrArrayHeadSpread3(): Bool & Impure = match ["one", "two"] {
    case [xs.., "one",  "two"] => true
    case _ => false
}

@test
def matchStrArrayHeadSpread4(): Bool & Impure = match [["one"], ["two"]] {
    case [xs.., ["one"],  ["two"]] => true
    case _ => false
}

@test
def matchStrArrayHeadSpread5(): Bool & Impure = match ["one", "two", "three", "four"] {
    case [xs.., "three",  "four"] => true
    case _ => false
}

@test
def matchStrArrayHeadSpread6(): Bool & Impure = match ["one"] {
    case [xs.., "one", "two"] => false
    case _ => true
}

@test
def matchStrArrayHeadSpread7(): Bool & Impure = match ["one", "two", "three", "four"] {
    case [xs.., "three",  "four"] => Array.length(xs) == 2
    case _ => false
}

@test
def matchStrArrayHeadSpread8(): Bool & Impure = match ["one", "two", "three", "four"] {
    case [xs.., "three",  "four"] => xs[1] == "two"
    case _ => false
}

@test
def matchBoolArrayHeadSpread1(): Bool & Impure = match [true] {
    case [xs.., true] => true
    case _ => false
}

@test
def matchBoolArrayHeadSpread2(): Bool & Impure = match [true] {
    case [xs.., false] => false
    case _ => true
}

@test
def matchBoolArrayHeadSpread3(): Bool & Impure = match [true, false] {
    case [xs.., true, false] => true
    case _ => false
}

@test
def matchBoolArrayHeadSpread4(): Bool & Impure = match [[false], [false]] {
    case [xs.., [false], [false]] => true
    case _ => false
}

@test
def matchBoolArrayHeadSpread5(): Bool & Impure = match [true, false, false, true] {
    case [xs.., false, true] => true
    case _ => false
}

@test
def matchBoolArrayHeadSpread6(): Bool & Impure = match [true] {
    case [xs.., true, false] => false
    case _ => true
}

@test
def matchBoolArrayHeadSpread7(): Bool & Impure = match [true, false, false, true] {
    case [xs.., false, true] => Array.length(xs) == 2
    case _ => false
}

@test
def matchBoolArrayHeadSpread8(): Bool & Impure = match [true, false, false, true] {
    case [xs.., false, true] => xs[1] == false
    case _ => false
}

@test
def matchTupleArrayHeadSpread1(): Bool & Impure = match [(1, "one")] {
    case [xs.., (1, "one")] => true
    case _ => false
}

@test
def matchTupleArrayHeadSpread2(): Bool & Impure = match [(1, "one")] {
    case [xs.., (1, "one ")] => false
    case _ => true
}

@test
def matchTupleArrayHeadSpread3(): Bool & Impure = match [(1, "one")] {
    case [xs.., (2, "one")] => false
    case _ => true
}

@test
def matchTupleArrayHeadSpread4(): Bool & Impure = match [(1, "one"), (2, "two")] {
    case [xs.., (1, "one"), (2, "two")] => true
    case _ => false
}

@test
def matchTupleArrayHeadSpread5(): Bool & Impure = match [[(1, "one")], [(2, "two")]] {
    case [xs.., [(1, "one")], [(2, "two")]] => true
    case _ => false
}

@test
def matchTupleArrayHeadSpread6(): Bool & Impure = match [(1, "one"), (2, "two"), (3, "three"), (4, "four")] {
    case [xs.., (3, "three"), (4, "four")] => true
    case _ => false
}

@test
def matchTupleArrayHeadSpread7(): Bool & Impure = match [(1, "one")] {
    case [xs.., (1, "one"), (2, "two")] => false
    case _ => true
}

@test
def matchTupleArrayHeadSpread8(): Bool & Impure = match [(1, "one"), (2, "two"), (3, "three"), (4, "four")] {
    case [xs.., (3, "three"), (4, "four")] => Array.length(xs) == 2
    case _ => false
}

@test
def matchTupleArrayHeadSpread9(): Bool & Impure = match [(1, "one"), (2, "two"), (3, "three"), (4, "four")] {
    case [xs.., (3, "three"), (4, "four")] => xs[1] == (2, "two")
    case _ => false
}

@test
def matchFloatArrayHeadSpread1(): Bool & Impure = match [1.1] {
    case [xs.., 1.1] => true
    case _ => false
}

@test
def matchFloatArrayHeadSpread2(): Bool & Impure = match [1.1] {
    case [xs.., 1.2] => false
    case _ => true
}

@test
def matchFloatArrayHeadSpread3(): Bool & Impure = match [1.1, 2.2] {
    case [xs.., 1.1, 2.2] => true
    case _ => false
}

@test
def matchFloatArrayHeadSpread4(): Bool & Impure = match [[1.1], [2.2]] {
    case [xs.., [1.1], [2.2]] => true
    case _ => false
}

@test
def matchFloatArrayHeadSpread5(): Bool & Impure = match [1.1, 2.2, 3.3, 4.4] {
    case [xs.., 3.3, 4.4] => true
    case _ => false
}

@test
def matchFloatArrayHeadSpread6(): Bool & Impure = match [1.1] {
    case [xs.., 1.1, 2.2] => false
    case _ => true
}

@test
def matchFloatArrayHeadSpread7(): Bool & Impure = match [1.1, 2.2, 3.3, 4.4] {
    case [xs.., 3.3, 4.4] => Array.length(xs) == 2
    case _ => false
}

@test
def matchFloatArrayHeadSpread8(): Bool & Impure = match [1.1, 2.2, 3.3, 4.4] {
    case [xs.., 3.3, 4.4] => xs[1] == 2.2
    case _ => false
}

@test
def matchFloat32ArrayHeadSpread1(): Bool & Impure = match [1.1f32] {
    case [xs.., 1.1f32] => true
    case _ => false
}

@test
def matchFloat32ArrayHeadSpread2(): Bool & Impure = match [1.1f32] {
    case [xs.., 1.0f32] => false
    case _ => true
}

@test
def matchFloat32ArrayHeadSpread3(): Bool & Impure = match [1.1f32, 2.2f32] {
    case [xs.., 1.1f32, 2.2f32] => true
    case _ => false
}

@test
def matchFloat32ArrayHeadSpread4(): Bool & Impure = match [[1.1f32], [2.2f32]] {
    case [xs.., [1.1f32], [2.2f32]] => true
    case _ => false
}

@test
def matchFloat32ArrayHeadSpread5(): Bool & Impure = match [1.1f32, 2.2f32, 3.3f32, 4.4f32] {
    case [xs.., 3.3f32, 4.4f32] => true
    case _ => false
}

@test
def matchFloat32ArrayHeadSpread6(): Bool & Impure = match [1.1f32] {
    case [xs.., 1.1f32, 2.2f32] => false
    case _ => true
}

@test
def matchFloat32ArrayHeadSpread7(): Bool & Impure = match [1.1f32, 2.2f32, 3.3f32, 4.4f32] {
    case [xs.., 3.3f32, 4.4f32] => Array.length(xs) == 2
    case _ => false
}

@test
def matchFloat32ArrayHeadSpread8(): Bool & Impure = match [1.1f32, 2.2f32, 3.3f32, 4.4f32] {
    case [xs.., 3.3f32, 4.4f32] => xs[1] == 2.2f32
    case _ => false
}

@test
def matchCharArrayHeadSpread1(): Bool & Impure = match ['1'] {
    case [xs.., '1'] => true
    case _ => false
}

@test
def matchCharArrayHeadSpread2(): Bool & Impure = match ['1'] {
    case [xs.., '2'] => false
    case _ => true
}

@test
def matchCharArrayHeadSpread3(): Bool & Impure = match ['1', '2'] {
    case [xs.., '1', '2'] => true
    case _ => false
}

@test
def matchCharArrayHeadSpread4(): Bool & Impure = match [['1'], ['2']] {
    case [xs.., ['1'], ['2']] => true
    case _ => false
}

// TODO: Broken?
//@test
//def matchCharArrayHeadSpread5(): Bool = match ['1', '2', '3', '4'] {
//    case [xs.., '1', '2'] => true
//    case _ => false
//}

@test
def matchCharArrayHeadSpread6(): Bool & Impure = match ['1'] {
    case [xs.., '1', '2'] => false
    case _ => true
}

// TODO: Broken?
//@test
//def matchCharArrayHeadSpread7(): Bool = match ['1', '2', '3', '4'] {
//    case [xs.., '1', '2'] => Array.length(xs) == 2
//    case _ => false
//}

// TODO: Broken?
//@test
//def matchCharArrayHeadSpread8(): Bool = match ['1', '2', '3', '4'] {
//    case [xs.., '1', '2'] => xs[1] == '2'
//    case _ => false
//}

@test
def matchOptionArrayHeadSpread1(): Bool & Impure = match [Some(1)] {
    case [xs.., Some(1)] => true
    case _ => false
}

@test
def matchOptionArrayHeadSpread2(): Bool & Impure = match [Some(1)] {
    case [xs.., None] => false
    case _ => true
}

@test
def matchOptionArrayHeadSpread3(): Bool & Impure = match [Some(1), Some(2)] {
    case [xs.., Some(1), Some(2)] => true
    case _ => false
}

@test
def matchOptionArrayHeadSpread4(): Bool & Impure = match [[Some(1)], [Some(2)]] {
    case [xs.., [Some(1)], [Some(2)]] => true
    case _ => false
}

@test
def matchOptionArrayHeadSpread5(): Bool & Impure = match [Some(1), Some(2), None(), Some(42)] {
    case [xs.., None(), Some(42)] => true
    case _ => false
}

@test
def matchOptionArrayHeadSpread6(): Bool & Impure = match [Some(1)] {
    case [xs.., Some(1), Some(2)] => false
    case _ => true
}

@test
def matchOptionArrayHeadSpread7(): Bool & Impure = match [Some(1), Some(2), None(), Some(42)] {
    case [xs.., None(), Some(42)] => Array.length(xs) == 2
    case _ => false
}

@test
def matchOptionArrayHeadSpread8(): Bool & Impure = match [Some(1), Some(2), None(), Some(42)] {
    case [xs.., None(), Some(42)] => xs[1] == Some(2)
    case _ => false
}

@test
def matchResultArrayHeadSpread1(): Bool & Impure = match [Ok(42)] {
    case [xs.., Ok(42)] => true
    case _ => false
}

@test
def matchResultArrayHeadSpread2(): Bool & Impure = match [Ok(42)] {
    case [xs.., Err(23)] => false
    case _ => true
}

@test
def matchResultArrayHeadSpread3(): Bool & Impure = match [Ok(42), Err(23)] {
    case [xs.., Ok(42), Err(23)] => true
    case _ => false
}

@test
def matchResultArrayHeadSpread4(): Bool & Impure = match [[Ok(42)], [Err(23)]] {
    case [xs.., [Ok(42)], [Err(23)]] => true
    case _ => false
}

@test
def matchResultArrayHeadSpread5(): Bool & Impure = match [Ok(42), Err(23), Ok(1), Err(2)] {
    case [xs.., Ok(1), Err(2)] => true
    case _ => false
}

@test
def matchResultArrayHeadSpread6(): Bool & Impure = match [Ok(42)] {
    case [xs.., Ok(42), Err(23)] => false
    case _ => true
}

@test
def matchResultArrayHeadSpread7(): Bool & Impure = match [Ok(42), Err(23), Ok(1), Err(2)] {
    case [xs.., Ok(1), Err(2)] => Array.length(xs) == 2
    case _ => false
}

@test
def matchResultArrayHeadSpread8(): Bool & Impure = match [Ok(42), Err(23), Ok(1), Err(2)] {
    case [xs.., Ok(1), Err(2)] => xs[1] == Err(23)
    case _ => false
}
