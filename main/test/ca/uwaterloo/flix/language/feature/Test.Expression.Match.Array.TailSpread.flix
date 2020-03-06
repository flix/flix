@test
def matchUnitArrayTailSpread1(): Bool & Impure = match [()] {
    case [(), ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread2(): Bool & Impure = match [(), ()] {
    case [(), (), ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread3(): Bool & Impure = match [[()], [()]] {
    case [[()], [()], ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread4(): Bool & Impure = match [(), (), (), ()] {
    case [(), (), ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread5(): Bool & Impure = match [()] {
    case [(), (), ..xs] => false
    case _ => true
}

@test
def matchUnitArrayTailSpread6(): Bool & Impure = match [(), (), (), ()] {
    case [(), (), ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchUnitArrayTailSpread7(): Bool & Impure = match [(), (), (), ()] {
    case [(), (), ..xs] => xs[1] == ()
    case _ => false
}

@test
def matchIntArrayTailSpread1(): Bool & Impure = match [1] {
    case [1, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread2(): Bool & Impure = match [1, 2] {
    case [1, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread3(): Bool  & Impure= match [1, 2] {
    case [1, 2, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread4(): Bool & Impure = match [1, 2] {
    case [1, _, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread5(): Bool & Impure = match [2, 1] {
    case [1, 2, ..xs] => false
    case _ => true
}

@test
def matchIntArrayTailSpread6(): Bool & Impure = match [[1], [2]] {
    case [[1], [2], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread7(): Bool & Impure = match [[1, 2, 3], [1, 2, 3]] {
    case [[1, 2, 3], [1, 2, 3], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread8(): Bool & Impure = match [[[1]], [[2]]] {
    case [[[1]], [[2]], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread9(): Bool & Impure = match [[1], [2], [3], [4]] {
    case [[1], [2], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread10(): Bool & Impure = match [1, 2, 3, 4] {
    case [1, 2, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread11(): Bool & Impure = match [1] {
    case [1, 2, ..xs] => false
    case _ => true
}

@test
def matchIntArrayTailSpread12(): Bool & Impure = match [1, 2, 3, 4] {
    case [1, 2, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchIntArrayTailSpread13(): Bool & Impure = match [1, 2, 3, 4] {
    case [1, 2, ..xs] => xs[1] == 4
    case _ => false
}

@test
def matchInt8ArrayTailSpread1(): Bool & Impure = match [1i8] {
    case [1i8, ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread2(): Bool & Impure = match [1i8] {
    case [2i8, ..xs] => false
    case _ => true
}

@test
def matchInt8ArrayTailSpread3(): Bool & Impure = match [1i8, 2i8] {
    case [1i8, 2i8, ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread4(): Bool & Impure = match [[1i8], [2i8]] {
    case [[1i8], [2i8], ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread5(): Bool & Impure = match [1i8, 2i8, 3i8, 4i8] {
    case [1i8, 2i8, ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread6(): Bool & Impure = match [1i8] {
    case [1i8, 2i8, ..xs] => false
    case _ => true
}

@test
def matchInt8ArrayTailSpread7(): Bool & Impure = match [1i8, 2i8, 3i8, 4i8] {
    case [1i8, 2i8, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchInt8ArrayTailSpread8(): Bool & Impure = match [1i8, 2i8, 3i8, 4i8] {
    case [1i8, 2i8, ..xs] => xs[1] == 4i8
    case _ => false
}

@test
def matchInt16ArrayTailSpread1(): Bool & Impure = match [1i16] {
    case [1i16, ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread2(): Bool & Impure = match [1i16] {
    case [2i16, ..xs] => false
    case _ => true
}

@test
def matchInt16ArrayTailSpread3(): Bool & Impure = match [1i16, 2i16] {
    case [1i16, 2i16, ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread4(): Bool & Impure = match [[1i16], [2i16]] {
    case [[1i16], [2i16], ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread5(): Bool & Impure = match [1i16, 2i16, 3i16, 4i16] {
    case [1i16, 2i16, ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread6(): Bool & Impure = match [1i16] {
    case [1i16, 2i16, ..xs] => false
    case _ => true
}

@test
def matchInt16ArrayTailSpread7(): Bool & Impure = match [1i16, 2i16, 3i16, 4i16] {
    case [1i16, 2i16, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchInt16ArrayTailSpread8(): Bool & Impure = match [1i16, 2i16, 3i16, 4i16] {
    case [1i16, 2i16, ..xs] => xs[1] == 4i16
    case _ => false
}

@test
def matchInt64ArrayTailSpread1(): Bool & Impure = match [1i64] {
    case [1i64, ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread2(): Bool & Impure = match [1i64] {
    case [2i64, ..xs] => false
    case _ => true
}

@test
def matchInt64ArrayTailSpread3(): Bool & Impure = match [1i64, 2i64] {
    case [1i64, 2i64, ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread4(): Bool & Impure = match [[1i64], [2i64]] {
    case [[1i64], [2i64], ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread5(): Bool & Impure = match [1i64, 2i64, 3i64, 4i64] {
    case [1i64, 2i64, ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread6(): Bool & Impure = match [1i64] {
    case [1i64, 2i64, ..xs] => false
    case _ => true
}

@test
def matchInt64ArrayTailSpread7(): Bool & Impure = match [1i64, 2i64, 3i64, 4i64] {
    case [1i64, 2i64, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchInt64ArrayTailSpread8(): Bool & Impure = match [1i64, 2i64, 3i64, 4i64] {
    case [1i64, 2i64, ..xs] => xs[1] == 4i64
    case _ => false
}

@test
def matchBigIntArrayTailSpread1(): Bool & Impure = match [1ii] {
    case [1ii, ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread2(): Bool & Impure = match [1ii] {
    case [2ii, ..xs] => false
    case _ => true
}

@test
def matchBigIntArrayTailSpread3(): Bool & Impure = match [1ii, 2ii] {
    case [1ii, 2ii, ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread4(): Bool & Impure = match [[1ii], [2ii]] {
    case [[1ii], [2ii], ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread5(): Bool & Impure = match [1ii, 2ii, 3ii, 4ii] {
    case [1ii, 2ii, ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread6(): Bool & Impure = match [1ii] {
    case [1ii, 2ii, ..xs] => false
    case _ => true
}

@test
def matchBigIntArrayTailSpread7(): Bool & Impure = match [1ii, 2ii, 3ii, 4ii] {
    case [1ii, 2ii, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchBigIntArrayTailSpread8(): Bool & Impure = match [1ii, 2ii, 3ii, 4ii] {
    case [1ii, 2ii, ..xs] => xs[1] == 4ii
    case _ => false
}

@test
def matchStrArrayTailSpread1(): Bool & Impure = match ["one"] {
    case ["one", ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread2(): Bool & Impure = match ["one"] {
    case ["one ", ..xs] => false
    case _ => true
}

@test
def matchStrArrayTailSpread3(): Bool & Impure = match ["one", "two"] {
    case ["one", "two", ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread4(): Bool & Impure = match [["one"], ["two"]] {
    case [["one"], ["two"], ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread5(): Bool & Impure = match ["one", "two", "three", "four"] {
    case ["one", "two", ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread6(): Bool & Impure = match ["one"] {
    case ["one", "two", ..xs] => false
    case _ => true
}

@test
def matchStrArrayTailSpread7(): Bool & Impure = match ["one", "two", "three", "four"] {
    case ["one", "two", ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchStrArrayTailSpread8(): Bool & Impure = match ["one", "two", "three", "four"] {
    case ["one", "two", ..xs] => xs[1] == "four"
    case _ => false
}

@test
def matchBoolArrayTailSpread1(): Bool & Impure = match [true] {
    case [true, ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread2(): Bool & Impure = match [true] {
    case [false, ..xs] => false
    case _ => true
}

@test
def matchBoolArrayTailSpread3(): Bool & Impure = match [true, false] {
    case [true, false, ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread4(): Bool & Impure = match [[false], [false]] {
    case [[false], [false], ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread5(): Bool & Impure = match [true, false, false, true] {
    case [true, false, ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread6(): Bool & Impure = match [true] {
    case [true, false, ..xs] => false
    case _ => true
}

@test
def matchBoolArrayTailSpread7(): Bool & Impure = match [true, false, false, true] {
    case [true, false, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchBoolArrayTailSpread8(): Bool & Impure = match [true, false, false, true] {
    case [true, false, ..xs] => xs[1] == true
    case _ => false
}

@test
def matchTupleArrayTailSpread1(): Bool & Impure = match [(1, "one")] {
    case [(1, "one"), ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread2(): Bool & Impure = match [(1, "one")] {
    case [(1, "one "), ..xs] => false
    case _ => true
}

@test
def matchTupleArrayTailSpread3(): Bool & Impure = match [(1, "one")] {
    case [(2, "one"), ..xs] => false
    case _ => true
}

@test
def matchTupleArrayTailSpread4(): Bool & Impure = match [(1, "one"), (2, "two")] {
    case [(1, "one"), (2, "two"), ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread5(): Bool & Impure = match [[(1, "one")], [(2, "two")]] {
    case [[(1, "one")], [(2, "two")], ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread6(): Bool & Impure = match [(1, "one"), (2, "two"), (3, "three"), (4, "four")] {
    case [(1, "one"), (2, "two"), ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread7(): Bool & Impure = match [(1, "one")] {
    case [(1, "one"), (2, "two"), ..xs] => false
    case _ => true
}

@test
def matchTupleArrayTailSpread8(): Bool & Impure = match [(1, "one"), (2, "two"), (3, "three"), (4, "four")] {
    case [(1, "one"), (2, "two"), ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchTupleArrayTailSpread9(): Bool & Impure = match [(1, "one"), (2, "two"), (3, "three"), (4, "four")] {
    case [(1, "one"), (2, "two"), ..xs] => xs[1] == (4, "four")
    case _ => false
}

@test
def matchFloatArrayTailSpread1(): Bool & Impure = match [1.1] {
    case [1.1, ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread2(): Bool & Impure = match [1.1] {
    case [1.2, ..xs] => false
    case _ => true
}

@test
def matchFloatArrayTailSpread3(): Bool & Impure = match [1.1, 2.2] {
    case [1.1, 2.2, ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread4(): Bool & Impure = match [[1.1], [2.2]] {
    case [[1.1], [2.2], ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread5(): Bool & Impure = match [1.1, 2.2, 3.3, 4.4] {
    case [1.1, 2.2, ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread6(): Bool & Impure = match [1.1] {
    case [1.1, 2.2, ..xs] => false
    case _ => true
}

@test
def matchFloatArrayTailSpread7(): Bool & Impure = match [1.1, 2.2, 3.3, 4.4] {
    case [1.1, 2.2, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchFloatArrayTailSpread8(): Bool & Impure = match [1.1, 2.2, 3.3, 4.4] {
    case [1.1, 2.2, ..xs] => xs[1] == 4.4
    case _ => false
}

@test
def matchFloat32ArrayTailSpread1(): Bool & Impure = match [1.1f32] {
    case [1.1f32, ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread2(): Bool & Impure = match [1.1f32] {
    case [1.0f32, ..xs] => false
    case _ => true
}

@test
def matchFloat32ArrayTailSpread3(): Bool & Impure = match [1.1f32, 2.2f32] {
    case [1.1f32, 2.2f32, ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread4(): Bool & Impure = match [[1.1f32], [2.2f32]] {
    case [[1.1f32], [2.2f32], ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread5(): Bool & Impure = match [1.1f32, 2.2f32, 3.3f32, 4.4f32] {
    case [1.1f32, 2.2f32, ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread6(): Bool & Impure = match [1.1f32] {
    case [1.1f32, 2.2f32, ..xs] => false
    case _ => true
}

@test
def matchFloat32ArrayTailSpread7(): Bool & Impure = match [1.1f32, 2.2f32, 3.3f32, 4.4f32] {
    case [1.1f32, 2.2f32, ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchFloat32ArrayTailSpread8(): Bool & Impure = match [1.1f32, 2.2f32, 3.3f32, 4.4f32] {
    case [1.1f32, 2.2f32, ..xs] => xs[1] == 4.4f32
    case _ => false
}

@test
def matchCharArrayTailSpread1(): Bool & Impure = match ['1'] {
    case ['1', ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread2(): Bool & Impure = match ['1'] {
    case ['2', ..xs] => false
    case _ => true
}

@test
def matchCharArrayTailSpread3(): Bool & Impure = match ['1', '2'] {
    case ['1', '2', ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread4(): Bool & Impure = match [['1'], ['2']] {
    case [['1'], ['2'], ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread5(): Bool & Impure = match ['1', '2', '3', '4'] {
    case ['1', '2', ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread6(): Bool & Impure = match ['1'] {
    case ['1', '2', ..xs] => false
    case _ => true
}

@test
def matchCharArrayTailSpread7(): Bool & Impure = match ['1', '2', '3', '4'] {
    case ['1', '2', ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchCharArrayTailSpread8(): Bool & Impure = match ['1', '2', '3', '4'] {
    case ['1', '2', ..xs] => xs[1] == '4'
    case _ => false
}

@test
def matchOptionArrayTailSpread1(): Bool & Impure = match [Some(1)] {
    case [Some(1), ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread2(): Bool & Impure = match [Some(1)] {
    case [None, ..xs] => false
    case _ => true
}

@test
def matchOptionArrayTailSpread3(): Bool & Impure = match [Some(1), Some(2)] {
    case [Some(1), Some(2), ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread4(): Bool & Impure = match [[Some(1)], [Some(2)]] {
    case [[Some(1)], [Some(2)], ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread5(): Bool & Impure = match [Some(1), Some(2), None(), Some(42)] {
    case [Some(1), Some(2), ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread6(): Bool & Impure = match [Some(1)] {
    case [Some(1), Some(2), ..xs] => false
    case _ => true
}

@test
def matchOptionArrayTailSpread7(): Bool & Impure = match [Some(1), Some(2), None(), Some(42)] {
    case [Some(1), Some(2), ..xs] => Array.length(xs) == 2
    case _ => false
}

@test
def matchOptionArrayTailSpread8(): Bool & Impure = match [Some(1), Some(2), None(), Some(42)] {
    case [Some(1), Some(2), ..xs] => xs[1] == Some(42)
    case _ => false
}

@test
def matchResultArrayTailSpread1(): Bool & Impure = match [Ok(42)] {
    case [Ok(42), ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread2(): Bool & Impure = match [Ok(42)] {
    case [Err(23) , ..xs] => false
    case _ => true
}

@test
def matchResultArrayTailSpread3(): Bool & Impure = match [Ok(42), Err(23)] {
    case [Ok(42), Err(23), ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread4(): Bool & Impure = match [[Ok(42)], [Err(23)]] {
    case [[Ok(42)], [Err(23)], ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread5(): Bool & Impure = match [Ok(42), Err(23), Ok(1), Err(2)] {
    case [Ok(42), Err(23), ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread6(): Bool & Impure = match [Ok(42)] {
    case [Ok(42), Err(23), ..xs]  => false
    case _ => true
}

@test
def matchResultArrayTailSpread7(): Bool & Impure = match [Ok(42), Err(23), Ok(1), Err(2)] {
    case [Ok(42), Err(23), ..xs]  => Array.length(xs) == 2
    case _ => false
}

@test
def matchResultArrayTailSpread8(): Bool & Impure = match [Ok(42), Err(23), Ok(1), Err(2)] {
    case [Ok(42), Err(23), ..xs]  => xs[1] == Err(2)
    case _ => false
}

