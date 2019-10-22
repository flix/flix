@test
def matchUnitArrayTailSpread1(): Bool = match [()] {
    case [(), ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread2(): Bool = match [(), ()] {
    case [(), (), ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread3(): Bool = match [[()], [()]] {
    case [[()], [()], ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread4(): Bool = match [(), (), (), ()] {
    case [(), (), ..xs] => true
    case _ => false
}

@test
def matchUnitArrayTailSpread5(): Bool = match [()] {
    case [(), (), ..xs] => false
    case _ => true
}

@test
def matchIntArrayTailSpread1(): Bool = match [1] {
    case [1, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread2(): Bool = match [1, 2] {
    case [1, ..xs] => false
    case _ => true
}

@test
def matchIntArrayTailSpread3(): Bool = match [1, 2] {
    case [1, 2, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread4(): Bool = match [1, 2] {
    case [1, _, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread5(): Bool = match [2, 1] {
    case [1, 2, ..xs] => false
    case _ => true
}

@test
def matchIntArrayTailSpread6(): Bool = match [[1], [2]] {
    case [[1], [2], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread7(): Bool = match [[1, 2, 3], [1, 2, 3]] {
    case [[1, 2, 3], [1, 2, 3], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread8(): Bool = match [[[1]], [[2]]] {
    case [[[1]], [[2]], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread9(): Bool = match [[1], [2], [3], [4]] {
    case [[1], [2], ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread10(): Bool = match [1, 2, 3, 4] {
    case [1, 2, ..xs] => true
    case _ => false
}

@test
def matchIntArrayTailSpread11(): Bool = match [1] {
    case [1, 2, ..xs] => false
    case _ => true
}

@test
def matchInt8ArrayTailSpread1(): Bool = match [1i8] {
    case [1i8, ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread2(): Bool = match [1i8] {
    case [2i8, ..xs] => false
    case _ => true
}

@test
def matchInt8ArrayTailSpread3(): Bool = match [1i8, 2i8] {
    case [1i8, 2i8, ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread4(): Bool = match [[1i8], [2i8]] {
    case [[1i8], [2i8], ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread5(): Bool = match [1i8, 2i8, 3i8, 4i8] {
    case [1i8, 2i8, ..xs] => true
    case _ => false
}

@test
def matchInt8ArrayTailSpread6(): Bool = match [1i8] {
    case [1i8, 2i8, ..xs] => false
    case _ => true
}

@test
def matchInt16ArrayTailSpread1(): Bool = match [1i16] {
    case [1i16, ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread2(): Bool = match [1i16] {
    case [2i16, ..xs] => false
    case _ => true
}

@test
def matchInt16ArrayTailSpread3(): Bool = match [1i16, 2i16] {
    case [1i16, 2i16, ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread4(): Bool = match [[1i16], [2i16]] {
    case [[1i16], [2i16], ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread5(): Bool = match [1i16, 2i16, 3i16, 4i16] {
    case [1i16, 2i16, ..xs] => true
    case _ => false
}

@test
def matchInt16ArrayTailSpread6(): Bool = match [1i16] {
    case [1i16, 2i16, ..xs] => false
    case _ => true
}

@test
def matchInt64ArrayTailSpread1(): Bool = match [1i64] {
    case [1i64, ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread2(): Bool = match [1i64] {
    case [2i64, ..xs] => false
    case _ => true
}

@test
def matchInt64ArrayTailSpread3(): Bool = match [1i64, 2i64] {
    case [1i64, 2i64, ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread4(): Bool = match [[1i64], [2i64]] {
    case [[1i64], [2i64], ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread5(): Bool = match [1i64, 2i64, 3i64, 4i64] {
    case [1i64, 2i64, ..xs] => true
    case _ => false
}

@test
def matchInt64ArrayTailSpread6(): Bool = match [1i64] {
    case [1i64, 2i64, ..xs] => false
    case _ => true
}

@test
def matchBigIntArrayTailSpread1(): Bool = match [1i8] {
    case [1i8, ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread2(): Bool = match [1ii] {
    case [2ii, ..xs] => false
    case _ => true
}

@test
def matchBigIntArrayTailSpread3(): Bool = match [1ii, 2ii] {
    case [1ii, 2ii, ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread4(): Bool = match [[1ii], [2ii]] {
    case [[1ii], [2ii], ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread5(): Bool = match [1ii, 2ii, 3ii, 4ii] {
    case [1ii, 2ii, ..xs] => true
    case _ => false
}

@test
def matchBigIntArrayTailSpread6(): Bool = match [1ii, 2ii] {
    case [1ii, ..xs] => false
    case _ => true
}

@test
def matchStrArrayTailSpread1(): Bool = match ["one"] {
    case ["one", ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread2(): Bool = match ["one"] {
    case ["one ", ..xs] => false
    case _ => true
}

@test
def matchStrArrayTailSpread3(): Bool = match ["one", "two"] {
    case ["one",  "two", ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread4(): Bool = match [["one"], ["two"]] {
    case [["one"],  ["two"], ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread5(): Bool = match ["one", "two", "three", "four"] {
    case ["one",  "two", ..xs] => true
    case _ => false
}

@test
def matchStrArrayTailSpread6(): Bool = match ["one", "two"] {
    case ["one", ..xs] => false
    case _ => true
}

@test
def matchBoolArrayTailSpread1(): Bool = match [true] {
    case [true, ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread2(): Bool = match [true] {
    case [false, ..xs] => false
    case _ => true
}

@test
def matchBoolArrayTailSpread3(): Bool = match [true, false] {
    case [true, false, ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread4(): Bool = match [[false], [false]] {
    case [[false], [false], ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread5(): Bool = match [true, false, false, true] {
    case [true, false, ..xs] => true
    case _ => false
}

@test
def matchBoolArrayTailSpread6(): Bool = match [true] {
    case [true, false, ..xs] => false
    case _ => true
}

@test
def matchTupleArrayTailSpread1(): Bool = match [(1, "one")] {
    case [(1, "one"), ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread2(): Bool = match [(1, "one")] {
    case [(1, "one "), ..xs] => false
    case _ => true
}

@test
def matchTupleArrayTailSpread3(): Bool = match [(1, "one")] {
    case [(2, "one"), ..xs] => false
    case _ => true
}

@test
def matchTupleArrayTailSpread4(): Bool = match [(1, "one"), (2, "two")] {
    case [(1, "one"), (2, "two"), ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread5(): Bool = match [[(1, "one")], [(2, "two")]] {
    case [[(1, "one")], [(2, "two")], ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread6(): Bool = match [(1, "one"), (2, "two"), (3, "three"), (4, "four")] {
    case [(1, "one"), (2, "two"), ..xs] => true
    case _ => false
}

@test
def matchTupleArrayTailSpread7(): Bool = match [(1, "one")] {
    case [(1, "one"), (2, "two"), ..xs] => false
    case _ => true
}

@test
def matchFloatArrayTailSpread1(): Bool = match [1.1] {
    case [1.1, ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread2(): Bool = match [1.1] {
    case [1.2, ..xs] => false
    case _ => true
}

@test
def matchFloatArrayTailSpread3(): Bool = match [1.1, 2.2] {
    case [1.1, 2.2, ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread4(): Bool = match [[1.1], [2.2]] {
    case [[1.1], [2.2], ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread5(): Bool = match [1.1, 2.2, 3.3, 4.4] {
    case [1.1, 2.2, ..xs] => true
    case _ => false
}

@test
def matchFloatArrayTailSpread6(): Bool = match [1.1] {
    case [1.1, 2.2, ..xs] => false
    case _ => true
}

@test
def matchFloat32ArrayTailSpread1(): Bool = match [1.1f32] {
    case [1.1f32, ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread2(): Bool = match [1.1f32] {
    case [1.0f32, ..xs] => false
    case _ => true
}

@test
def matchFloat32ArrayTailSpread3(): Bool = match [1.1f32, 2.2f32] {
    case [1.1f32, 2.2f32, ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread4(): Bool = match [[1.1f32], [2.2f32]] {
    case [[1.1f32], [2.2f32], ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread5(): Bool = match [1.1f32, 2.2f32, 3.3f32, 4.4f32] {
    case [1.1f32, 2.2f32, ..xs] => true
    case _ => false
}

@test
def matchFloat32ArrayTailSpread6(): Bool = match [1.1f32] {
    case [1.1f32, 2.2f32, ..xs] => false
    case _ => true
}

@test
def matchCharArrayTailSpread1(): Bool = match ['1'] {
    case ['1', ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread2(): Bool = match ['1'] {
    case ['2', ..xs] => false
    case _ => true
}

@test
def matchCharArrayTailSpread3(): Bool = match ['1', '2'] {
    case ['1', '2', ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread4(): Bool = match [['1'], ['2']] {
    case [['1'], ['2'], ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread5(): Bool = match ['1', '2', '3', '4'] {
    case ['1', '2', ..xs] => true
    case _ => false
}

@test
def matchCharArrayTailSpread6(): Bool = match ['1'] {
    case ['1', '2', ..xs] => false
    case _ => true
}

@test
def matchOptionArrayTailSpread1(): Bool = match [Some(1)] {
    case [Some(1), ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread2(): Bool = match [Some(1)] {
    case [None, ..xs] => false
    case _ => true
}

@test
def matchOptionArrayTailSpread3(): Bool = match [Some(1), Some(2)] {
    case [Some(1), Some(2), ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread4(): Bool = match [[Some(1)], [Some(2)]] {
    case [[Some(1)], [Some(2)], ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread5(): Bool = match [Some(1), Some(2), None(), Some(42)] {
    case [Some(1), Some(2), ..xs] => true
    case _ => false
}

@test
def matchOptionArrayTailSpread6(): Bool = match [Some(1)] {
    case [Some(1), Some(2), ..xs] => false
    case _ => true
}

@test
def matchResultArrayTailSpread1(): Bool = match [Ok(42)] {
    case [Ok(42), ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread2(): Bool = match [Ok(42)] {
    case [Err(23) , ..xs] => false
    case _ => true
}

@test
def matchResultArrayTailSpread3(): Bool = match [Ok(42), Err(23)] {
    case [Ok(42), Err(23), ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread4(): Bool = match [[Ok(42)], [Err(23)]] {
    case [[Ok(42)], [Err(23)], ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread5(): Bool = match [Ok(42), Err(23), Ok(1), Err(2)] {
    case [Ok(42), Err(23), ..xs]  => true
    case _ => false
}

@test
def matchResultArrayTailSpread6(): Bool = match [Ok(42)] {
    case [Ok(42), Err(23), ..xs]  => false
    case _ => true
}
