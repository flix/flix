
@test
def matchEmptyIntArray(): Bool & Impure = match ([]:Array[Int]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyUnitArray(): Bool & Impure = match ([]:Array[()]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyCharArray(): Bool & Impure = match ([]:Array[Char]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyBoolArray(): Bool & Impure = match ([]:Array[Bool]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyFloat32Array(): Bool & Impure = match ([]:Array[Float32]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyFloat64Array(): Bool & Impure = match ([]:Array[Float64]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyInt8Array(): Bool & Impure = match ([]:Array[Int8]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyInt16Array(): Bool & Impure = match ([]:Array[Int16]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyInt64Array(): Bool & Impure = match ([]:Array[Int64]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyStrArray(): Bool & Impure = match ([]:Array[String]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyBigIntArray(): Bool & Impure = match ([]:Array[BigInt]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyOptionIntArray(): Bool & Impure = match ([]:Array[Option[Int]]) {
    case [] => true
    case _ => false
}

@test
def matchEmptyResultArray(): Bool & Impure = match ([]:Array[Result[Int,String]]) {
    case [] => true
    case _ => false
}

@test
def matchUnitArray1(): Bool & Impure = match [()] {
    case [()] => true
    case _ => false
}

@test
def matchUnitArray2(): Bool & Impure = match [(),()] {
    case [(),()] => true
    case _ => false
}

@test
def matchUnitArray3(): Bool & Impure = match [[()],[()]] {
    case [[()],[()]] => true
    case _ => false
}

@test
def matchIntArray1(): Bool & Impure = match [1] {
    case [1] => true
    case _ => false
}

@test
def matchIntArray2(): Bool & Impure = match [1,2] {
    case [1] => false
    case _ => true
}

@test
def matchIntArray3(): Bool & Impure = match [1,2] {
    case [1,2] => true
    case _ => false
}

@test
def matchIntArray4(): Bool & Impure = match [1,2] {
    case [1,_] => true
    case _ => false
}

@test
def matchIntArray5(): Bool & Impure = match [2,1] {
    case [1,2] => false
    case _ => true
}

@test
def matchIntArray6(): Bool & Impure = match [[1],[2]] {
    case [[1],[2]] => true
    case _ => false
}

@test
def matchIntArray7(): Bool & Impure = match [[1],[2]] {
    case [[1]] => false
    case _ => true
}

@test
def matchIntArray8(): Bool & Impure = match [[1,2,3],[1,2,3]] {
    case [[1,2,3],[1,2,3]] => true
    case _ => false
}

@test
def matchIntArray9(): Bool & Impure = match [[[1]],[[2]]] {
    case [[[1]],[[2]]] => true
    case _ => false
}

@test
def matchInt8Array1(): Bool & Impure = match [1i8] {
    case [1i8] => true
    case _ => false
}

@test
def matchInt8Array2(): Bool & Impure = match [1i8] {
    case [2i8] => false
    case _ => true
}

@test
def matchInt8Array3(): Bool & Impure = match [1i8,2i8] {
    case [1i8,2i8] => true
    case _ => false
}

@test
def matchInt8Array4(): Bool & Impure = match [[1i8],[2i8]] {
    case [[1i8],[2i8]] => true
    case _ => false
}

@test
def matchInt16Array1(): Bool & Impure = match [1i16] {
    case [1i16] => true
    case _ => false
}

@test
def matchInt16Array2(): Bool & Impure = match [1i16] {
    case [2i16] => false
    case _ => true
}

@test
def matchInt16Array3(): Bool & Impure = match [1i16,2i16] {
    case [1i16,2i16] => true
    case _ => false
}

@test
def matchInt16Array4(): Bool & Impure = match [[1i16],[2i16]] {
    case [[1i16],[2i16]] => true
    case _ => false
}

@test
def matchInt64Array1(): Bool & Impure = match [1i64] {
    case [1i64] => true
    case _ => false
}

@test
def matchInt64Array2(): Bool & Impure = match [1i64] {
    case [2i64] => false
    case _ => true
}

@test
def matchInt64Array3(): Bool & Impure = match [1i64,2i64] {
    case [1i64,2i64] => true
    case _ => false
}

@test
def matchInt64Array4(): Bool & Impure = match [[1i64],[2i64]] {
    case [[1i64],[2i64]] => true
    case _ => false
}

@test
def matchBigIntArray1(): Bool & Impure = match [1i8] {
    case [1i8] => true
    case _ => false
}

@test
def matchBigIntArray2(): Bool & Impure = match [1ii] {
    case [2ii] => false
    case _ => true
}

@test
def matchBigIntArray3(): Bool & Impure = match [1ii,2ii] {
    case [1ii,2ii] => true
    case _ => false
}

@test
def matchBigIntArray4(): Bool & Impure = match [[1ii],[2ii]] {
    case [[1ii],[2ii]] => true
    case _ => false
}

@test
def matchStrArray1(): Bool & Impure = match ["one"] {
    case ["one"] => true
    case _ => false
}

@test
def matchStrArray2(): Bool & Impure = match ["one"] {
    case ["one "] => false
    case _ => true
}

@test
def matchStrArray3(): Bool & Impure = match ["one","two"] {
    case ["one", "two"] => true
    case _ => false
}

@test
def matchStrArray4(): Bool & Impure = match [["one"],["two"]] {
    case [["one"], ["two"]] => true
    case _ => false
}

@test
def matchBoolArray1(): Bool & Impure = match [true] {
    case [true] => true
    case _ => false
}

@test
def matchBoolArray2(): Bool & Impure = match [true] {
    case [false] => false
    case _ => true
}

@test
def matchBoolArray3(): Bool & Impure = match [true,false] {
    case [true,false] => true
    case _ => true
}

@test
def matchBoolArray4(): Bool & Impure = match [[false],[false]] {
    case [[false],[false]] => true
    case _ => true
}

@test
def matchTupleArray1(): Bool & Impure = match [(1,"one")] {
    case [(1,"one")] => true
    case _ => false
}

@test
def matchTupleArray2(): Bool & Impure = match [(1,"one")] {
    case [(1,"one ")] => false
    case _ => true
}

@test
def matchTupleArray3(): Bool & Impure = match [(1,"one")] {
    case [(2,"one")] => false
    case _ => true
}

@test
def matchTupleArray4(): Bool & Impure = match [(1,"one"),(2,"two")] {
    case [(1,"one"),(2,"two")] => true
    case _ => true
}

@test
def matchTupleArray5(): Bool & Impure = match [[(1,"one")],[(2,"two")]] {
    case [[(1,"one")],[(2,"two")]] => true
    case _ => true
}

@test
def matchFloatArray1(): Bool & Impure = match [1.1] {
    case [1.1] => true
    case _ => false
}

@test
def matchFloatArray2(): Bool & Impure = match [1.1] {
    case [1.2] => false
    case _ => true
}

@test
def matchFloatArray3(): Bool & Impure = match [1.1,2.2] {
    case [1.1,2.2] => true
    case _ => false
}

@test
def matchFloatArray4(): Bool & Impure = match [[1.1],[2.2]] {
    case [[1.1],[2.2]] => true
    case _ => false
}

@test
def matchFloat32Array1(): Bool & Impure = match [1.1f32] {
    case [1.1f32] => true
    case _ => false
}

@test
def matchFloat32Array2(): Bool & Impure = match [1.1f32] {
    case [1.0f32] => false
    case _ => true
}

@test
def matchFloat32Array3(): Bool & Impure = match [1.1f32,2.2f32] {
    case [1.1f32,2.2f32] => true
    case _ => false
}

@test
def matchFloat32Array4(): Bool & Impure = match [[1.1f32],[2.2f32]] {
    case [[1.1f32],[2.2f32]] => true
    case _ => false
}

@test
def matchCharArray1(): Bool & Impure = match ['1'] {
    case ['1'] => true
    case _ => false
}

@test
def matchCharArray2(): Bool & Impure = match ['1'] {
    case ['2'] => false
    case _ => true
}

@test
def matchCharArray3(): Bool & Impure = match ['1','2'] {
    case ['1','2'] => true
    case _ => false
}

@test
def matchCharArray4(): Bool & Impure = match [['1'],['2']] {
    case [['1'],['2']] => true
    case _ => false
}

@test
def matchOptionArray1(): Bool & Impure = match [Some(1)] {
    case [Some(1)] => true
    case _ => false
}

@test
def matchOptionArray2(): Bool & Impure = match [Some(1)] {
    case [None] => false
    case _ => true
}

@test
def matchOptionArray3(): Bool & Impure = match [Some(1),Some(2)] {
    case [Some(1),Some(2)] => true
    case _ => false
}

@test
def matchOptionArray4(): Bool & Impure = match [[Some(1)],[Some(2)]] {
    case [[Some(1)],[Some(2)]] => true
    case _ => false
}

@test
def matchResultArray1(): Bool & Impure = match [Ok(42)] {
    case [Ok(42)]  => true
    case _ => false
}

@test
def matchResultArray2(): Bool & Impure = match [Ok(42)] {
    case [Err(23) ] => false
    case _ => true
}

@test
def matchResultArray3(): Bool & Impure = match [Ok(42),Err(23)] {
    case [Ok(42),Err(23)]  => true
    case _ => false
}

@test
def matchResultArray4(): Bool & Impure = match [[Ok(42)],[Err(23)]] {
    case [[Ok(42)],[Err(23)]]  => true
    case _ => false
}
