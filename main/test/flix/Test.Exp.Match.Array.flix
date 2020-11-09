namespace Test/Exp/Match/Array {

    @test
    def testMatchEmptyUnitArray01(): Bool & Impure = match ([]: Array[Unit]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyBoolArray01(): Bool & Impure = match ([]: Array[Bool]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyCharArray01(): Bool & Impure = match ([]: Array[Char]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyFloat32Array01(): Bool & Impure = match ([]: Array[Float32]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyFloat64Array01(): Bool & Impure = match ([]: Array[Float64]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyInt8Array01(): Bool & Impure = match ([]: Array[Int8]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyInt16Array01(): Bool & Impure = match ([]: Array[Int16]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyInt64Array01(): Bool & Impure = match ([]: Array[Int64]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyStringArray01(): Bool & Impure = match ([]: Array[String]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyBigIntArray01(): Bool & Impure = match ([]: Array[BigInt]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyOptionIntArray01(): Bool & Impure = match ([]: Array[Option[Int]]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchEmptyResultArray01(): Bool & Impure = match ([]: Array[Result[Int,String]]) {
        case [] => true
        case _  => false
    }

    @test
    def testMatchUnitArray01(): Bool & Impure = match [()] {
        case [()] => true
        case _    => false
    }

    @test
    def testMatchUnitArray02(): Bool & Impure = match [(), ()] {
        case [(), ()] => true
        case _        => false
    }

    @test
    def testMatchUnitArray03(): Bool & Impure = match [[()], [()]] {
        case [[()], [()]] => true
        case _            => false
    }

    @test
    def testMatchInt8Array1(): Bool & Impure = match [1i8] {
        case [1i8] => true
        case _ => false
    }

    @test
    def testMatchInt8Array2(): Bool & Impure = match [1i8] {
        case [2i8] => false
        case _ => true
    }

    @test
    def testMatchInt8Array3(): Bool & Impure = match [1i8,2i8] {
        case [1i8,2i8] => true
        case _ => false
    }

    @test
    def testMatchInt8Array4(): Bool & Impure = match [[1i8],[2i8]] {
        case [[1i8],[2i8]] => true
        case _ => false
    }

    @test
    def testMatchInt16Array1(): Bool & Impure = match [1i16] {
        case [1i16] => true
        case _ => false
    }

    @test
    def testMatchInt16Array2(): Bool & Impure = match [1i16] {
        case [2i16] => false
        case _ => true
    }

    @test
    def testMatchInt16Array3(): Bool & Impure = match [1i16,2i16] {
        case [1i16,2i16] => true
        case _ => false
    }

    @test
    def testMatchInt16Array4(): Bool & Impure = match [[1i16],[2i16]] {
        case [[1i16],[2i16]] => true
        case _ => false
    }


    @test
    def testMatchInt32Array1(): Bool & Impure = match [1] {
        case [1] => true
        case _ => false
    }

    @test
    def testMatchInt32Array2(): Bool & Impure = match [1,2] {
        case [1] => false
        case _ => true
    }

    @test
    def testMatchInt32Array3(): Bool & Impure = match [1,2] {
        case [1,2] => true
        case _ => false
    }

    @test
    def testMatchInt32Array4(): Bool & Impure = match [1,2] {
        case [1,_] => true
        case _ => false
    }

    @test
    def testMatchInt32Array5(): Bool & Impure = match [2,1] {
        case [1,2] => false
        case _ => true
    }

    @test
    def testMatchInt32Array6(): Bool & Impure = match [[1],[2]] {
        case [[1],[2]] => true
        case _ => false
    }

    @test
    def testMatchInt32Array7(): Bool & Impure = match [[1],[2]] {
        case [[1]] => false
        case _ => true
    }

    @test
    def testMatchInt32Array8(): Bool & Impure = match [[1,2,3],[1,2,3]] {
        case [[1,2,3],[1,2,3]] => true
        case _ => false
    }

    @test
    def testMatchInt32Array9(): Bool & Impure = match [[[1]],[[2]]] {
        case [[[1]],[[2]]] => true
        case _ => false
    }

    @test
    def testMatchInt64Array1(): Bool & Impure = match [1i64] {
        case [1i64] => true
        case _ => false
    }

    @test
    def testMatchInt64Array2(): Bool & Impure = match [1i64] {
        case [2i64] => false
        case _ => true
    }

    @test
    def testMatchInt64Array3(): Bool & Impure = match [1i64,2i64] {
        case [1i64,2i64] => true
        case _ => false
    }

    @test
    def testMatchInt64Array4(): Bool & Impure = match [[1i64],[2i64]] {
        case [[1i64],[2i64]] => true
        case _ => false
    }

    @test
    def testMatchBigIntArray1(): Bool & Impure = match [1i8] {
        case [1i8] => true
        case _ => false
    }

    @test
    def testMatchBigIntArray2(): Bool & Impure = match [1ii] {
        case [2ii] => false
        case _ => true
    }

    @test
    def testMatchBigIntArray3(): Bool & Impure = match [1ii,2ii] {
        case [1ii,2ii] => true
        case _ => false
    }

    @test
    def testMatchBigIntArray4(): Bool & Impure = match [[1ii],[2ii]] {
        case [[1ii],[2ii]] => true
        case _ => false
    }

    @test
    def testMatchStrArray1(): Bool & Impure = match ["one"] {
        case ["one"] => true
        case _ => false
    }

    @test
    def testMatchStrArray2(): Bool & Impure = match ["one"] {
        case ["one "] => false
        case _ => true
    }

    @test
    def testMatchStrArray3(): Bool & Impure = match ["one","two"] {
        case ["one", "two"] => true
        case _ => false
    }

    @test
    def testMatchStrArray4(): Bool & Impure = match [["one"],["two"]] {
        case [["one"], ["two"]] => true
        case _ => false
    }

    @test
    def testMatchBoolArray1(): Bool & Impure = match [true] {
        case [true] => true
        case _ => false
    }

    @test
    def testMatchBoolArray2(): Bool & Impure = match [true] {
        case [false] => false
        case _ => true
    }

    @test
    def testMatchBoolArray3(): Bool & Impure = match [true,false] {
        case [true,false] => true
        case _ => true
    }

    @test
    def testMatchBoolArray4(): Bool & Impure = match [[false],[false]] {
        case [[false],[false]] => true
        case _ => true
    }

    @test
    def testMatchTupleArray1(): Bool & Impure = match [(1,"one")] {
        case [(1,"one")] => true
        case _ => false
    }

    @test
    def testMatchTupleArray2(): Bool & Impure = match [(1,"one")] {
        case [(1,"one ")] => false
        case _ => true
    }

    @test
    def testMatchTupleArray3(): Bool & Impure = match [(1,"one")] {
        case [(2,"one")] => false
        case _ => true
    }

    @test
    def testMatchTupleArray4(): Bool & Impure = match [(1,"one"),(2,"two")] {
        case [(1,"one"),(2,"two")] => true
        case _ => true
    }

    @test
    def testMatchTupleArray5(): Bool & Impure = match [[(1,"one")],[(2,"two")]] {
        case [[(1,"one")],[(2,"two")]] => true
        case _ => true
    }

    @test
    def testMatchFloatArray1(): Bool & Impure = match [1.1] {
        case [1.1] => true
        case _ => false
    }

    @test
    def testMatchFloatArray2(): Bool & Impure = match [1.1] {
        case [1.2] => false
        case _ => true
    }

    @test
    def testMatchFloatArray3(): Bool & Impure = match [1.1,2.2] {
        case [1.1,2.2] => true
        case _ => false
    }

    @test
    def testMatchFloatArray4(): Bool & Impure = match [[1.1],[2.2]] {
        case [[1.1],[2.2]] => true
        case _ => false
    }

    @test
    def testMatchFloat32Array1(): Bool & Impure = match [1.1f32] {
        case [1.1f32] => true
        case _ => false
    }

    @test
    def testMatchFloat32Array2(): Bool & Impure = match [1.1f32] {
        case [1.0f32] => false
        case _ => true
    }

    @test
    def testMatchFloat32Array3(): Bool & Impure = match [1.1f32,2.2f32] {
        case [1.1f32,2.2f32] => true
        case _ => false
    }

    @test
    def testMatchFloat32Array4(): Bool & Impure = match [[1.1f32],[2.2f32]] {
        case [[1.1f32],[2.2f32]] => true
        case _ => false
    }

    @test
    def testMatchCharArray1(): Bool & Impure = match ['1'] {
        case ['1'] => true
        case _ => false
    }

    @test
    def testMatchCharArray2(): Bool & Impure = match ['1'] {
        case ['2'] => false
        case _ => true
    }

    @test
    def testMatchCharArray3(): Bool & Impure = match ['1','2'] {
        case ['1','2'] => true
        case _ => false
    }

    @test
    def testMatchCharArray4(): Bool & Impure = match [['1'],['2']] {
        case [['1'],['2']] => true
        case _ => false
    }

    @test
    def testMatchOptionArray1(): Bool & Impure = match [Some(1)] {
        case [Some(1)] => true
        case _ => false
    }

    @test
    def testMatchOptionArray2(): Bool & Impure = match [Some(1)] {
        case [None] => false
        case _ => true
    }

    @test
    def testMatchOptionArray3(): Bool & Impure = match [Some(1),Some(2)] {
        case [Some(1),Some(2)] => true
        case _ => false
    }

    @test
    def testMatchOptionArray4(): Bool & Impure = match [[Some(1)],[Some(2)]] {
        case [[Some(1)],[Some(2)]] => true
        case _ => false
    }

    @test
    def testMatchResultArray1(): Bool & Impure = match [Ok(42)] {
        case [Ok(42)]  => true
        case _ => false
    }

    @test
    def testMatchResultArray2(): Bool & Impure = match [Ok(42)] {
        case [Err(23) ] => false
        case _ => true
    }

    @test
    def testMatchResultArray3(): Bool & Impure = match [Ok(42),Err(23)] {
        case [Ok(42),Err(23)]  => true
        case _ => false
    }

    @test
    def testMatchResultArray4(): Bool & Impure = match [[Ok(42)],[Err(23)]] {
        case [[Ok(42)],[Err(23)]]  => true
        case _ => false
    }

    enum Option[t] {
        case None,
        case Some(t)
    }

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

}
